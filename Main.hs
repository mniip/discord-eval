{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Main where

import Control.Exception (evaluate)
import Control.Concurrent
import Control.Concurrent.STM
import Control.Lens
import Control.Monad
import Control.Monad.Catch
import Control.Monad.IO.Class
import Control.Monad.State.Class
import Control.Monad.Trans.Reader
import Data.Aeson
import Data.Char
import Data.List
import Data.Map (Map)
import qualified Data.Map as M
import Data.Ord
import Data.Set (Set)
import qualified Data.Set as S
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Data.Time.Clock
import Data.Time.Clock.POSIX
import Data.Tuple
import GHC.Stack
import System.IO
import System.Process

import Discord

getAuth :: IO Auth
getAuth = Auth <$> read <$> readFile "auth.conf"
login = loginRest =<< getAuth

prune_messages_after_seconds = 10 * 60
max_blocks_per_msg = 10
max_chars_per_msg = 2000
react_wait = "\x231B"
react_check = "\x2705"
sandbox_cmd = "cat"
max_output = 1000

type BotHandle = (RestChan, Gateway, [ThreadIdType])
data Command = Reset | EvalLine Text | EvalBlock Text deriving (Eq, Show)
data ParsedReq = ParsedReq
  { _reqCommands :: [Command]
  , _reqMode :: Text
  } deriving (Eq, Show)
makeLenses ''ParsedReq

data EvalState = EvalState
  { _botHandle :: MVar BotHandle
  , _recentMsgs :: Map (Snowflake, Snowflake) (UTCTime, ParsedReq, Maybe Snowflake)
  , _channel :: TChan (Snowflake, Snowflake)
  , _processing :: TVar (S.Set (Snowflake, Snowflake))
  }
makeLenses ''EvalState

newtype EvalM a = EvalM { runEvalM :: ReaderT (MVar EvalState) IO a }
  deriving (Functor, Applicative, Monad, MonadIO, MonadThrow, MonadCatch, MonadMask)

instance MonadState EvalState EvalM where
  get = EvalM $ ReaderT readMVar
  state f = EvalM $ ReaderT (`modifyMVar` forcing . f)
    where forcing (b, a) = do a' <- evaluate a
                              pure (a', b)

withBot :: (BotHandle -> EvalM a) -> EvalM a
withBot = bracket (use botHandle >>= \h -> liftIO $ takeMVar h)
                  (\bot -> use botHandle >>= \h -> liftIO $ putMVar h bot)

logM :: String -> EvalM ()
logM s = liftIO $ do time <- getCurrentTime
                     putStrLn $ show time ++ ": " ++ s

logShowM :: Show a => a -> EvalM ()
logShowM = logM . show

traceM :: HasCallStack => EvalM ()
traceM = logShowM callStack

instance Exception RestCallException

sendEither :: (FromJSON a, Request (r a)) => r a -> EvalM (Either RestCallException a)
sendEither req = withBot (\bot -> liftIO $ restCall bot req)

sendTrace :: HasCallStack => (FromJSON a, Request (r a)) => r a -> EvalM ()
sendTrace req = do resp <- sendEither req
                   case resp of
                     Left err -> do logM "Error while calling REST:"
                                    logShowM err
                                    traceM
                     Right _ -> pure ()

send :: HasCallStack => (FromJSON a, Request (r a)) => r a -> EvalM a
send req = do resp <- sendEither req
              case resp of
                Left err -> throwM err
                Right x -> pure x

main = do h <- newEmptyMVar
          c <- newTChanIO
          p <- newTVarIO S.empty
          env <- newMVar $ EvalState { _botHandle = h
                                     , _recentMsgs = M.empty
                                     , _channel = c
                                     , _processing = p
                                     }
          forkIO $ runReaderT (runEvalM backendLoop) env
          runReaderT (runEvalM mainLoop) env
  where mainLoop = forever $ catch (do bot <- liftIO $ loginRestGateway =<< getAuth
                                       finally (do use botHandle >>= \h -> liftIO $ putMVar h bot
                                                   logM "Connected"
                                                   eventLoop
                                                   use botHandle >>= \h -> liftIO $ takeMVar h)
                                               (do logM "Disconnected"
                                                   liftIO $ stopDiscord bot)
                                       pure ())
                                   (\e -> do logM "Exception in mainLoop:"
                                             logShowM (e :: SomeException)
                                             liftIO $ threadDelay $ 10 * 1000000)
        eventLoop = do bot <- use botHandle >>= liftIO . readMVar
                       everr <- liftIO $ nextEvent bot
                       case everr of
                         Right event -> do catch (handleEvent event)
                                                 (\e -> do logM "Exception in eventLoop:"
                                                           logShowM (e :: SomeException))
                                           eventLoop
                         Left err -> do logM "Exception in nextEvent:"
                                        logShowM err

parseMessage :: Text -> ParsedReq
parseMessage t | Just ('>', cmd) <- T.uncons t
               , (mode, rest) <- T.span isLetter cmd = ParsedReq { _reqMode = mode, _reqCommands = take max_blocks_per_msg $ go rest }
  where go t | Just next <- T.stripPrefix "!reset" t
             = Reset : go next
             | Just rest <- T.stripPrefix "```" t
             , "```" `T.isInfixOf` rest
             , (inside, next) <- T.breakOn "```" rest
             = EvalBlock (T.copy $ dropLanguageTag inside) : go (T.drop 3 next)
             | Just rest <- T.stripPrefix "``" t
             , "``" `T.isInfixOf` rest
             , (inside, next) <- T.breakOn "``" rest
             = EvalLine (T.map stripControl inside) : go (T.drop 2 next)
             | Just rest <- T.stripPrefix "`" t
             , "`" `T.isInfixOf` rest
             , (inside, next) <- T.breakOn "`" rest
             = EvalLine (T.map stripControl inside) : go (T.drop 1 next)
             | T.null t = []
             | otherwise = go (T.tail t)
        dropLanguageTag t | (first, rest) <- T.break (== '\n') t
                          , not (T.any isSpace first)
                          , not (T.null rest) = rest
                          | otherwise = t
        stripControl c | isControl c = ' '
                       | otherwise = c
parseMessage _ = ParsedReq { _reqCommands = [], _reqMode = "" }

pruneMessages :: EvalM ()
pruneMessages = do time <- liftIO getCurrentTime
                   modifying recentMsgs (M.filter (\(ts, _, _) -> diffUTCTime time ts < fromInteger prune_messages_after_seconds))

enqueue :: (Snowflake, Snowflake) -> EvalM Bool
enqueue s = do q <- use channel
               p <- use processing
               liftIO $ atomically $ do
                 set <- readTVar p
                 unless (S.member s set) $ do writeTVar p (S.insert s set)
                                              writeTChan q s
                 pure (S.null set)

dequeue :: EvalM (Snowflake, Snowflake)
dequeue = do q <- use channel
             p <- use processing
             liftIO $ atomically $ do
               s <- readTChan q
               modifyTVar' p (S.delete s)
               pure s

handleEvent :: HasCallStack => Event -> EvalM ()
handleEvent (MessageCreate Message{..}) = do
  pruneMessages
  time <- liftIO getCurrentTime
  let req = parseMessage messageText
  assign (recentMsgs . at (messageChannel, messageId)) $ Just (time, req, Nothing)
  unless (null $ view reqCommands req) $ do
    empty <- enqueue (messageChannel, messageId)
    unless empty $ do
      sendTrace $ CreateReaction (messageChannel, messageId) react_wait
handleEvent (MessageUpdate Message{..}) = do
  pruneMessages
  x <- use (recentMsgs . at (messageChannel, messageId))
  case x of
    Just (ts, oldreq, reply) | req <- parseMessage messageText
                             , oldreq /= req
                             -> if null $ view reqCommands req
                                then case reply of
                                  Just id -> do sendTrace $ DeleteMessage (messageChannel, id)
                                                assign (recentMsgs . at (messageChannel, messageId) . _Just . _3) Nothing
                                  _ -> pure ()
                                else do assign (recentMsgs . at (messageChannel, messageId)) $ Just (ts, req, reply)
                                        empty <- enqueue (messageChannel, messageId)
                                        unless empty $ do
                                          sendTrace $ CreateReaction (messageChannel, messageId) react_wait
    _ -> pure ()
handleEvent (MessageDelete messageChannel messageId) = do
  pruneMessages
  x <- use (recentMsgs . at (messageChannel, messageId))
  case x of
    Just (ts, oldreq, Just id) -> sendTrace $ DeleteMessage (messageChannel, id)
    _ -> pure ()
handleEvent _ = pure ()

backendLoop :: EvalM ()
backendLoop = forever $ catch (do (chan, msg) <- dequeue
                                  sendTrace $ DeleteOwnReaction (chan, msg) react_wait
                                  x <- use (recentMsgs . at (chan, msg))
                                  case x of
                                    Just (_, ParsedReq{..}, reply) -> do
                                      outs <- forM _reqCommands $ \cmd -> do
                                        sendTrace $ TriggerTypingIndicator chan
                                        case cmd of
                                          Reset -> do resetMode _reqMode
                                                      pure ""
                                          EvalLine ln -> evalLine _reqMode ln
                                          EvalBlock blk -> evalBlock _reqMode blk
                                      let res = formatResults outs
                                      case reply of
                                        Just id -> sendTrace $ EditMessage (chan, id) res Nothing
                                        _ -> do Message{..} <- send $ CreateMessage chan res
                                                assign (recentMsgs . at (chan, msg) . _Just . _3) (Just messageId)
                                    Nothing -> pure ())
                              (\e -> do logM "Exception in backendLoop:"
                                        logShowM (e :: SomeException))

formatResults :: [Text] -> Text
formatResults res = if T.null msg then react_check else msg
  where
    nonempty = zip [0..] $ filter (not . T.null) $ map (T.replace "``" "``\x200D") res
    
    sorted = sortBy (comparing $ T.length . snd) nonempty
    
    accumulate :: Int -> S.Set Int -> [(Int, Text)] -> S.Set Int
    accumulate n s [] = s
    accumulate n s ((i, x):xs)
      | n + 8 + T.length x < max_chars_per_msg - 10 * length xs
      = accumulate (n + 8 + T.length x) (S.insert i s) xs
      | otherwise = s
    
    small = accumulate 0 S.empty sorted

    format (i, x)
      | i `S.member` small = T.concat ["```\n", x, "```\n"]
      | otherwise = " ... "

    msg = T.concat $ map format nonempty

resetMode :: Text -> EvalM ()
resetMode _ = pure ()

evalLine :: Text -> Text -> EvalM Text
evalLine mode line = launchProcess line

evalBlock :: Text -> Text -> EvalM Text
evalBlock mode block = fmap (T.take max_output . T.concat) $ mapM launchProcess $ [":{"] ++ T.lines block ++ [":}"]

launchProcess :: HasCallStack => Text -> EvalM Text
launchProcess s = liftIO $ do
  T.writeFile "input" (T.filter (/= '\n') s `T.snoc` '\n')
  input <- openBinaryFile "input" ReadMode
  (outr, outw) <- createPipe
  (_, _, _, p) <- createProcess (shell sandbox_cmd) { std_in = UseHandle input
                                                    , std_out = UseHandle outw
                                                    , std_err = UseHandle outw
                                                    , close_fds = True
                                                    }
  finally (go "" outr)
          (do hClose outr
              waitForProcess p)
  where go rd hdl | T.length rd >= max_output = pure $ T.take max_output rd
                  | otherwise = do ch <- T.hGetChunk hdl
                                   if T.null ch
                                   then pure rd
                                   else go (T.append rd ch) hdl
