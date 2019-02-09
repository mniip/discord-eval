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

import DupQueue

getAuth :: IO Auth
getAuth = Auth <$> read <$> readFile "auth.conf"
login = loginRest =<< getAuth

prune_messages_after_seconds = 10 * 60
max_blocks_per_msg = 10
max_chars_per_msg = 2000
react_wait = "\x231B"
react_check = "\x2705"
max_output = 1000
sandbox_cmd = "cat"
sandbox_conf = "--"

type BotHandle = (RestChan, Gateway, [ThreadIdType])
data Mode = HaskellEval | Haskell | C | Shell deriving (Eq, Show)
data Command = Reset Mode | EvalLine Mode Text | EvalBlock Mode Text deriving (Eq, Show)
type ParsedReq = [Command]

data EvalState = EvalState
  { _botHandle :: MVar BotHandle
  , _recentMsgs :: Map (Snowflake, Snowflake) (UTCTime, ParsedReq, Maybe Snowflake)
  , _channel :: UChan (Snowflake, Snowflake)
  }
makeLenses ''EvalState

newtype EvalM a = EvalM { runEvalM :: ReaderT (MVar EvalState) IO a }
  deriving (Functor, Applicative, Monad, MonadIO, MonadThrow, MonadCatch, MonadMask)

instance MonadState EvalState EvalM where
  get = EvalM $ ReaderT readMVar
  state f = EvalM $ ReaderT (`modifyMVar` forcing . f)
    where forcing (b, a) = do a' <- evaluate a
                              pure (a', b)

logM :: String -> EvalM ()
logM s = liftIO $ do time <- getCurrentTime
                     putStrLn $ show time ++ ": " ++ s

logShowM :: Show a => a -> EvalM ()
logShowM = logM . show

traceM :: HasCallStack => EvalM ()
traceM = logShowM callStack

instance Exception RestCallException

sendEither :: (FromJSON a, Request (r a)) => r a -> EvalM (Either RestCallException a)
sendEither req = do bh <- use botHandle
                    bot <- liftIO (readMVar bh)
                    liftIO $ restCall bot req

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
          c <- newUChan
          env <- newMVar $ EvalState { _botHandle = h
                                     , _recentMsgs = M.empty
                                     , _channel = c
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

parseMode :: Text -> Mode
parseMode t = case T.toLower t of
  "" -> HaskellEval
  "hs" -> HaskellEval
  "haskell" -> HaskellEval
  "ghc" -> Haskell
  "c" -> C
  "gcc" -> C
  "!" -> Shell
  "sh" -> Shell
  "bash" -> Shell
  "shell" -> Shell
  _ -> HaskellEval

parseMessage :: Text -> ParsedReq
parseMessage t | Just ('>', cmd) <- T.uncons t
               , (m, rest) <- T.break (\x -> x == '`' || isSpace x) cmd = take max_blocks_per_msg $ go (parseMode m) rest
  where go mode t | Just next <- T.stripPrefix "!reset" t
                  = Reset mode : go mode next
                  | Just rest <- T.stripPrefix "```" t
                  , "```" `T.isInfixOf` rest
                  , (inside, next) <- T.breakOn "```" rest
                  , (tag, code) <- dropLanguageTag inside
                  = EvalBlock (maybe mode parseMode tag) (T.copy code) : go mode (T.drop 3 next)
                  | Just rest <- T.stripPrefix "``" t
                  , "``" `T.isInfixOf` rest
                  , (inside, next) <- T.breakOn "``" rest
                  = EvalLine mode (T.map stripControl inside) : go mode (T.drop 2 next)
                  | Just rest <- T.stripPrefix "`" t
                  , "`" `T.isInfixOf` rest
                  , (inside, next) <- T.breakOn "`" rest
                  = EvalLine mode (T.map stripControl inside) : go mode (T.drop 1 next)
                  | Just next <- T.stripPrefix "\\" t
                  = go mode (T.tail next)
                  | T.null t = []
                  | otherwise = go mode (T.tail t)
        dropLanguageTag t | (first, rest) <- T.break (== '\n') t
                          , not (T.any isSpace first)
                          , not (T.null rest) = (mfilter (not . T.null) $ Just first, rest)
                          | otherwise = (Nothing, t)
        stripControl c | isControl c = ' '
                       | otherwise = c
parseMessage _ = []

pruneMessages :: EvalM ()
pruneMessages = do time <- liftIO getCurrentTime
                   modifying recentMsgs (M.filter (\(ts, _, _) -> diffUTCTime time ts < fromInteger prune_messages_after_seconds))

handleEvent :: HasCallStack => Event -> EvalM ()
handleEvent (MessageCreate Message{..}) = do
  pruneMessages
  time <- liftIO getCurrentTime
  let req = parseMessage messageText
  assign (recentMsgs . at (messageChannel, messageId)) $ Just (time, req, Nothing)
  unless (null req) $ do
    queue <- use channel
    status <- liftIO $ writeUChan queue (messageChannel, messageId)
    when (status == Just False) $ do
      sendTrace $ CreateReaction (messageChannel, messageId) react_wait
handleEvent (MessageUpdate Message{..}) = do
  pruneMessages
  x <- use (recentMsgs . at (messageChannel, messageId))
  case x of
    Just (ts, oldreq, reply) | req <- parseMessage messageText
                             , oldreq /= req
                             -> if null req
                                then case reply of
                                  Just id -> do sendTrace $ DeleteMessage (messageChannel, id)
                                                assign (recentMsgs . at (messageChannel, messageId) . _Just . _3) Nothing
                                  _ -> pure ()
                                else do assign (recentMsgs . at (messageChannel, messageId)) $ Just (ts, req, reply)
                                        queue <- use channel
                                        status <- liftIO $ writeUChan queue (messageChannel, messageId)
                                        when (status == Just False) $ do
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
backendLoop = forever $ catch (do queue <- use channel
                                  (chan, msg) <- liftIO $ readUChan queue
                                  sendTrace $ DeleteOwnReaction (chan, msg) react_wait
                                  x <- use (recentMsgs . at (chan, msg))
                                  case x of
                                    Just (_, req, reply) -> do
                                      outs <- forM req $ \cmd -> do
                                        sendTrace $ TriggerTypingIndicator chan
                                        case cmd of
                                          Reset mode -> do resetMode mode
                                                           pure ""
                                          EvalLine mode ln -> evalLine mode ln
                                          EvalBlock mode blk -> evalBlock mode blk
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
    nonempty = zip [0..] $ filter (not . T.null) $ map (T.replace "``" "``\x200D" . T.filter (\x -> x == '\n' || not (isControl x))) res
    
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

resetMode :: Mode -> EvalM ()
resetMode HaskellEval = launchWithData (proc sandbox_cmd [sandbox_conf, "kill", "Dghci"]) "" >> pure ()
resetMode _ = pure ()

evalLine :: Mode -> Text -> EvalM Text
evalLine HaskellEval line = launchWithLine (proc sandbox_cmd [sandbox_conf, "Dghci"]) line
evalLine mode line = evalBlock mode line

evalBlock :: Mode -> Text -> EvalM Text
evalBlock HaskellEval block = fmap (T.take max_output . T.concat) $ mapM (evalLine HaskellEval) $ [":{"] ++ T.lines block ++ [":}"]
evalBlock C block = launchWithData (proc sandbox_cmd [sandbox_conf, "rungcc"]) block
evalBlock Shell block = launchWithData (proc sandbox_cmd [sandbox_conf, "bash"]) block
evalBlock Haskell block = launchWithData (proc sandbox_cmd [sandbox_conf, "runghc"]) block

launchWithLine :: HasCallStack => CreateProcess -> Text -> EvalM Text
launchWithLine cp s = launchWithData cp (T.filter (/= '\n') s `T.snoc` '\n')

launchWithData :: HasCallStack => CreateProcess -> Text -> EvalM Text
launchWithData cp s = liftIO $ do
  T.writeFile "input" s
  input <- openBinaryFile "input" ReadMode
  (outr, outw) <- createPipe
  (_, _, _, p) <- createProcess cp { std_in = UseHandle input
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
