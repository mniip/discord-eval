{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Main where

import Control.Arrow
import Control.Concurrent
import Control.Lens
import Control.Monad
import Control.Monad.Catch
import Control.Monad.IO.Class
import Data.Aeson
import Data.ByteString (ByteString)
import qualified Data.ByteString as B
import Data.Char
import Data.List
import Data.Map (Map)
import qualified Data.Map as M
import Data.Ord
import Data.Set (Set)
import qualified Data.Set as S
import Data.Text (Text)
import qualified Data.Text as T
import Data.Text.Encoding
import Data.Text.Encoding.Error
import Data.Time.Clock
import GHC.Stack
import System.IO
import System.Process
import Waargonaut
import Waargonaut.Types hiding (elem)

import Discord

import DupQueue
import JsonUtil
import Pastebin
import StateM

config_file = "eval.conf"

type BotHandle = (RestChan, Gateway, [ThreadIdType])
data Mode = HaskellEval | Haskell | C | Shell deriving (Eq, Show)
data Command = Reset Mode | EvalLine Mode Text | EvalBlock Mode Text deriving (Eq, Show)
type ParsedReq = [Command]

data EvalState = EvalState
  { _botHandle :: BotHandle
  , _recentMsgs :: Map (Snowflake, Snowflake) (UTCTime, ParsedReq, Maybe Snowflake)
  , _channel :: UChan (Snowflake, Snowflake)
  }
makeLenses ''EvalState

type EvalM = StateM Json EvalState

logM :: String -> EvalM ()
logM s = do
  time <- liftIO getCurrentTime
  let line = unlines $ map (show time ++) $ zipWith (++) (": " : repeat "> ") $ lines s
  preuse (persistent . oat "log" . _Just . jstring) >>= \case
    Just logFile -> liftIO $ appendFile logFile line
    Nothing -> pure ()
  liftIO $ putStr line

logShowM :: Show a => a -> EvalM ()
logShowM = logM . show

traceM :: HasCallStack => EvalM ()
traceM = logShowM callStack

instance Exception RestCallException

sendEither :: (FromJSON a, Request (r a)) => r a -> EvalM (Either RestCallException a)
sendEither req = use (dynamic . botHandle) >>= \bot -> liftIO $ restCall bot req

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
          let initEnv = EvalState { _botHandle = error "Not connected"
                                  , _recentMsgs = M.empty
                                  , _channel = c
                                  }
          evalStateM (readJsonFile config_file) (writeJsonFile config_file) initEnv $ do
            forkStateM backendLoop
            mainLoop
  where mainLoop = forever $ catch (do Just !auth <- preuse (persistent . oat "auth" . _Just . jtext)
                                       bot <- liftIO $ loginRestGateway $ Auth auth
                                       finally (do assign (dynamic . botHandle) bot
                                                   logM "Connected"
                                                   eventLoop)
                                               (do logM "Disconnected"
                                                   liftIO $ stopDiscord bot)
                                       pure ())
                                   (\e -> do logM "Exception in mainLoop:"
                                             logShowM (e :: SomeException)
                                             liftIO $ threadDelay $ 10 * 1000000)
        eventLoop = do bot <- use (dynamic . botHandle)
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

parseMessage :: Int -> Text -> ParsedReq
parseMessage maxBlocks t | Just ('>', cmd) <- T.uncons t
                         , (m, rest) <- T.break (\x -> x == '`' || isSpace x) cmd = take maxBlocks $ go (parseMode m) rest
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
parseMessage _ _ = []

pruneMessages :: EvalM ()
pruneMessages = do time <- liftIO getCurrentTime
                   Just !secs <- preuse (persistent . oat "pruneAfterSeconds" . _Just . jint)
                   modifying (dynamic . recentMsgs)
                     (M.filter (\(ts, _, _) -> diffUTCTime time ts < fromIntegral secs))

checkTest :: Maybe Snowflake -> EvalM () -> EvalM ()
checkTest mb c = do Just !guilds <- preuses (persistent . oat "testGuilds" . _Just . jarr)
                                            (toListOf $ folded . jint . to fromIntegral)
                    Just !test <- preuse (persistent . oat "test" . _Just . jbool)
                    when (test == any (`elem` guilds) mb) $ c

handleEvent :: HasCallStack => Event -> EvalM ()
handleEvent (MessageCreate Message{..}) = checkTest messageGuild $ do
  pruneMessages
  time <- liftIO getCurrentTime
  Just !maxBlocks <- preuse (persistent . oat "maxBlocksPerMsg" . _Just . jint)
  let req = parseMessage maxBlocks messageText
  assign (dynamic . recentMsgs . at (messageChannel, messageId)) $ Just (time, req, Nothing)
  unless (null req) $ do
    queue <- use (dynamic . channel)
    status <- liftIO $ writeUChan queue (messageChannel, messageId)
    when (status == Just False) $ do
      Just !wait <- preuse (persistent . oat "reactWait" . _Just . jtext)
      sendTrace $ CreateReaction (messageChannel, messageId) wait
handleEvent (MessageUpdate Message{..}) = checkTest messageGuild $ do
  pruneMessages
  x <- use (dynamic . recentMsgs . at (messageChannel, messageId))
  Just !maxBlocks <- preuse (persistent . oat "maxBlocksPerMsg" . _Just . jint)
  case x of
    Just (ts, oldreq, reply) | req <- parseMessage maxBlocks messageText
                             , oldreq /= req
                             -> if null req
                                then case reply of
                                  Just id -> do sendTrace $ DeleteMessage (messageChannel, id)
                                                assign (dynamic . recentMsgs . at (messageChannel, messageId) . _Just . _3) Nothing
                                  _ -> pure ()
                                else do assign (dynamic . recentMsgs . at (messageChannel, messageId)) $ Just (ts, req, reply)
                                        queue <- use (dynamic . channel)
                                        status <- liftIO $ writeUChan queue (messageChannel, messageId)
                                        when (status == Just False) $ do
                                          Just !wait <- preuse (persistent . oat "reactWait" . _Just . jtext)
                                          sendTrace $ CreateReaction (messageChannel, messageId) wait
    _ -> pure ()
handleEvent (MessageDelete messageChannel messageId) = do
  pruneMessages
  x <- use (dynamic . recentMsgs . at (messageChannel, messageId))
  case x of
    Just (ts, oldreq, Just id) -> sendTrace $ DeleteMessage (messageChannel, id)
    _ -> pure ()
handleEvent _ = pure ()

toBS :: Text -> ByteString
toBS = encodeUtf8

fromBS :: ByteString -> Text
fromBS = decodeUtf8With (replace '?')

backendLoop :: EvalM ()
backendLoop = forever $ catch (do queue <- use (dynamic . channel)
                                  (chan, msg) <- liftIO $ readUChan queue
                                  Just !wait <- preuse (persistent . oat "reactWait" . _Just . jtext)
                                  sendTrace $ DeleteOwnReaction (chan, msg) wait
                                  x <- use (dynamic . recentMsgs . at (chan, msg))
                                  case x of
                                    Just (_, req, reply) -> do
                                      outs <- forM req $ \cmd -> do
                                        sendTrace $ TriggerTypingIndicator chan
                                        case cmd of
                                          Reset mode -> do resetMode mode
                                                           pure ""
                                          EvalLine mode ln -> fromBS <$> evalLine mode (toBS ln)
                                          EvalBlock mode blk -> fromBS <$> evalBlock mode (toBS blk)
                                      text <- formatResults outs
                                      case reply of
                                        Just id -> sendTrace $ EditMessage (chan, id) text Nothing
                                        _ -> do Message{..} <- send $ CreateMessage chan text
                                                assign (dynamic . recentMsgs . at (chan, msg) . _Just . _3) (Just messageId)
                                    Nothing -> pure ())
                              (\e -> do logM "Exception in backendLoop:"
                                        logShowM (e :: SomeException))

formatResults :: [Text] -> EvalM Text
formatResults res = do Just !maxChars <- preuse (persistent . oat "maxCharsPerMsg" . _Just . jint)
                       msg <- doFormat maxChars
                       if T.null msg
                       then do Just !check <- preuse (persistent . oat "reactCheck" . _Just . jtext)
                               pure check
                       else pure msg
  where doFormat maxChars = T.concat <$> mapM format nonempty
          where
            nonempty = zip [0..] $ filter (not . T.null . fst) $ map (sanitize &&& id) res

            sanitize s = let r = T.replace "``" "``\x200D" $ T.filter (\x -> x == '\n' || not (isControl x)) s
                         in if T.isSuffixOf "`" r then T.append r "\x200D" else r

            sorted = sortBy (comparing $ T.length . fst . snd) nonempty

            accumulate :: Int -> Set Int -> [(Int, (Text, Text))] -> Set Int
            accumulate n s [] = s
            accumulate n s ((i, (x, _)):xs)
              | n + 8 + T.length x < maxChars - (3 + pasteUrlLength) * length xs
              = accumulate (n + 8 + T.length x) (S.insert i s) xs
              | otherwise = s

            small = accumulate 0 S.empty sorted

            format (i, (san, orig))
              | i `S.member` small = pure $ T.concat ["```\n", san, "```\n"]
              | otherwise = do link <- liftIO $ paste $ encodeUtf8 orig -- TODO: don't double encode
                               pure $ T.concat ["<", decodeUtf8With lenientDecode link, ">\n"]

resetMode :: Mode -> EvalM ()
resetMode HaskellEval = launchWithData ["kill", "Dghci"] "" >> pure ()
resetMode _ = pure ()

evalLine :: Mode -> ByteString -> EvalM ByteString
evalLine HaskellEval line = launchWithLine ["Dghci"] line
evalLine mode line = evalBlock mode line

evalBlock :: Mode -> ByteString -> EvalM ByteString
evalBlock HaskellEval block = do
  Just !maxChars <- preuse (persistent . oat "maxOutput" . _Just . jint)
  fmap (B.take maxChars . B.concat) $ mapM (evalLine HaskellEval) $ [":{"] ++ B.split (fromIntegral $ fromEnum '\n') block ++ [":}"]
evalBlock C block = launchWithData ["rungcc"] block
evalBlock Shell block = launchWithData ["bash"] block
evalBlock Haskell block = launchWithData ["runghc"] block

launchWithLine :: HasCallStack => [String] -> ByteString -> EvalM ByteString
launchWithLine args s = launchWithData args (B.filter (/= fromIntegral (fromEnum '\n')) s `B.snoc` fromIntegral (fromEnum '\n'))

launchWithData :: HasCallStack => [String] -> ByteString -> EvalM ByteString
launchWithData args s = do
  Just !cmd <- preuse (persistent . oat "sandboxCmd" . _Just . jstring)
  Just !conf <- preuse (persistent . oat "sandboxConf" . _Just . jstring)
  liftIO $ B.writeFile "input" s
  input <- liftIO $ openBinaryFile "input" ReadMode
  (outr, outw) <- liftIO createPipe
  (_, _, _, p) <- liftIO $ createProcess (proc cmd (conf:args)) { std_in = UseHandle input
                                                                , std_out = UseHandle outw
                                                                , std_err = UseHandle outw
                                                                , close_fds = True
                                                                }
  Just !maxChars <- preuse (persistent . oat "maxOutput" . _Just . jint)
  liftIO $ finally (B.hGet outr maxChars)
                   (do hClose outr
                       waitForProcess p)
