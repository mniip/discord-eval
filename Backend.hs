{-# OPTIONS_GHC -fplugin=Polysemy.Plugin #-}
module Backend where

import Calamity hiding (Embed)
import Control.Concurrent
import Control.Exception hiding (bracket, bracket_)
import Control.Exception qualified as E
import Control.Monad
import Control.Monad.IO.Class
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.Foldable
import Data.List.NonEmpty qualified as NE
import Data.Map qualified as M
import Data.Text qualified as T
import Data.Text.Encoding qualified as T
import Data.Traversable
import Df1 hiding (Message)
import DiPolysemy
import Network.HTTP.Client hiding (Request)
import Network.HTTP.Client.TLS
import Polysemy
import Polysemy.Error
import Polysemy.Reader
import Polysemy.Resource
import System.IO
import System.Process hiding (runCommand)
import TextShow

import Config
import Format
import Types
import UpdateChan

communicateProc
  :: Members '[Resource, Embed IO, Reader EvalConfig, LogEff] r
  => CmdLine -> ByteString -> Sem r ByteString
communicateProc (cmd NE.:| args) input = do
  (rdIn, wrIn) <- liftIO createPipe
  (rdOut, wrOut) <- liftIO createPipe
  bracket
    do
      debug $ "process: " <> showt cmd <> " " <> showt args
      liftIO $ createProcess (proc cmd args)
        { std_in = UseHandle rdIn
        , std_out = UseHandle wrOut
        , std_err = UseHandle wrOut
        , close_fds = True
        }
    (\(_, _, _, p) -> do
      code <- liftIO $ waitForProcess p
      debug $ "exit: " <> showt code)
    \_ -> do
      liftIO do
        hClose rdIn
        hClose wrOut
        void $ forkIO $ E.finally (BS.hPut wrIn input) (hClose wrIn)
      config :: EvalConfig <- ask
      liftIO $ E.finally (BS.hGet rdOut config.pastebinSizeLimit) (hClose rdOut)

runCmd
  :: Members '[Resource, Embed IO, Reader EvalConfig, LogEff] r
  => (Int, Command) -> Sem r ByteString
runCmd (i, cmd) = push (segment $ "C" <> showt i) do
  config :: EvalConfig <- ask
  case cmd of
    Reset name
      | Just LiveState { resetCommand } <- M.lookup name config.interpreters
      -> communicateProc resetCommand ""
    EvalLine name txt
      | Just LiveState { runCommand } <- M.lookup name config.interpreters
      -> communicateProc runCommand (T.encodeUtf8 txt)
    EvalBlock name txt
      | Just LiveState { runCommand } <- M.lookup name config.interpreters
      -> communicateProc runCommand (T.encodeUtf8 txt)
    _ -> pure mempty

backend
  :: Members '[Resource, Final IO, Embed IO, Reader EvalConfig, LogEff] r
  => UpdateChan (Snowflake Message) Request -> Sem r ()
backend chan = bracket_
  (info @String "Starting backend thread")
  (info @String "Closing backend thread")
  $ push "backend" do
    mgr <- liftIO $ newManager tlsManagerSettings
    runReader mgr $ forever do
      debug @String "Waiting"
      (msgId, req NE.:| oldReqs) <- readUpdateChan chan
      push (segment $ showt msgId) do
        for_ oldReqs \oldReq -> liftIO $ putMVar oldReq.response Nothing
        var <- liftIO newEmptyMVar
        liftIO $ putMVar req.response $ Just var

        eTxt <- runError $ fromExceptionSem $
          fold <$> for req.commands \commands -> do
            config :: EvalConfig <- ask
            outputs <- filter (not . BS.null)
              <$> traverse runCmd (zip [0..] $ NE.toList commands)
            formatResults config.msgSizeLimit outputs

        liftIO $ putMVar var $ case eTxt of
          Right txt -> txt
          Left (exc :: SomeException) -> showt exc
