{-# OPTIONS_GHC -fplugin=Polysemy.Plugin #-}
module Main where

import Backend
import Calamity.Cache.InMemory
import Calamity hiding (Embed)
import Calamity.Metrics.Noop
import Config
import Control.Monad.IO.Class
import Data.Yaml
import Di qualified
import DiPolysemy
import Frontend
import Polysemy
import Polysemy.Async
import Polysemy.Reader
import Polysemy.Resource
import UpdateChan

main :: IO ()
main = Di.new \di -> runFinal $ embedToFinal $ runDiToIO di $
  asyncToIOFinal $ resourceToIOFinal $
    liftIO (decodeFileEither configFile) >>= \case
      Left err -> critical $ prettyPrintParseException err
      Right (config :: EvalConfig) -> runReader config do
        chan <- newUpdateChan
        bracket (async $ backend chan) cancel \_ ->
          runMetricsNoop $ runCacheInMemoryNoMsg $ do
            tk <- asks (.token)
            runBotIO (BotToken tk) defaultIntents $
              frontend chan
          >>= \case
            Just (StartupError err) -> critical err
            Nothing -> pure ()
