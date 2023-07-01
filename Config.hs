module Config where

import Calamity.Types
import Data.Aeson
import Data.List.NonEmpty (NonEmpty)
import Data.Map (Map)
import Data.Set (Set)
import Data.Text (Text)
import GHC.Generics (Generic)

newtype InterpreterName = InterpreterName Text
  deriving newtype (Eq, Ord, Show, FromJSONKey, ToJSONKey, FromJSON, ToJSON)

newtype Blocks = Blocks Int
  deriving newtype (FromJSON, ToJSON)

type CmdLine = NonEmpty FilePath

data Interpreter
  = LiveState
  { runCommand :: !CmdLine
  , resetCommand :: !CmdLine
  }
  | IndexedState
  { runCommand :: !CmdLine
  , pruneCommand :: !CmdLine
  }
  deriving stock (Generic)
  deriving anyclass (FromJSON, ToJSON)

data Wrapper = Wrapper
  { prefix :: !(Maybe Text)
  , suffix :: !(Maybe Text)
  , stripControl :: !(Maybe Bool)
  , interpreter :: !InterpreterName
  }
  deriving stock (Generic)
  deriving anyclass (FromJSON, ToJSON)

data EvalConfig = EvalConfig
  { logFile :: !FilePath
  , token :: !Text
  , testGuilds :: !(Set (Snowflake Guild))
  , isTest :: !Bool
  , forgetAfterSeconds :: !Int
  , blockCountLimit :: !Blocks
  , msgSizeLimit :: !Int
  , pastebinSizeLimit :: !Int
  , reactWait :: !RawEmoji
  , reactCancel :: !RawEmoji
  , emptyOutput :: !Text
  , interpreters :: !(Map InterpreterName Interpreter)
  , defaultInline :: Wrapper
  , defaultCodeBlock :: Wrapper
  , codeBlockLanguages :: !(Map Text Wrapper)
  }
  deriving stock (Generic)
  deriving anyclass (FromJSON, ToJSON)

configFile :: FilePath
configFile = "eval.yaml"
