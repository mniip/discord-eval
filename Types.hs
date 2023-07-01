module Types where

import Control.Concurrent
import Data.List.NonEmpty (NonEmpty)
import Data.Text (Text)
import TextShow

import Config

data Command
  = Reset InterpreterName
  | EvalLine InterpreterName Text
  | EvalBlock InterpreterName Text
  deriving stock (Eq, Show)
  deriving TextShow via FromStringShow Command

data Request = Request
  { commands :: !(Maybe (NonEmpty Command))
  , response :: !(MVar (Maybe (MVar Text)))
  }
