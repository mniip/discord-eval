{-# OPTIONS_GHC -fplugin=Polysemy.Plugin #-}
module Cache where

import Calamity hiding (Embed, Member)
import Control.Monad.IO.Class
import Data.Default
import Data.List.NonEmpty (NonEmpty)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as M
import Data.Set (Set)
import Data.Set qualified as S
import Data.Time
import Polysemy
import Polysemy.AtomicState
import Polysemy.Reader

import Config
import Types

data ActiveMessage = ActiveMessage
  { id :: !(Snowflake Message)
  , updated :: !UTCTime
  , channel :: !(Snowflake Channel)
  , commands :: !(Maybe (NonEmpty Command))
  , response :: !(Maybe (Snowflake Message))
  }

  deriving Show

data EvalState = EvalState
  { messageById :: !(Map (Snowflake Message) ActiveMessage)
  , messageByAge :: !(Set (UTCTime, Snowflake Message))
  }

instance Default EvalState where
  def = EvalState
    { messageById = M.empty
    , messageByAge = S.empty
    }

pruneMessages
  :: Members '[Embed IO, Reader EvalConfig, AtomicState EvalState] r => Sem r ()
pruneMessages = do
  config <- ask
  now <- liftIO getCurrentTime
  atomicModify' \state -> let
    (pruned, messageByAge) = S.spanAntitone
      (\(t, _) -> now `diffUTCTime` t > secondsToNominalDiffTime
        (fromIntegral config.forgetAfterSeconds))
      state.messageByAge
    messageById = state.messageById `M.withoutKeys` S.map snd pruned
    in state
      { messageByAge
      , messageById
      }

lookupMessage
  :: Member (AtomicState EvalState) r
  => Snowflake Message -> Sem r (Maybe ActiveMessage)
lookupMessage mId = M.lookup mId . (.messageById) <$> atomicGet

upsertMessage
  :: Member (AtomicState EvalState) r => ActiveMessage -> Sem r ()
upsertMessage am = atomicModify' \state ->
  let
    (mOldAm, messageById) = M.insertLookupWithKey
      (\_ new _old -> new) am.id am state.messageById
    messageByAge = S.insert (am.updated, am.id) case mOldAm of
      Nothing -> state.messageByAge
      Just oldAm -> S.delete (oldAm.updated, oldAm.id) state.messageByAge
  in state
    { messageById
    , messageByAge
    }

deleteMessage
  :: Member (AtomicState EvalState) r => Snowflake Message -> Sem r ()
deleteMessage mId = atomicModify' \state ->
  case M.lookup mId state.messageById of
    Just am -> state
      { messageById = M.delete mId state.messageById
      , messageByAge = S.delete (am.updated, am.id) state.messageByAge
      }
    Nothing -> state
