module UpdateChan
  ( UpdateChan
  , newUpdateChan
  , writeUpdateChan
  , readUpdateChan
  ) where

import Control.Concurrent.STM
import Control.Exception
import Control.Monad.IO.Class
import Data.Bifunctor
import Data.List.NonEmpty (NonEmpty)
import Data.List.NonEmpty qualified as NE
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as M
import Data.Maybe
import Data.Sequence (Seq)
import Data.Sequence qualified as Seq

data UpdateChanData k v = UpdateChanData
  { keys :: !(Map k Int)
  , keyOffset :: !Int
  , values :: !(Seq (k, NonEmpty v))
  }
  deriving stock Show

doRead :: Ord k => TVar (UpdateChanData k v) -> STM (k, NonEmpty v)
doRead var = do
  state <- readTVar var
  case Seq.viewl state.values of
    Seq.EmptyL -> retry
    (k, vs) Seq.:< values -> do
      writeTVar var UpdateChanData
        { keys = M.delete k state.keys
        , keyOffset = state.keyOffset - 1
        , values
        }
      pure (k, vs)

doWrite :: Ord k => TVar (UpdateChanData k v) -> k -> v -> STM Bool
doWrite var k v = stateTVar var \state ->
  let
    (mi, keys) = M.insertLookupWithKey (\_ _new old -> old) k
      (Seq.length state.values - state.keyOffset) state.keys
    values = case mi of
      Nothing -> state.values Seq.|> (k, NE.singleton v)
      Just i -> Seq.adjust (second (v NE.<|)) (i + state.keyOffset) state.values
  in
    ( isNothing mi
    , UpdateChanData
      { keys
      , keyOffset = state.keyOffset
      , values
      }
    )

-- | A queue that removes duplicates based on keys and tells whether a push -- has resulted in an immediate pop in another thread.
data UpdateChan k v = UpdateChan
  { state :: !(TVar (UpdateChanData k v))
  , demand :: !(TVar Int)
  }

newUpdateChan :: MonadIO m => m (UpdateChan k v)
newUpdateChan = liftIO do
  state <- newTVarIO UpdateChanData
    { keys = M.empty
    , keyOffset = 0
    , values = Seq.empty
    }
  demand <- newTVarIO 0
  pure UpdateChan{..}

-- | Wait if the queue is empty, and then retrieve the least recently pushed
-- key along with the list of values pushed to it, starting from most recent.
readUpdateChan :: (Ord k, MonadIO m) => UpdateChan k v -> m (k, NonEmpty v)
readUpdateChan chan = liftIO $ bracket acquire release
  \committed -> atomically $ doRead chan.state <* writeTVar committed True
  where
    acquire = atomically do
      modifyTVar' chan.demand succ
      newTVar False
    release committed = atomically $ readTVar committed >>= \case
      True -> pure ()
      False -> modifyTVar' chan.demand pred

-- | Add a key-value pair to the end of the queue, unless the queue already
-- contains a not yet retrieved pair with the same key, in which case the value
-- is added to that location in the queue.
--
-- If by the time this function is entered, some other thread is already waiting
-- in 'readUpdateChan' to retrieve the value we just pushed, return 'True'.
--
-- If by the time this function is exited, no other thread is yet waiting in
-- 'readUpdateChan' to retrieve the value we just pushed, return 'False'.
--
-- In all other cases (if there is a race), the return value is unspecified.
writeUpdateChan :: (Ord k, MonadIO m) => UpdateChan k v -> k -> v -> m Bool
writeUpdateChan chan k v = liftIO $ atomically do
  b <- doWrite chan.state k v
  stateTVar chan.demand (\n -> (n > 0, n - if b then 1 else 0))
