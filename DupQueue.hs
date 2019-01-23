module DupQueue where

import Control.Concurrent
import Control.Exception
import Data.Sequence (Seq)
import qualified Data.Sequence as S

-- | A Chan capable of reporting whether any thread is currently reading.
data CChan a = CChan (MVar (MVar (ChItem a))) (MVar (MVar (ChItem a))) (MVar Int)

data ChItem a = ChItem a (MVar (ChItem a))

newCChan :: IO (CChan a)
newCChan = do hole <- newEmptyMVar
              read <- newMVar hole
              write <- newMVar hole
              cnt <- newMVar 0
              pure (CChan read write cnt)

readCChan :: CChan a -> IO a
readCChan (CChan read _ cnt) = modifyMVarMasked read $ \oldHole -> do
  ChItem x newHole <- readMVar oldHole
  modifyMVar_ cnt (evaluate . pred)
  pure (newHole, x)

-- | Push into a queue and return whether the value was immediately popped.
writeCChan :: CChan a -> a -> IO Bool
writeCChan (CChan read write cnt) x = do
  newHole <- newEmptyMVar
  modifyMVarMasked write $ \oldHole -> do
    waiting <- modifyMVar cnt $ \c -> if c == 0
      then do waiting <- isEmptyMVar read
              evaluate (succ c, waiting)
      else evaluate (succ c, False)
    putMVar oldHole (ChItem x newHole)
    pure (newHole, waiting)

-- | A chan that doesn't accept duplicate values.
data UChan a = UChan (CChan a) (MVar (Seq a))

newUChan :: IO (UChan a)
newUChan = do chan <- newCChan
              seq <- newMVar S.empty
              pure (UChan chan seq)

readUChan :: UChan a -> IO a
readUChan (UChan chan seq) = mask_ $ do
  a <- readCChan chan
  modifyMVar_ seq (evaluate . S.drop 1)
  pure a

-- | If the value isn't currently in the queue, push it and return whether it
-- was immediately popped. Otherwise return Nothing.
writeUChan :: Eq a => UChan a -> a -> IO (Maybe Bool)
writeUChan (UChan chan seq) x = mask_ $ do
  s <- takeMVar seq
  if x `elem` s
  then do putMVar seq s
          pure Nothing
  else do w <- writeCChan chan x
          putMVar seq $! s S.|> x
          pure (Just w)
