{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}

module StateM where

import Control.Concurrent
import Control.Lens
import Control.Monad
import Control.Monad.Fail
import Control.Monad.Catch
import Control.Monad.IO.Class
import Control.Monad.State.Class
import Control.Monad.Trans.Reader

data StateMS p d = StateMS
  { _persistent :: !p
  , _dynamic :: !d
  }
makeLenses ''StateMS

data StateMR p d = StateMR
  { stateRead :: !(IO p)
  , stateWrite :: !(p -> IO ())
  , stateMVar :: !(MVar (StateMS p d))
  }

-- | In 'StateM p d a', @p@ is the persistent state type and @d@ is the dynamic state type.
newtype StateM p d a = StateM { unStateM :: ReaderT (StateMR p d) IO a }
  deriving (Functor, Applicative, Monad, MonadIO, MonadThrow, MonadCatch, MonadMask)

instance Eq p => MonadState (StateMS p d) (StateM p d) where
  get = StateM $ ReaderT $ \StateMR{..} -> readMVar stateMVar
  state f = StateM $ ReaderT $ \StateMR{..} -> modifyMVar stateMVar $ \ms -> do
    let (a, !ms') = f ms
    when (_persistent ms' /= _persistent ms) $ stateWrite (_persistent ms')
    pure (ms', a)

instance MonadFail (StateM p d) where
  fail = error

rehashStateM :: StateM p d ()
rehashStateM = StateM $ ReaderT $ \StateMR{..} -> modifyMVar_ stateMVar $ \ms -> do
  p <- stateRead
  pure (ms { _persistent = p })

unliftingStateM :: IO p -> (p -> IO ()) -> d -> ((forall x. StateM p d x -> IO x) -> IO a) -> IO a
unliftingStateM rd wr d k = do
  p <- rd
  mvar <- newMVar $ StateMS { _persistent = p
                            , _dynamic = d
                            }
  k (\(StateM sm) -> runReaderT sm $ StateMR { stateRead = rd
                                             , stateWrite = wr
                                             , stateMVar = mvar
                                             }
    )

evalStateM :: IO p -> (p -> IO ()) -> d -> StateM p d a -> IO a
evalStateM rd wr d (StateM f) = do
  p <- rd
  mvar <- newMVar $ StateMS { _persistent = p
                            , _dynamic = d
                            }
  runReaderT f $ StateMR { stateRead = rd
                         , stateWrite = wr
                         , stateMVar = mvar
                         }

forkStateM :: StateM p d () -> StateM p d ()
forkStateM (StateM (ReaderT f)) = StateM $ ReaderT $ \mr -> void $ forkIO (f mr)
