-- | Description : Internal custom Prelude.
--
-- Not meant for use by users of the library. It is here to avoid a whole Relude dependency.
-- Some of the module is directly copy pasted from Relude.
-- It is an exposed module in case someone does find some use in them.
module Sayland.Internal.Prelude (
  module Sayland.Internal.Prelude,
  module Data.IORef,
  module Prelude,
  module Control.Concurrent.MVar,
  module Control.Monad.STM,
) where

import Control.Concurrent.MVar hiding (newEmptyMVar, newMVar, putMVar, takeMVar, tryTakeMVar)
import Control.Concurrent.MVar qualified as CCM
import Control.Exception (Exception)
import Control.Exception qualified as CE
import Control.Monad (void)
import Control.Monad.IO.Class
import Control.Monad.STM hiding (atomically)
import Control.Monad.STM qualified as STM
import Data.IORef hiding (atomicModifyIORef, atomicModifyIORef', atomicWriteIORef, newIORef, readIORef)
import Data.IORef qualified as Ref
import GHC.Stack (HasCallStack)
import Prelude

-- | lifted 'throwIO'.
throwIO :: (HasCallStack, Exception e, MonadIO m) => e -> m a
throwIO = liftIO . CE.throwIO
{-# INLINE throwIO #-}
{-# SPECIALIZE throwIO :: (HasCallStack, Exception e) => e -> IO a #-}

-- | Lifted to 'MonadIO' version of 'STM.atomically'.
atomically :: (MonadIO m) => STM a -> m a
atomically = liftIO . STM.atomically
{-# INLINE atomically #-}
{-# SPECIALIZE atomically :: STM a -> IO a #-}

-- | Lifted `atomicModifyIORef'`
atomicModifyIORef' :: (MonadIO m) => IORef a -> (a -> (a, b)) -> m b
atomicModifyIORef' ref how = liftIO $ Ref.atomicModifyIORef' ref how
{-# INLINE atomicModifyIORef' #-}
{-# SPECIALIZE atomicModifyIORef' :: IORef a -> (a -> (a, b)) -> IO b #-}

atomicWriteIORef :: (MonadIO m) => IORef a -> a -> m ()
atomicWriteIORef ref what = liftIO $ Ref.atomicWriteIORef ref what
{-# INLINE atomicWriteIORef #-}
{-# SPECIALIZE atomicWriteIORef :: IORef a -> a -> IO () #-}

-- | Lifted `readIORef`
readIORef :: (MonadIO m) => IORef a -> m a
readIORef = liftIO . Ref.readIORef
{-# INLINE readIORef #-}
{-# SPECIALIZE readIORef :: IORef a -> IO a #-}

-- | Lifted `newIORef`
newIORef :: (MonadIO m) => a -> m (IORef a)
newIORef = liftIO . Ref.newIORef
{-# INLINE newIORef #-}
{-# SPECIALIZE newIORef :: a -> IO (IORef a) #-}

-- | Lifted version of 'CCM.putMVar'.
putMVar :: (MonadIO m) => MVar a -> a -> m ()
putMVar m a = liftIO $ CCM.putMVar m a
{-# INLINE putMVar #-}
{-# SPECIALIZE putMVar :: MVar a -> a -> IO () #-}

-- | Lifted version of 'CCM.takeMVar'.
takeMVar :: (MonadIO m) => MVar a -> m a
takeMVar a = liftIO $ CCM.takeMVar a
{-# INLINE takeMVar #-}
{-# SPECIALIZE takeMVar :: MVar a -> IO a #-}

-- | Lifted to version of 'CCM.newMVar'.
newMVar :: (MonadIO m) => a -> m (MVar a)
newMVar = liftIO . CCM.newMVar
{-# INLINE newMVar #-}
{-# SPECIALIZE newMVar :: a -> IO (MVar a) #-}

-- | Lifted to version of 'CCM.newEmptyMVar'.
newEmptyMVar :: (MonadIO m) => m (MVar a)
newEmptyMVar = liftIO CCM.newEmptyMVar
{-# INLINE newEmptyMVar #-}
{-# SPECIALIZE newEmptyMVar :: IO (MVar a) #-}

-- | Lifted version of 'CCM.tryTakeMVar'.
tryTakeMVar :: (MonadIO m) => MVar a -> m (Maybe a)
tryTakeMVar = liftIO . CCM.tryTakeMVar
{-# INLINE tryTakeMVar #-}
{-# SPECIALIZE tryTakeMVar :: MVar a -> IO (Maybe a) #-}

tryTakeMVar_ :: (MonadIO m) => MVar a -> m ()
tryTakeMVar_ = void . liftIO . CCM.tryTakeMVar
{-# INLINE tryTakeMVar_ #-}
{-# SPECIALIZE tryTakeMVar_ :: MVar a -> IO () #-}

-- | Alias for `pure ()`
pass :: (Applicative f) => f ()
pass = pure ()
{-# INLINE pass #-}
