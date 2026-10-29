-- | Description : The monad Sayland runs in on the dejafu branch, and its concurrency primitives.
--
-- This branch exists to find race conditions with <https://hackage.haskell.org/package/dejafu dejafu>.
-- Everything main does in @IO@ runs in dejafu's t'ConcIO' here, so dejafu's scheduler sees every thread, lock and
-- transaction, and can try the ways they interleave.
--
-- The types keep the names main uses (t'IORef', t'MVar', t'TVar', t'STM', ...) so the rest of the library reads the same.
-- They are fixed to t'ConcIO' rather than left polymorphic, so dejafu's types don't spread to every object.
-- The functions on them are monomorphic for the same reason, and the ones main lifts with 'Control.Monad.IO.Class.liftIO'
-- are lifted with 'liftConc'.
module Sayland.Internal.Conc (
  ConcIO,
  MonadConcIO (..),

  -- * Types
  STM,
  TVar,
  TQueue,
  TMVar,
  IORef,
  MVar,
  ThreadId,

  -- * Transactions
  atomically,
  newTVar,
  newTVarN,
  newTVarIO,
  readTVar,
  readTVarIO,
  writeTVar,
  modifyTVar,
  modifyTVar',
  swapTVar,
  stateTVar,
  retry,
  check,
  orElse,
  throwSTM,
  newTQueue,
  newTQueueIO,
  writeTQueue,
  readTQueue,
  tryReadTQueue,
  flushTQueue,
  unGetTQueue,
  isEmptyTQueue,
  newTMVar,
  newEmptyTMVar,
  newEmptyTMVarIO,
  takeTMVar,
  putTMVar,
  readTMVar,
  tryTakeTMVar,
  tryPutTMVar,
  tryReadTMVar,
  writeTMVar,

  -- * IORefs
  newIORef,
  newIORefN,
  readIORef,
  writeIORef,
  modifyIORef,
  modifyIORef',
  atomicModifyIORef',
  atomicWriteIORef,
  atomicSwapIORef,

  -- * MVars
  newMVar,
  newMVarN,
  newEmptyMVar,
  putMVar,
  takeMVar,
  readMVar,
  tryTakeMVar,
  tryTakeMVar_,
  tryPutMVar,
  tryReadMVar,
  isEmptyMVar,
  withMVar,
  modifyMVar,
  modifyMVar_,

  -- * Threads
  fork,
  forkN,
  forkFinally,
  forkFinallyN,
  killThread,
  throwTo,
  myThreadId,
  yield,

  -- * Exceptions
  throwIO,

  -- * Other
  pass,
) where

import Control.Concurrent.Classy qualified as C
import Control.Exception (Exception, SomeException)
import Control.Monad (void)
import Control.Monad.Catch (throwM, try)
import Control.Monad.Reader (ReaderT, lift)
import Data.Maybe (isNothing)
import Test.DejaFu.Conc (ConcIO)
import Prelude

-- | Transactions, dejafu's model of them.
type STM = C.STM ConcIO

-- | A transactional variable.
type TVar = C.TVar STM

-- | A transactional queue.
type TQueue = C.TQueue STM

-- | A transactional MVar.
type TMVar = C.TMVar STM

-- | A mutable reference. dejafu models relaxed memory for these, like the hardware does.
type IORef = C.IORef ConcIO

-- | A synchronising variable.
type MVar = C.MVar ConcIO

-- | A thread. dejafu shows threads by the names given to `forkN` in its traces.
type ThreadId = C.ThreadId ConcIO

-- | Monads that can run t'ConcIO' actions. Plays the part of 'Control.Monad.IO.Class.MonadIO' on this branch.
class (Monad m) => MonadConcIO m where
  -- | Run a t'ConcIO' action, like 'Control.Monad.IO.Class.liftIO'.
  liftConc :: ConcIO a -> m a

instance MonadConcIO ConcIO where
  liftConc = id

instance (MonadConcIO m) => MonadConcIO (ReaderT r m) where
  liftConc = lift . liftConc

-- Transactions {{{

-- | Run a transaction.
atomically :: (MonadConcIO m) => STM a -> m a
atomically = liftConc . C.atomically

newTVar :: a -> STM (TVar a)
newTVar = C.newTVar

-- | A t'TVar' with a name, which dejafu shows in its traces.
newTVarN :: String -> a -> STM (TVar a)
newTVarN = C.newTVarN

newTVarIO :: (MonadConcIO m) => a -> m (TVar a)
newTVarIO = liftConc . C.newTVarConc

readTVar :: TVar a -> STM a
readTVar = C.readTVar

readTVarIO :: (MonadConcIO m) => TVar a -> m a
readTVarIO = liftConc . C.readTVarConc

writeTVar :: TVar a -> a -> STM ()
writeTVar = C.writeTVar

modifyTVar :: TVar a -> (a -> a) -> STM ()
modifyTVar = C.modifyTVar

modifyTVar' :: TVar a -> (a -> a) -> STM ()
modifyTVar' = C.modifyTVar'

swapTVar :: TVar a -> a -> STM a
swapTVar = C.swapTVar

stateTVar :: TVar s -> (s -> (a, s)) -> STM a
stateTVar = C.stateTVar

retry :: STM a
retry = C.retry

check :: Bool -> STM ()
check = C.check

orElse :: STM a -> STM a -> STM a
orElse = C.orElse

throwSTM :: (Exception e) => e -> STM a
throwSTM = C.throwSTM

newTQueue :: STM (TQueue a)
newTQueue = C.newTQueue

newTQueueIO :: (MonadConcIO m) => m (TQueue a)
newTQueueIO = atomically newTQueue

writeTQueue :: TQueue a -> a -> STM ()
writeTQueue = C.writeTQueue

readTQueue :: TQueue a -> STM a
readTQueue = C.readTQueue

tryReadTQueue :: TQueue a -> STM (Maybe a)
tryReadTQueue = C.tryReadTQueue

flushTQueue :: TQueue a -> STM [a]
flushTQueue = C.flushTQueue

unGetTQueue :: TQueue a -> a -> STM ()
unGetTQueue = C.unGetTQueue

isEmptyTQueue :: TQueue a -> STM Bool
isEmptyTQueue = C.isEmptyTQueue

newTMVar :: a -> STM (TMVar a)
newTMVar = C.newTMVar

newEmptyTMVar :: STM (TMVar a)
newEmptyTMVar = C.newEmptyTMVar

newEmptyTMVarIO :: (MonadConcIO m) => m (TMVar a)
newEmptyTMVarIO = atomically newEmptyTMVar

takeTMVar :: TMVar a -> STM a
takeTMVar = C.takeTMVar

putTMVar :: TMVar a -> a -> STM ()
putTMVar = C.putTMVar

readTMVar :: TMVar a -> STM a
readTMVar = C.readTMVar

tryTakeTMVar :: TMVar a -> STM (Maybe a)
tryTakeTMVar = C.tryTakeTMVar

tryPutTMVar :: TMVar a -> a -> STM Bool
tryPutTMVar = C.tryPutTMVar

tryReadTMVar :: TMVar a -> STM (Maybe a)
tryReadTMVar = C.tryReadTMVar

-- | Put a value in a t'TMVar', replacing what it held.
writeTMVar :: TMVar a -> a -> STM ()
writeTMVar var a = tryTakeTMVar var >> putTMVar var a

-- }}}

-- IORefs {{{

newIORef :: (MonadConcIO m) => a -> m (IORef a)
newIORef = liftConc . C.newIORef

-- | A t'IORef' with a name, which dejafu shows in its traces.
newIORefN :: (MonadConcIO m) => String -> a -> m (IORef a)
newIORefN name = liftConc . C.newIORefN name

readIORef :: (MonadConcIO m) => IORef a -> m a
readIORef = liftConc . C.readIORef

writeIORef :: (MonadConcIO m) => IORef a -> a -> m ()
writeIORef ref = liftConc . C.writeIORef ref

-- | Not atomic: a read, then a write.
modifyIORef :: (MonadConcIO m) => IORef a -> (a -> a) -> m ()
modifyIORef ref = liftConc . C.modifyIORef ref

-- | Not atomic: a read, then a write.
modifyIORef' :: (MonadConcIO m) => IORef a -> (a -> a) -> m ()
modifyIORef' ref = liftConc . C.modifyIORef' ref

atomicModifyIORef' :: (MonadConcIO m) => IORef a -> (a -> (a, b)) -> m b
atomicModifyIORef' ref = liftConc . C.atomicModifyIORef' ref

atomicWriteIORef :: (MonadConcIO m) => IORef a -> a -> m ()
atomicWriteIORef ref = liftConc . C.atomicWriteIORef ref

-- | Put a value in an t'IORef' and return the old one, atomically. Like "GHC.IORef"'s.
atomicSwapIORef :: (MonadConcIO m) => IORef a -> a -> m a
atomicSwapIORef ref new = atomicModifyIORef' ref (new,)

-- }}}

-- MVars {{{

newMVar :: (MonadConcIO m) => a -> m (MVar a)
newMVar = liftConc . C.newMVar

-- | An t'MVar' with a name, which dejafu shows in its traces.
newMVarN :: (MonadConcIO m) => String -> a -> m (MVar a)
newMVarN name = liftConc . C.newMVarN name

newEmptyMVar :: (MonadConcIO m) => m (MVar a)
newEmptyMVar = liftConc C.newEmptyMVar

putMVar :: (MonadConcIO m) => MVar a -> a -> m ()
putMVar var = liftConc . C.putMVar var

takeMVar :: (MonadConcIO m) => MVar a -> m a
takeMVar = liftConc . C.takeMVar

readMVar :: (MonadConcIO m) => MVar a -> m a
readMVar = liftConc . C.readMVar

tryTakeMVar :: (MonadConcIO m) => MVar a -> m (Maybe a)
tryTakeMVar = liftConc . C.tryTakeMVar

-- | Like `tryTakeMVar`, but discards the value.
tryTakeMVar_ :: (MonadConcIO m) => MVar a -> m ()
tryTakeMVar_ = void . tryTakeMVar

tryPutMVar :: (MonadConcIO m) => MVar a -> a -> m Bool
tryPutMVar var = liftConc . C.tryPutMVar var

tryReadMVar :: (MonadConcIO m) => MVar a -> m (Maybe a)
tryReadMVar = liftConc . C.tryReadMVar

-- | Whether an t'MVar' is empty. Not concurrency's: it returns whether the t'MVar' is full (as of version 1.11.0.3).
isEmptyMVar :: (MonadConcIO m) => MVar a -> m Bool
isEmptyMVar = fmap isNothing . tryReadMVar

-- | Run with the value of an t'MVar' taken, putting it back after. Like main's, the action is not lifted: lift the
-- whole call with 'liftConc'.
withMVar :: MVar a -> (a -> ConcIO b) -> ConcIO b
withMVar = C.withMVar

-- | Modify the value of an t'MVar', putting the old value back if the action throws.
modifyMVar :: MVar a -> (a -> ConcIO (a, b)) -> ConcIO b
modifyMVar = C.modifyMVar

-- | `modifyMVar` without a result.
modifyMVar_ :: MVar a -> (a -> ConcIO a) -> ConcIO ()
modifyMVar_ = C.modifyMVar_

-- }}}

-- Threads {{{

fork :: ConcIO () -> ConcIO ThreadId
fork = C.fork

-- | Fork a thread with a name, which dejafu shows in its traces.
forkN :: String -> ConcIO () -> ConcIO ThreadId
forkN = C.forkN

-- | Fork a thread, then run a handler with how it ended. The handler runs with asynchronous exceptions masked.
forkFinally :: ConcIO a -> (Either SomeException a -> ConcIO ()) -> ConcIO ThreadId
forkFinally = C.forkFinally

-- | `forkFinally` for a thread with a name, which dejafu shows in its traces.
forkFinallyN :: String -> ConcIO a -> (Either SomeException a -> ConcIO ()) -> ConcIO ThreadId
forkFinallyN name action andThen = C.mask $ \restore -> C.forkN name $ try (restore action) >>= andThen

killThread :: ThreadId -> ConcIO ()
killThread = C.killThread

throwTo :: (Exception e) => ThreadId -> e -> ConcIO ()
throwTo = C.throwTo

myThreadId :: (MonadConcIO m) => m ThreadId
myThreadId = liftConc C.myThreadId

yield :: (MonadConcIO m) => m ()
yield = liftConc C.yield

-- }}}

-- | Throw an exception, like 'Control.Exception.throwIO'.
throwIO :: (Exception e, MonadConcIO m) => e -> m a
throwIO = liftConc . throwM

-- | Alias for `pure ()`
pass :: (Applicative f) => f ()
pass = pure ()
{-# INLINE pass #-}

-- vim: foldmethod=marker
