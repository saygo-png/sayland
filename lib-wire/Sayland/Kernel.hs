-- | Description : A simulated kernel: the Unix sockets, file descriptors and shared memory Sayland uses, for dejafu.
--
-- dejafu can only explore what happens in its own monad. A real socket call would block its whole scheduler, and it
-- could not try the interleavings that matter, like a peer's message arriving between two steps of this side.
-- So on this branch Sayland talks over sockets simulated with transactions, and so does everything else it asks of the
-- kernel: file descriptors passed with messages, and the memory of shm pools.
--
-- Each system call is one transaction, so it is one step for dejafu, like a real system call is atomic to the threads
-- that make it. Blocking calls ('accept', 'recv', and 'send' when the buffer is full) retry until they can go on.
-- Errors are t'IOError's with the type and errno a real kernel gives, so code that tells them apart keeps working.
--
-- Unlike a real kernel, this one notices memory being used after it was unmapped, or unmapped twice. Such a 'Fault'
-- is recorded in the kernel (see `faults`) and thrown in the thread that caused it, since on real hardware the
-- program could carry on with corrupted memory or crash.
module Sayland.Kernel (
  -- * The kernel
  Kernel,
  KernelConfig (..),
  defaultKernelConfig,
  newKernel,
  getEnv,
  writeStderr,
  faults,
  Fault (..),

  -- * Files
  doesFileExist,
  removeLink,

  -- * Unix stream sockets
  Socket,
  socketKernel,
  ShutdownCmd (..),
  socket,
  bind,
  listen,
  accept,
  connect,
  close,
  shutdown,
  send,
  recv,
  unreadBytes,

  -- * File descriptors
  memfd,
  setFdSize,
  fdSize,
  fdInode,
  pwrite,
  closeFd,
  openFds,

  -- * Shared memory
  mmap,
  munmap,
  peekBytes,
  pokeBytes,
  liveMappings,
) where

import Control.Exception (Exception (displayException))
import Control.Monad
import Control.Monad.Catch (throwM)
import Data.ByteString qualified as BS
import Data.Foldable (for_)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Sequence (Seq)
import Data.Sequence qualified as Seq
import Foreign (Ptr, WordPtr, ptrToWordPtr, wordPtrToPtr)
import Foreign.C (CInt)
import GHC.IO.Exception (IOErrorType (..), IOException (..))
import Sayland.Internal.Conc
import System.Posix.Types (Fd (Fd))
import Prelude

-- The kernel {{{

-- | The state of the simulated kernel. The compositor and its clients share one, like processes on one machine.
data Kernel = Kernel
  { environment :: Map String String
  , stderr :: String -> ConcIO ()
  , socketBuffer :: Maybe Int
  , filesystem :: TVar (Map FilePath Socket)
  -- ^ The socket files, by path. Nothing else Sayland uses lives in the filesystem.
  , descriptors :: TVar (Map Fd File)
  -- ^ The open file descriptors, of every process. Each one refers to a file.
  , nextDescriptor :: TVar CInt
  , nextFile :: TVar Int
  , mappings :: TVar (Map WordPtr Mapping)
  -- ^ The memory mappings that are live, by their address.
  , nextMapping :: TVar WordPtr
  , faultLog :: TVar [Fault]
  -- ^ The faults so far, newest first.
  }

-- | How to make a t'Kernel'.
data KernelConfig = KernelConfig
  { environment :: [(String, String)]
  -- ^ The environment variables every process sees, like @XDG_RUNTIME_DIR@ and @WAYLAND_DEBUG@.
  , stderr :: String -> ConcIO ()
  -- ^ What processes write to stderr goes here.
  , socketBuffer :: Maybe Int
  -- ^ How many bytes a socket buffers before a send to it blocks. `Nothing` for no limit.
  -- A real kernel has a limit, in the hundreds of kilobytes. A small one finds deadlocks that need full buffers.
  }

-- | A runtime directory, stderr thrown away, and sockets that buffer without limit.
defaultKernelConfig :: KernelConfig
defaultKernelConfig =
  KernelConfig
    { environment = [("XDG_RUNTIME_DIR", "/run/user/1000")]
    , stderr = const pass
    , socketBuffer = Nothing
    }

-- | A kernel with no sockets, files or mappings yet.
newKernel :: KernelConfig -> ConcIO Kernel
newKernel config =
  atomically $ do
    filesystem <- newTVarN "filesystem" mempty
    descriptors <- newTVarN "descriptors" mempty
    nextDescriptor <- newTVar 3 -- 0, 1 and 2 are stdin, stdout and stderr.
    nextFile <- newTVar 0
    mappings <- newTVarN "mappings" mempty
    nextMapping <- newTVar 0x7f0000000000
    faultLog <- newTVarN "faults" []
    pure
      Kernel
        { environment = Map.fromList config.environment
        , stderr = config.stderr
        , socketBuffer = config.socketBuffer
        , filesystem
        , descriptors
        , nextDescriptor
        , nextFile
        , mappings
        , nextMapping
        , faultLog
        }

-- | Look up an environment variable.
getEnv :: Kernel -> String -> ConcIO (Maybe String)
getEnv kernel name = pure $ Map.lookup name kernel.environment

-- | Write to stderr.
writeStderr :: Kernel -> String -> ConcIO ()
writeStderr kernel = kernel.stderr

-- | Memory used where it should not be. A real kernel would not notice, or only by crashing the program.
data Fault
  = -- | Memory was read or written outside every live mapping, like after it was unmapped.
    SegmentationFault WordPtr
  | -- | Memory was read or written past the end of the file it maps.
    BusError WordPtr
  | -- | @munmap@ of a range that is not exactly a live mapping: unmapped already, or with another length.
    -- A real kernel unmaps whatever is there, which could be memory mapped since.
    InvalidUnmap WordPtr Int
  deriving stock (Show, Eq)

instance Exception Fault where
  displayException = \case
    SegmentationFault addr -> "segmentation fault at " <> show addr
    BusError addr -> "bus error at " <> show addr
    InvalidUnmap addr len -> "munmap of " <> show len <> " bytes at " <> show addr <> ", which is not a live mapping"

-- | The faults so far, oldest first.
faults :: Kernel -> STM [Fault]
faults kernel = reverse <$> readTVar kernel.faultLog

-- | Record a fault and throw it.
fault :: Kernel -> Fault -> ConcIO a
fault kernel f = atomically (modifyTVar' kernel.faultLog (f :)) >> throwM f

-- | An t'IOError' like the one a system call failing with the given errno gives.
sysError :: IOErrorType -> CInt -> String -> String -> IOError
sysError ty errno call description = IOError Nothing ty call description (Just errno) Nothing

ebadf, einval, epipe, enoent, econnrefused, eaddrinuse :: String -> IOError
ebadf call = sysError InvalidArgument 9 call "Bad file descriptor"
einval call = sysError InvalidArgument 22 call "Invalid argument"
epipe call = sysError ResourceVanished 32 call "Broken pipe"
enoent call = sysError NoSuchThing 2 call "No such file or directory"
econnrefused call = sysError NoSuchThing 111 call "Connection refused"
eaddrinuse call = sysError ResourceBusy 98 call "Address already in use"

-- | Run a system call: a transaction that either fails with an error or gives a result.
syscall :: STM (Either IOError a) -> ConcIO a
syscall = atomically >=> either throwM pure

-- }}}

-- Files {{{

-- | Whether there is a file at the path.
doesFileExist :: Kernel -> FilePath -> ConcIO Bool
doesFileExist kernel path = Map.member path <$> readTVarIO kernel.filesystem

-- | Remove the file at the path. A socket bound to it stays as it is, but can no longer be connected to.
removeLink :: Kernel -> FilePath -> ConcIO ()
removeLink kernel path =
  syscall $
    stateTVar kernel.filesystem $ \files ->
      if Map.member path files then (Right (), Map.delete path files) else (Left (enoent "removeLink"), files)

-- }}}

-- Unix stream sockets {{{

-- | A Unix domain stream socket.
data Socket = Socket {kernel :: Kernel, state :: TVar SocketState}

-- | The kernel a socket belongs to.
socketKernel :: Socket -> Kernel
socketKernel sock = sock.kernel

data SocketState
  = Unconnected
  | Bound FilePath
  | -- | Listening, with the connections not accepted yet, oldest first.
    Listening FilePath (Seq Socket)
  | Connected Stream
  | Closed

-- | Both directions of a connection, from one end.
data Stream = Stream {input :: Pipe, output :: Pipe}

-- | One direction of a connection.
data Pipe = Pipe
  { segments :: TVar (Seq Segment)
  -- ^ What was sent and not received yet, oldest first.
  , writerOpen :: TVar Bool
  -- ^ False once the sending end shut down or closed. The receiver reads the rest, then the end of the stream.
  , readerOpen :: TVar Bool
  -- ^ False once the receiving end shut down or closed. Sending fails with @EPIPE@.
  }

-- | The bytes of one send, and the files sent along with them, which arrive with the first byte.
data Segment = Segment BS.ByteString [File]

-- | Which way of a socket to shut down.
data ShutdownCmd = ShutdownReceive | ShutdownSend | ShutdownBoth
  deriving stock (Show, Eq)

newPipe :: STM Pipe
newPipe = Pipe <$> newTVarN "pipe" Seq.empty <*> newTVar True <*> newTVar True

-- | A new socket, not bound or connected.
socket :: Kernel -> ConcIO Socket
socket kernel = Socket kernel <$> atomically (newTVarN "socket" Unconnected)

-- | Bind a socket to a path, making a socket file there.
bind :: Socket -> FilePath -> ConcIO ()
bind sock path = syscall $ do
  files <- readTVar sock.kernel.filesystem
  readTVar sock.state >>= \case
    _ | Map.member path files -> pure . Left $ eaddrinuse "bind"
    Unconnected -> do
      writeTVar sock.state (Bound path)
      writeTVar sock.kernel.filesystem (Map.insert path sock files)
      pure $ Right ()
    Closed -> pure . Left $ ebadf "bind"
    _ -> pure . Left $ einval "bind"

-- | Listen for connections on a bound socket. Unlike a real one, the backlog has no limit.
listen :: Socket -> Int -> ConcIO ()
listen sock _backlog =
  syscall $
    readTVar sock.state >>= \case
      Bound path -> Right () <$ writeTVar sock.state (Listening path Seq.empty)
      Listening{} -> pure $ Right ()
      Closed -> pure . Left $ ebadf "listen"
      _ -> pure . Left $ einval "listen"

-- | Accept the oldest connection, blocking until there is one.
-- Closing the socket while a thread waits here makes it fail with @EBADF@, like GHC's IO manager does.
accept :: Socket -> ConcIO Socket
accept sock =
  syscall $
    readTVar sock.state >>= \case
      Listening path pending -> case pending of
        Seq.Empty -> retry
        next Seq.:<| rest -> Right next <$ writeTVar sock.state (Listening path rest)
      Closed -> pure . Left $ ebadf "accept"
      _ -> pure . Left $ einval "accept"

-- | Connect to the socket listening at a path.
connect :: Socket -> FilePath -> ConcIO ()
connect sock path = syscall $ do
  files <- readTVar sock.kernel.filesystem
  local <- readTVar sock.state
  case (local, Map.lookup path files) of
    (Closed, _) -> pure . Left $ ebadf "connect"
    (Unconnected, Nothing) -> pure . Left $ enoent "connect"
    (Unconnected, Just listener) ->
      readTVar listener.state >>= \case
        Listening listenPath pending -> do
          toServer <- newPipe
          toClient <- newPipe
          server <- Socket sock.kernel <$> newTVarN "socket" (Connected Stream{input = toServer, output = toClient})
          writeTVar listener.state (Listening listenPath (pending Seq.|> server))
          writeTVar sock.state (Connected Stream{input = toClient, output = toServer})
          pure $ Right ()
        _ -> pure . Left $ econnrefused "connect"
    _ -> pure . Left $ einval "connect"

-- | Close a socket. Closing it again does nothing, like the network package's close.
--
-- The peer of a connected socket reads what was sent, then the end of the stream, and its sends fail with @EPIPE@.
-- A thread of this side blocked on the socket fails with @EBADF@. The connections a listening socket had not accepted
-- yet are closed too.
close :: Socket -> ConcIO ()
close sock = atomically $ closeSTM sock

closeSTM :: Socket -> STM ()
closeSTM sock = do
  readTVar sock.state >>= \case
    Connected stream -> do
      writeTVar stream.output.writerOpen False
      writeTVar stream.input.readerOpen False
      writeTVar stream.input.segments Seq.empty
    Listening _ pending -> for_ pending closeSTM
    _ -> pass
  writeTVar sock.state Closed

-- | Shut down receiving, sending or both on a connected socket.
-- Shutting down sending makes the peer read the end of the stream, shutting down receiving makes its sends fail.
shutdown :: Socket -> ShutdownCmd -> ConcIO ()
shutdown sock how =
  syscall $
    readTVar sock.state >>= \case
      Connected stream -> do
        when (how /= ShutdownReceive) $ writeTVar stream.output.writerOpen False
        when (how /= ShutdownSend) $ writeTVar stream.input.readerOpen False
        pure $ Right ()
      Closed -> pure . Left $ ebadf "shutdown"
      _ -> pure . Left $ sysError InvalidArgument 107 "shutdown" "Transport endpoint is not connected"

-- | Send all the bytes, with the file descriptors along with them, like @sendmsg@ with @SCM_RIGHTS@.
-- The peer gets its own descriptors for the same files.
--
-- With a limit on the socket buffer, blocks while sending would go over it. Whatever the limit, one send that fits in
-- an empty buffer always goes through whole, so messages are never split.
send :: Socket -> BS.ByteString -> [Fd] -> ConcIO ()
send sock bytes fds =
  syscall $
    readTVar sock.state >>= \case
      Connected stream -> do
        writable <- (&&) <$> readTVar stream.output.writerOpen <*> readTVar stream.output.readerOpen
        table <- readTVar sock.kernel.descriptors
        queued <- readTVar stream.output.segments
        let buffered = sum $ (\(Segment b _) -> BS.length b) <$> queued
            full = maybe False (\limit -> not (Seq.null queued) && buffered + BS.length bytes > limit) sock.kernel.socketBuffer
        case traverse (`Map.lookup` table) fds of
          _ | not writable -> pure . Left $ epipe "send"
          Nothing -> pure . Left $ ebadf "send"
          Just files
            | full -> retry
            | otherwise -> Right () <$ writeTVar stream.output.segments (queued Seq.|> Segment bytes files)
      Closed -> pure . Left $ ebadf "send"
      _ -> pure . Left $ sysError InvalidArgument 107 "send" "Transport endpoint is not connected"

-- | Receive up to the given number of bytes, and the file descriptors sent along with them, blocking until there is
-- something to receive. An empty result is the end of the stream.
--
-- Like Linux, a receive does not go past the start of a send that carries file descriptors, unless it starts there.
recv :: Socket -> Int -> ConcIO (BS.ByteString, [Fd])
recv sock size =
  syscall $
    readTVar sock.state >>= \case
      Connected stream ->
        readTVar stream.input.segments >>= \case
          Seq.Empty -> do
            open <- (&&) <$> readTVar stream.input.writerOpen <*> readTVar stream.input.readerOpen
            if open then retry else pure $ Right ("", [])
          Segment first files Seq.:<| rest -> do
            let (bytes, remaining) = takeBytes size (Segment first [] Seq.<| rest)
            writeTVar stream.input.segments remaining
            installed <- traverse (install sock.kernel) files
            pure $ Right (bytes, installed)
      Closed -> pure . Left $ ebadf "recv"
      _ -> pure . Left $ sysError InvalidArgument 107 "recv" "Transport endpoint is not connected"
  where
    -- Take bytes from the front, stopping before a segment with files.
    takeBytes :: Int -> Seq Segment -> (BS.ByteString, Seq Segment)
    takeBytes n = \case
      Segment bytes [] Seq.:<| rest
        | n > 0 && not (BS.null bytes) ->
            let (taken, left) = BS.splitAt n bytes
                rest' = if BS.null left then rest else Segment left [] Seq.<| rest
                (more, remaining) = if BS.null left then takeBytes (n - BS.length taken) rest else ("", rest')
             in (taken <> more, remaining)
        | BS.null bytes -> takeBytes n rest
      segments -> ("", segments)

-- | How many bytes were sent to a connected socket and not received yet.
unreadBytes :: Socket -> STM Int
unreadBytes sock =
  readTVar sock.state >>= \case
    Connected stream -> sum . fmap (\(Segment b _) -> BS.length b) <$> readTVar stream.input.segments
    _ -> pure 0

-- }}}

-- File descriptors {{{

-- | A file in memory, like one made with @memfd_create@.
data File = File {fileNumber :: Int, contents :: TVar BS.ByteString}

-- | A new file descriptor for a file.
install :: Kernel -> File -> STM Fd
install kernel file = do
  n <- readTVar kernel.nextDescriptor
  writeTVar kernel.nextDescriptor (n + 1)
  modifyTVar' kernel.descriptors $ Map.insert (Fd n) file
  pure (Fd n)

-- | The file a descriptor refers to.
lookupFd :: Kernel -> String -> Fd -> STM (Either IOError File)
lookupFd kernel call fd = maybe (Left $ ebadf call) Right . Map.lookup fd <$> readTVar kernel.descriptors

-- | Make an empty file in memory, and a descriptor for it. Like @memfd_create@, or @shm_open@ then @shm_unlink@.
memfd :: Kernel -> ConcIO Fd
memfd kernel = atomically $ do
  n <- readTVar kernel.nextFile
  writeTVar kernel.nextFile (n + 1)
  file <- File n <$> newTVarN ("file-" <> show n) BS.empty
  install kernel file

-- | Change the size of a file, padding it with zero bytes or cutting it. Like @ftruncate@.
setFdSize :: Kernel -> Fd -> Int -> ConcIO ()
setFdSize kernel fd size =
  syscall $
    lookupFd kernel "setFdSize" fd >>= \case
      _ | size < 0 -> pure . Left $ einval "setFdSize"
      Left e -> pure $ Left e
      Right file -> fmap Right . modifyTVar' file.contents $ \bytes ->
        BS.take size bytes <> BS.replicate (size - BS.length bytes) 0

-- | The size of the file a descriptor refers to.
fdSize :: Kernel -> Fd -> ConcIO Int
fdSize kernel fd = syscall $ lookupFd kernel "fstat" fd >>= traverse (fmap BS.length . readTVar . (.contents))

-- | Write bytes to a file at an offset, growing it if they go past its end. Like @pwrite@.
pwrite :: Kernel -> Fd -> Int -> BS.ByteString -> ConcIO ()
pwrite kernel fd offset bytes =
  syscall $
    lookupFd kernel "pwrite" fd >>= \case
      _ | offset < 0 -> pure . Left $ einval "pwrite"
      Left e -> pure $ Left e
      Right file -> fmap Right . modifyTVar' file.contents $ \old ->
        let padded = old <> BS.replicate (offset - BS.length old) 0
         in BS.take offset padded <> bytes <> BS.drop (offset + BS.length bytes) padded

-- | The number of the file a descriptor refers to, which no other file has. Like the inode @fstat@ gives.
fdInode :: Kernel -> Fd -> ConcIO Int
fdInode kernel fd = syscall $ fmap (.fileNumber) <$> lookupFd kernel "fstat" fd

-- | Close a file descriptor. The file stays as long as another descriptor or a mapping refers to it.
closeFd :: Kernel -> Fd -> ConcIO ()
closeFd kernel fd =
  syscall $
    stateTVar kernel.descriptors $ \table ->
      if Map.member fd table then (Right (), Map.delete fd table) else (Left (ebadf "closeFd"), table)

-- | The open file descriptors.
openFds :: Kernel -> STM [Fd]
openFds kernel = Map.keys <$> readTVar kernel.descriptors

-- }}}

-- Shared memory {{{

-- | A live memory mapping of a file.
data Mapping = Mapping {file :: File, size :: Int}

-- | Map a file into memory, shared, from its start. Like @mmap@ with @MAP_SHARED@, readable and writable.
-- Each mapping gets an address no mapping had before, so memory used after it is unmapped is never mapped again.
mmap :: Kernel -> Fd -> Int -> ConcIO (Ptr ())
mmap kernel fd size =
  syscall $
    lookupFd kernel "mmap" fd >>= \case
      _ | size <= 0 -> pure . Left $ einval "mmap"
      Left e -> pure $ Left e
      Right file -> do
        addr <- readTVar kernel.nextMapping
        writeTVar kernel.nextMapping (addr + 0x100000000) -- Far enough apart that an offset into one never reaches the next.
        modifyTVar' kernel.mappings $ Map.insert addr Mapping{file, size}
        pure . Right $ wordPtrToPtr addr

-- | Unmap memory mapped by `mmap`. Unmapping anything but exactly a live mapping is an `InvalidUnmap` fault.
munmap :: Kernel -> Ptr () -> Int -> ConcIO ()
munmap kernel ptr size = do
  let addr = ptrToWordPtr ptr
  unmapped <- atomically . stateTVar kernel.mappings $ \live -> case Map.lookup addr live of
    Just mapping | mapping.size == size -> (True, Map.delete addr live)
    _ -> (False, live)
  unless unmapped . fault kernel $ InvalidUnmap addr size

-- | The live mapping a range of memory is in, with the offset of its start in the mapping.
-- A `SegmentationFault` if the range is not in one mapping, a `BusError` if it is past the end of the mapped file.
mapped :: Kernel -> Ptr () -> Int -> STM (Either Fault (File, Int))
mapped kernel ptr len = do
  let addr = ptrToWordPtr ptr
  live <- readTVar kernel.mappings
  case Map.lookupLE addr live of
    Just (start, mapping)
      | addr + fromIntegral len <= start + fromIntegral mapping.size -> do
          let offset = fromIntegral (addr - start)
          fileSize <- BS.length <$> readTVar mapping.file.contents
          pure $ if offset + len <= fileSize then Right (mapping.file, offset) else Left (BusError addr)
    _ -> pure . Left $ SegmentationFault addr

-- | Read bytes from mapped memory.
peekBytes :: Kernel -> Ptr () -> Int -> ConcIO BS.ByteString
peekBytes kernel ptr len =
  atomically (mapped kernel ptr len) >>= \case
    Left f -> fault kernel f
    Right (file, offset) -> BS.take len . BS.drop offset <$> readTVarIO file.contents

-- | Write bytes to mapped memory.
pokeBytes :: Kernel -> Ptr () -> BS.ByteString -> ConcIO ()
pokeBytes kernel ptr bytes = do
  let len = BS.length bytes
  written <-
    atomically $
      mapped kernel ptr len >>= \case
        Left f -> pure $ Just f
        Right (file, offset) -> Nothing <$ modifyTVar' file.contents (\old -> BS.take offset old <> bytes <> BS.drop (offset + len) old)
  for_ written $ fault kernel

-- | The live mappings: their address and size.
liveMappings :: Kernel -> STM [(Ptr (), Int)]
liveMappings kernel = fmap (\(addr, m) -> (wordPtrToPtr addr, m.size)) . Map.toList <$> readTVar kernel.mappings

-- }}}

-- vim: foldmethod=marker
