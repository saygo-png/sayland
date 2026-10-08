-- | Description : A client that opens an xdg toplevel and draws a rainbow into it.
module Main (main) where

import Control.Concurrent (forkIO, myThreadId)
import Control.Concurrent.STM (writeTMVar)
import Control.Exception (bracket, finally, handle, throwTo)
import Data.ByteString (hPut)
import DemoUtils
import GHC.IO.Handle
import Network.Socket (close)
import Relude hiding (hFlush)
import Sayland
import Sayland.Wire
import System.Posix (ShmOpenFlags (ShmOpenFlags), fdToHandle, ownerReadMode, ownerWriteMode, setFdSize, shmOpen, shmUnlink, unionFileModes)
import System.Random (randomIO)

table :: ProtocolTable
table = waylandTable <> xdg_shellTable

main :: IO ()
main = runReaderT program =<< waylandSetup table

program :: Wayland Client ()
program = do
  ClientEnv env <- ask
  running :: MVar () <- newEmptyMVar

  display <- getWlDisplay
  registry <- newObject display Request_wl_display_get_registry

  -- A crash in the event loop has to reach the main thread. Otherwise the
  -- `finally` below just fills `running` and the daemon exits successfully despite errors.
  mainThread <- liftIO myThreadId
  let rethrow :: SomeException -> IO ()
      rethrow = throwTo mainThread

  liftIO
    . void
    . forkIO
    $ finally
      (handle rethrow $ putStrLn "\n--- Starting event loop ---" >> runReaderT (clientLoop env.socket) (ClientEnv env))
      (close env.socket >> putMVar running ())

  -- Round trip, so the registry has advertised its globals before binding to them.
  callback <- newObject display Request_wl_display_sync
  takeMVar callback.done

  putStrLn "Binding to required interfaces..."
  wl_shm <- bindToInterface @Wl_shm registry
  wl_compositor <- bindToInterface @Wl_compositor registry
  xdg_wm_base <- bindToInterface @Xdg_wm_base registry

  surface <- newObject wl_compositor Request_wl_compositor_create_surface
  xdg_surface <- newObject xdg_wm_base $ \i -> Request_xdg_wm_base_get_xdg_surface i surface.wlid
  _xdg_toplevel <- newObject xdg_surface Request_xdg_surface_get_toplevel

  configured <- liftIO newEmptyTMVarIO
  modifyIORef env.eventHandlers $ (:) $ EventHandler $ \_oid -> \case
    (Event_xdg_surface_configure _) -> do
      atomically $ writeTMVar configured ()
  sendMsg surface Request_wl_surface_commit
  atomically $ takeTMVar configured
  bufferWidth <- newIORef 512
  bufferHeight <- newIORef 512
  shm_pool_rand :: Int <- randomIO
  let colorChannels :: Int32 = 4
  let
    makeSharedMemoryObject = shmOpen ("basic-window" <> show shm_pool_rand) (ShmOpenFlags True True False True) (Relude.foldl' unionFileModes ownerWriteMode [ownerReadMode])
    useSharedMemoryObject fileDescriptor =
      usingReaderT (ClientEnv env) $ do
        bw <- readIORef bufferWidth
        bh <- readIORef bufferHeight
        let frameSize = bw * bh * colorChannels
        liftIO . setFdSize fileDescriptor $ fromIntegral frameSize
        wl_shm_pool <- newObject wl_shm $ \i -> Request_wl_shm_create_pool i (c fileDescriptor) (c frameSize)
        wl_buffer <- newObject wl_shm_pool $ \i -> Request_wl_shm_pool_create_buffer i 0 (c bw) (c bh) (c (bw * colorChannels)) Enum_wl_shm_format_argb8888
        fileHandle <- liftIO $ fdToHandle fileDescriptor

        liftIO $ hPut fileHandle $ rainbowImage bw bh
        liftIO $ hFlush fileHandle
        sendMsg surface $ Request_wl_surface_attach (Just wl_buffer.wlid) 0 0
        sendMsg surface Request_wl_surface_commit
        -- Wait for exit
        takeMVar running

  liftIO . void $ bracket makeSharedMemoryObject (const $ shmUnlink $ "basic-window" <> show shm_pool_rand) useSharedMemoryObject
