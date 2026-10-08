{-# LANGUAGE QuasiQuotes #-}

{- HLINT ignore "Use camelCase" -}
module Client.WallpaperDaemon (test) where

import Control.Concurrent (forkIO, myThreadId)
import Control.Concurrent.STM (writeTMVar)
import Control.Exception
import Data.ByteString (hPut)
import Network.Socket
import Relude hiding (ByteString, get, isPrefixOf, put)
import Sayland
import Sayland.Wire
import System.Posix (ownerReadMode, ownerWriteMode, setFdSize, unionFileModes)
import System.Posix.IO
import System.Posix.SharedMem
import System.Timeout (timeout)
import Test.Tasty.HUnit
import TestUtils

bufferWidth, bufferHeight :: Int32
bufferWidth = 1920
bufferHeight = 1080

poolName :: String
poolName = "saywallpaper-shared-pool"

colorFormat :: Enum_wl_shm_format
colorFormat = Enum_wl_shm_format_argb8888

colorChannels :: Int32
colorChannels = 4

table :: ProtocolTable
table = waylandTable <> wlr_layer_shell_unstable_v1Table

test :: Assertion
test = main

main :: IO ()
main = void . timeout 3_000_000 $ runReaderT program =<< waylandSetup table

program :: Wayland Client ()
program = do
  ClientEnv env <- ask
  serial :: TMVar WlUInt <- newEmptyTMVarIO
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
  zwlr_layer_shell_v1 <- bindToInterface @Zwlr_layer_shell_v1 registry

  modifyIORef env.eventHandlers $ (:) $ EventHandler $ \_oid -> \case
    (Event_zwlr_layer_surface_v1_configure receivedSerial _ _) -> do
      atomically $ writeTMVar serial receivedSerial
    _ -> pass

  surface <- newObject wl_compositor Request_wl_compositor_create_surface
  layer_surface <- newObject zwlr_layer_shell_v1 $ \i ->
    Request_zwlr_layer_shell_v1_get_layer_surface i surface.wlid Nothing Enum_zwlr_layer_shell_v1_layer_background [wl|wallpaper|]

  sendMsg layer_surface $ Request_zwlr_layer_surface_v1_set_size (fromIntegral bufferWidth) (fromIntegral bufferHeight)
  sendMsg layer_surface $ Request_zwlr_layer_surface_v1_set_exclusive_zone $ -1

  sendMsg surface Request_wl_surface_commit
  atomically (takeTMVar serial) >>= sendMsg layer_surface . Request_zwlr_layer_surface_v1_ack_configure

  let makeSharedMemoryObject = shmOpen poolName (ShmOpenFlags True True False True) (Relude.foldl' unionFileModes ownerWriteMode [ownerReadMode])
      useSharedMemoryObject fileDescriptor =
        usingReaderT (ClientEnv env) $ do
          let frameSize = bufferWidth * bufferHeight * colorChannels
          liftIO . setFdSize fileDescriptor $ fromIntegral frameSize
          wl_shm_pool <- newObject wl_shm $ \i -> Request_wl_shm_create_pool i (c fileDescriptor) (c frameSize)
          wl_buffer <- newObject wl_shm_pool $ \i -> Request_wl_shm_pool_create_buffer i 0 (c bufferWidth) (c bufferHeight) (c (bufferWidth * colorChannels)) colorFormat

          fileHandle <- liftIO $ fdToHandle fileDescriptor

          liftIO $ hPut fileHandle (rainbowImage bufferWidth bufferHeight)
          hFlush fileHandle
          sendMsg surface $ Request_wl_surface_attach (Just wl_buffer.wlid) 0 0
          sendMsg surface Request_wl_surface_commit

          -- Wait for exit
          takeMVar running

  liftIO . void $ bracket makeSharedMemoryObject (const $ shmUnlink poolName) useSharedMemoryObject
