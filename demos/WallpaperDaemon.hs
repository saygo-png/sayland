{-# LANGUAGE QuasiQuotes #-}

{- HLINT ignore "Use camelCase" -}

-- | Description : A client that draws a rainbow wallpaper with wlr-layer-shell.
module Main (main) where

import Control.Concurrent.STM (writeTMVar)
import Control.Exception
import Data.ByteString (hPut)
import DemoUtils
import Relude
import Sayland
import System.Posix

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

main :: IO ()
main = withWaylandClient table program

program :: Wayland Client ()
program = do
  waylandEnv@(ClientEnv env) <- ask
  serial :: TMVar Word32 <- newEmptyTMVarIO

  display <- getWlDisplay
  registry <- newObject display Request_wl_display_get_registry

  -- Round trip, so the registry has advertised its globals before binding to them.
  roundtrip

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

  let makeSharedMemoryObject = shmOpen poolName (ShmOpenFlags True True False True) (foldl' unionFileModes ownerWriteMode [ownerReadMode])
      useSharedMemoryObject fileDescriptor =
        usingReaderT waylandEnv $ do
          let frameSize = bufferWidth * bufferHeight * colorChannels
          liftIO . setFdSize fileDescriptor $ fromIntegral frameSize
          wl_shm_pool <- newObject wl_shm $ \i -> Request_wl_shm_create_pool i fileDescriptor frameSize
          wl_buffer <- newObject wl_shm_pool $ \i -> Request_wl_shm_pool_create_buffer i 0 bufferWidth bufferHeight (bufferWidth * colorChannels) colorFormat

          fileHandle <- liftIO $ fdToHandle fileDescriptor

          liftIO $ hPut fileHandle (rainbowImage bufferWidth bufferHeight)
          hFlush fileHandle
          sendMsg surface $ Request_wl_surface_attach (Just wl_buffer.wlid) 0 0
          sendMsg surface Request_wl_surface_commit

          -- Keep the wallpaper up until the compositor goes away.
          untilDisconnected

  liftIO . void $ bracket makeSharedMemoryObject (const $ shmUnlink poolName) useSharedMemoryObject
