{- HLINT ignore "Use camelCase" -}
module Main (main) where

import Config
import Control.Concurrent (forkIO)
import Control.Concurrent.STM (newTQueue)
import Control.Exception
import Data.Bimap qualified as BM
import Data.ByteString.Lazy hiding (singleton)
import Data.Map qualified as Map
import Data.Maybe (fromJust)
import Network.Socket hiding (openSocket)
import Protocol (InterfaceClientTable, VersionTable)
import Relude hiding (ByteString, get, isPrefixOf, put)
import Saywayland.Protocols.Wayland
import Saywayland.Protocols.WlrLayerShell
import Saywayland.Types
import Saywayland.WaylandSocket
import System.Posix (ownerReadMode, ownerWriteMode, setFdSize, unionFileModes)
import System.Posix.IO
import System.Posix.SharedMem

interfaceTable :: InterfaceClientTable
interfaceTable = waylandInterfaceClientTable <> wlr_layer_shell_unstable_v1InterfaceClientTable

versionTable :: VersionTable
versionTable = waylandVersionTable <> wlr_layer_shell_unstable_v1VersionTable

main :: IO ()
main = do
  runReaderT program =<< waylandSetup
  where
    waylandSetup = do
      let display :: Interface Client = Interface $ Wl_display $ TObjectID wlDisplayID
      getSocketPath openSocket >>= \case
        Just path -> do
          putStrLn $ "using socket path: " <> show path
          sock <- socket AF_UNIX Stream defaultProtocol
          connect sock $ SockAddrUnix path
          counter <- newIORef wlDisplayID
          objects <- newIORef $ Map.fromList [(wlDisplayID, display)]
          globals <- newIORef BM.empty
          handlers <- newIORef mempty
          interfaceTable' <- newIORef $ Map.fromList interfaceTable
          versionTable' <- newIORef $ Map.fromList versionTable
          fdqueue <- atomically newTQueue
          pure $ ClientEnv $ ClientEnvironment sock counter objects globals interfaceTable' versionTable' handlers fdqueue
        Nothing -> error "couldn't find `$WAYLAND_DISPLAY`, nor any open socket."

program :: Wayland Client ()
program = do
  ClientEnv env <- ask
  serial :: TMVar Word32 <- newEmptyTMVarIO
  running :: MVar () <- newEmptyMVar

  display <- fromJust <$> getInterface' @Wl_display 1
  registryId <- TObjectID <$> newObjectId
  runRequest display $ Request_wl_display_get_registry registryId
  registry <- fromJust <$> getInterface registryId

  liftIO
    . void
    . forkIO
    $ finally
      (putStrLn "\n--- Starting event loop ---" >> runReaderT (clientLoop env.socket) (ClientEnv env))
      (close env.socket >> putMVar running ())

  putStrLn "Binding to required interfaces..."
  wlShmId <- fromJust <$> bindToInterface registry "wl_shm"
  wl_shm <- fromJust <$> getInterface' @Wl_shm wlShmId

  wlCompositorId <- TObjectID . fromJust <$> bindToInterface registry "wl_compositor"
  wl_compositor :: Wl_compositor <- fromJust <$> getInterface wlCompositorId

  zwlrLayerShellV1Id <- TObjectID . fromJust <$> bindToInterface registry "zwlr_layer_shell_v1"
  zwlr_layer_shell_V1 :: Zwlr_layer_shell_v1 <- fromJust <$> getInterface zwlrLayerShellV1Id

  modifyIORef env.eventHandlers $ (:) $ EventHandler $ \_oid -> \case
    (Event_zwlr_layer_surface_v1_configure receivedSerial _ _) -> do
      atomically $ putTMVar serial receivedSerial
    _ -> pass

  wlSurfaceId <- TObjectID <$> newObjectId
  runRequest wl_compositor $ Request_wl_compositor_create_surface wlSurfaceId
  surface' <- fromJust <$> getInterface wlSurfaceId

  layerSurfaceId <- TObjectID <$> newObjectId
  runRequest zwlr_layer_shell_V1 $ Request_zwlr_layer_shell_v1_get_layer_surface layerSurfaceId wlSurfaceId 0 Enum_zwlr_layer_shell_v1_layer_background "wallpaper"

  zwlrLayerSurfaceV1 <- fromJust <$> getInterface layerSurfaceId
  runRequest zwlrLayerSurfaceV1 $ Request_zwlr_layer_surface_v1_set_size (fromIntegral bufferWidth) (fromIntegral bufferHeight)
  runRequest zwlrLayerSurfaceV1 $ Request_zwlr_layer_surface_v1_set_exclusive_zone $ -1

  runRequest surface' Request_wl_surface_commit
  atomically (takeTMVar serial) >>= runRequest zwlrLayerSurfaceV1 . Request_zwlr_layer_surface_v1_ack_configure

  let makeSharedMemoryObject = shmOpen poolName (ShmOpenFlags True True False True) (Relude.foldl' unionFileModes ownerWriteMode [ownerReadMode])
      useSharedMemoryObject fileDescriptor =
        usingReaderT (ClientEnv env) $ do
          let frameSize = bufferWidth * bufferHeight * colorChannels
          liftIO . setFdSize fileDescriptor $ fromIntegral frameSize
          wlShmPoolId <- TObjectID <$> newObjectId
          runRequest wl_shm $ Request_wl_shm_create_pool wlShmPoolId fileDescriptor frameSize
          wl_shm_pool <- fromJust <$> getInterface wlShmPoolId
          wlBufferId <- TObjectID <$> newObjectId
          runRequest wl_shm_pool $ Request_wl_shm_pool_create_buffer wlBufferId 0 bufferWidth bufferHeight (bufferWidth * colorChannels) colorFormat

          fileHandle <- liftIO $ fdToHandle fileDescriptor

          liftIO $ hPut fileHandle image
          runRequest surface' $ Request_wl_surface_attach wlBufferId 0 0
          runRequest surface' Request_wl_surface_commit

          -- Wait for exit
          takeMVar running

  liftIO . void $ bracket makeSharedMemoryObject (const $ shmUnlink poolName) useSharedMemoryObject
