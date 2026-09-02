module Main (main) where

import Control.Concurrent (forkIO)
import Control.Concurrent.STM (newTQueue, writeTMVar)
import Control.Exception (bracket, finally)
import Data.Bimap qualified as BM
import Data.ByteString (hPut, pack)
import Data.Map qualified as Map
import Data.Maybe (fromJust)
import GHC.IO.Handle
import Network.Socket (Family (AF_UNIX), SockAddr (SockAddrUnix), SocketType (Stream), close, connect, defaultProtocol, socket)
import Relude hiding (hFlush)
import Sayland
import System.Posix (ShmOpenFlags (ShmOpenFlags), fdToHandle, ownerReadMode, ownerWriteMode, setFdSize, shmOpen, shmUnlink, unionFileModes)
import System.Random (randomIO)

interfaceTable :: InterfaceClientTable
interfaceTable = waylandInterfaceClientTable <> xdg_shellInterfaceClientTable

versionTable :: VersionTable
versionTable = waylandVersionTable <> xdg_shellVersionTable

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
          q <- atomically newTQueue
          pure $ ClientEnv $ ClientEnvironment sock counter objects globals interfaceTable' versionTable' handlers q
        Nothing -> error "couldn't find `$WAYLAND_DISPLAY`, nor any open socket."

program :: Wayland Client ()
program = do
  ClientEnv env <- ask
  running :: MVar () <- newEmptyMVar

  display <- fromJust <$> getInterface' @Wl_display 1
  registryId :: TObjectID Wl_registry <- TObjectID <$> newObjectId
  runRequest display $ Request_wl_display_get_registry registryId
  registry <- fromJust <$> getInterface registryId

  liftIO
    . void
    . forkIO
    $ finally
      (putStrLn "\n--- Starting event loop ---" >> runReaderT (clientLoop env.socket) (ClientEnv env))
      (close env.socket >> putMVar running ())

  putStrLn "Binding to required interfaces..."
  wlShmId :: TObjectID Wl_shm <- TObjectID . fromJust <$> bindToInterface registry "wl_shm"
  wl_shm <- fromJust <$> getInterface wlShmId

  wlCompositorId :: TObjectID Wl_compositor <- TObjectID . fromJust <$> bindToInterface registry "wl_compositor"
  wl_compositor <- fromJust <$> getInterface wlCompositorId

  wlSurfaceId :: TObjectID Wl_surface <- TObjectID <$> newObjectId
  runRequest wl_compositor $ Request_wl_compositor_create_surface wlSurfaceId
  surface' <- fromJust <$> getInterface wlSurfaceId

  xdgWmBaseId :: TObjectID Xdg_wm_base <- TObjectID . fromJust <$> bindToInterface registry "xdg_wm_base"
  xdg_wm_base <- fromJust <$> getInterface xdgWmBaseId

  xdgSurfaceId :: TObjectID Xdg_surface <- TObjectID <$> newObjectId
  runRequest xdg_wm_base $ Request_xdg_wm_base_get_xdg_surface xdgSurfaceId wlSurfaceId
  xdg_surface <- fromJust <$> getInterface xdgSurfaceId

  xdgToplevelId :: TObjectID Xdg_toplevel <- TObjectID <$> newObjectId
  runRequest xdg_surface $ Request_xdg_surface_get_toplevel xdgToplevelId

  configured <- liftIO newEmptyTMVarIO
  modifyIORef env.eventHandlers $ (:) $ EventHandler $ \_oid -> \case
    (Event_xdg_surface_configure _) -> do
      atomically $ writeTMVar configured ()
  runRequest surface' Request_wl_surface_commit
  liftIO . atomically $ takeTMVar configured
  bufferWidth <- liftIO $ newIORef 512
  bufferHeight <- liftIO $ newIORef 512
  shm_pool_rand :: Int <- randomIO
  let colorChannels :: Int = 4
  let
    makeSharedMemoryObject = shmOpen ("basic-window" <> show shm_pool_rand) (ShmOpenFlags True True False True) (Relude.foldl' unionFileModes ownerWriteMode [ownerReadMode])
    useSharedMemoryObject fileDescriptor =
      usingReaderT (ClientEnv env) $ do
        bw <- liftIO $ readIORef bufferWidth
        bh <- liftIO $ readIORef bufferHeight
        let frameSize = bw * bh * colorChannels
        liftIO . setFdSize fileDescriptor $ fromIntegral frameSize
        wlShmPoolId :: TObjectID Wl_shm_pool <- TObjectID <$> newObjectId
        runRequest wl_shm $ Request_wl_shm_create_pool wlShmPoolId fileDescriptor frameSize
        wl_shm_pool <- fromJust <$> getInterface wlShmPoolId
        wlBufferId :: TObjectID Wl_buffer <- TObjectID <$> newObjectId
        runRequest wl_shm_pool $ Request_wl_shm_pool_create_buffer wlBufferId 0 bw bh (bw * colorChannels) Enum_wl_shm_format_argb8888
        fileHandle <- liftIO $ fdToHandle fileDescriptor

        liftIO $ hPut fileHandle $ image bw bh
        liftIO $ hFlush fileHandle
        runRequest surface' $ Request_wl_surface_attach wlBufferId 0 0
        runRequest surface' Request_wl_surface_commit
        -- Wait for exit
        takeMVar running

  liftIO . void $ bracket makeSharedMemoryObject (const $ shmUnlink $ "basic-window" <> show shm_pool_rand) useSharedMemoryObject

-- | Rainbow image :D
image :: Int -> Int -> ByteString
image bufferWidth bufferHeight =
  generateBGRA8 $ \x y ->
    let tx = fi x / fi @Int (fi bufferWidth - 1) :: Double
        ty = fi y / fi @Int (fi bufferHeight - 1) :: Double
        b = round $ tx * 255 -- left -> right
        g = round $ ty * 255 -- top -> bottom
        r = round $ (1 - tx) * 255 -- right -> left
        a = round $ (1 - ty) * 255 -- bottom -> top
     in (b, g, r, a)
  where
    fi :: forall a b. (Integral a, Num b) => a -> b
    fi = fromIntegral
    generateBGRA8 :: (Int -> Int -> (Word8, Word8, Word8, Word8)) -> ByteString
    generateBGRA8 pixelFn =
      pack
        [ byte
        | y <- [0 .. fi bufferHeight - 1]
        , x <- [0 .. fi bufferWidth - 1]
        , let (b, g, r, a) = pixelFn x y
        , byte <- [b, g, r, a]
        ]
