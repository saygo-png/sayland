{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Saywayland.Protocols.Wayland where

-- this module imlements some interfaces using classes defined by the `Protocol` module.

import Control.Concurrent (threadDelay)
import Control.Exception (try)
import Control.Lens (makeFieldsId, (.~))
import Data.Bimap qualified as BM
import Data.Binary.Put (runPut)
import Data.ByteString qualified as BS
import Data.ByteString.Char8 qualified as BS8
import Data.Char (toUpper)
import Data.Map qualified as Map
import Data.Maybe (fromJust)
import Data.Sequence qualified as Seq
import Debug.Trace (traceIO)
import Foreign (Ptr, nullPtr, withForeignPtr)
import MMAP (mapShared, mkMmapFlags, mmap, munmap, protRead, protWrite)
import Protocol
import Relude hiding (get)
import Saywayland.Types
import System.Posix (Fd, setFdSize)

-- TemplateHaskell Definitions {{{
$(loadProtocolFile False "protocols/wayland.xml")

-- }}}

-- Interfaces {{{
data WL_display = WL_display {wlid :: Word32}

makeFieldsId ''WL_display

data WL_registry = WL_registry {wlid :: Word32}

makeFieldsId ''WL_registry

data WL_callback = WL_callback {wlid :: Word32, done :: MVar ()}

makeFieldsId ''WL_callback

data WL_compositor = WL_compositor {wlid :: Word32}

makeFieldsId ''WL_compositor

data WL_shm_pool = WL_shm_pool {wlid :: Word32, fd :: Fd, size :: IORef Int, ptr :: IORef (Ptr ())}

makeFieldsId ''WL_shm_pool

data WL_shm = WL_shm {wlid :: Word32, formats :: IORef [Enum_wl_shm_format]}

makeFieldsId ''WL_shm

data WL_buffer = WL_buffer
  { wlid    :: Word32
  , offset  :: Int
  , width   :: Int
  , height  :: Int
  , stride  :: Int
  , pool    :: ObjectID
  , format  :: Enum_wl_shm_format
  }

makeFieldsId ''WL_buffer

data WL_data_offer = WL_data_offer {wlid :: Word32}
makeFieldsId ''WL_data_offer

data WL_data_source = WL_data_source {wlid :: Word32}
makeFieldsId ''WL_data_source

data WL_data_device = WL_data_device {wlid :: Word32}
makeFieldsId ''WL_data_device

data WL_data_device_manager = WL_data_device_manager {wlid :: Word32}
makeFieldsId ''WL_data_device_manager

data WL_shell = WL_shell {wlid :: Word32}
makeFieldsId ''WL_shell

data WL_shell_surface = WL_shell_surface {wlid :: Word32}
makeFieldsId ''WL_shell_surface

data WL_region = WL_region {wlid :: Word32}
makeFieldsId ''WL_region

--  Nothing or empty least means no change. In order to "reset" values, set them to the defaults - ObjectID `0`, normal transform, etc.
data ContentUpdate = ContentUpdate
  { buffer          :: Maybe ObjectID
  , offset          :: Maybe (Int, Int)
  , damage          :: [Rectangle]
  , damageBuffer    :: [Rectangle]
  , frameCallbacks  :: [WL_callback]
  , opaqueRegion    :: Maybe ObjectID
  , inputRegion     :: Maybe ObjectID
  , bufferScale     :: Maybe Int
  , bufferTransform :: Maybe Enum_wl_output_transform
  , bufferRelease   :: Maybe WL_callback
  }

data SurfaceRole where SurfaceRole :: a -> SurfaceRole

-- used both as a template for content updates, and to indicate no change.
emptyContentUpdate :: ContentUpdate
emptyContentUpdate = ContentUpdate
  { buffer        = Nothing
  , offset        = Just (0,0)
  , damage        = []
  , damageBuffer  = []
  , frameCallbacks= []
  , opaqueRegion  = Nothing
  , inputRegion   = Nothing
  , bufferScale   = Nothing
  , bufferTransform = Nothing
  , bufferRelease   = Nothing
  }

data WL_surface = WL_surface
  { wlid          :: Word32
  , pendingState  :: IORef ContentUpdate
  , cuQueue       :: IORef (Seq.Seq ContentUpdate)
  , role          :: IORef SurfaceRole
  }
makeFieldsId ''WL_surface

data WL_seat = WL_seat {wlid :: Word32}
makeFieldsId ''WL_seat

data WL_pointer = WL_pointer {wlid :: Word32}
makeFieldsId ''WL_pointer

data WL_keyboard = WL_keyboard {wlid :: Word32}
makeFieldsId ''WL_keyboard

data WL_touch = WL_touch {wlid :: Word32}
makeFieldsId ''WL_touch

data WL_output = WL_output {wlid :: Word32}
makeFieldsId ''WL_output

data WL_subcompositor = WL_subcompositor {wlid :: Word32}

makeFieldsId ''WL_subcompositor

data WL_subsurface = WL_subsurface {wlid :: Word32}

makeFieldsId ''WL_subsurface

data WL_fixes = WL_fixes {wlid :: Word32}

makeFieldsId ''WL_fixes

instance DefaultIO WL_display where
  defM = pure WL_display{wlid = wlDisplayID}

instance DefaultIO WL_registry where
  defM = pure WL_registry{wlid = 0}

instance DefaultIO WL_buffer where
  defM = pure WL_buffer{wlid = 0, offset = 0, width = 0, height = 0, stride = 0, format = Enum_wl_shm_format_argb8888,pool=0}

instance DefaultIO WL_region where
  defM = pure WL_region{wlid = 0}

instance DefaultIO WL_callback where
  defM = newEmptyMVar <&> WL_callback 0

instance DefaultIO WL_compositor where
  defM = pure WL_compositor{wlid = 0}

instance DefaultIO WL_shm_pool where
  defM = do
    ref <- newIORef 0
    ptrRef <- newIORef nullPtr
    pure $ WL_shm_pool 0 0 ref ptrRef

instance DefaultIO WL_shm where
  defM = newIORef [] <&> WL_shm 0

instance DefaultIO WL_surface where
  defM = do
    let wlid = 0
    role <- newIORef $ SurfaceRole ()
    pendingState <- newIORef emptyContentUpdate
    cuQueue <- newIORef Seq.Empty
    pure WL_surface{..}

instance DefaultIO WL_data_offer where
  defM = pure WL_data_offer{wlid = 0}

instance DefaultIO WL_data_device where
  defM = pure WL_data_device{wlid = 0}

instance DefaultIO WL_data_device_manager where
  defM = pure WL_data_device_manager{wlid = 0}

instance DefaultIO WL_data_source where
  defM = pure WL_data_source{wlid = 0}

instance DefaultIO WL_shell where
  defM = pure WL_shell{wlid = 0}

instance DefaultIO WL_shell_surface where
  defM = pure WL_shell_surface{wlid = 0}

instance DefaultIO WL_seat where
  defM = pure WL_seat{wlid = 0}

instance DefaultIO WL_pointer where
  defM = pure WL_pointer{wlid = 0}

instance DefaultIO WL_keyboard where
  defM = pure WL_keyboard{wlid = 0}

instance DefaultIO WL_touch where
  defM = pure WL_touch{wlid = 0}

instance DefaultIO WL_output where
  defM = pure WL_output{wlid = 0}

instance DefaultIO WL_subcompositor where
  defM = pure WL_subcompositor{wlid = 0}

instance DefaultIO WL_subsurface where
  defM = pure WL_subsurface{wlid = 0}

instance DefaultIO WL_fixes where
  defM = pure WL_fixes{wlid = 0}

-- }}}

-- Tables {{{
$(generateTables False (\(x1 : x2 : xs) -> toUpper x1 : toUpper x2 : xs) "protocols/wayland.xml")

-- }}}

-- Interface Implementations {{{

-- WL_display {{{
instance Interface' WL_display Client where
  type Event WL_display = Event_wl_display
  type Request WL_display = Request_wl_display
  runEvent _display Event_wl_display_delete_id{id = did} = do
    ClientEnv env <- ask
    liftIO $ modifyIORef env.objects (Map.delete did)
  runEvent _display Event_wl_display_error{object_id, code, message} = do
    liftIO $ print $ "Unhandled error from `" <> show object_id <> "`: [" <> show code <> "] " <> message
  runRequest display request@Request_wl_display_sync{callback} = do
    mvar <- newEmptyMVar
    callbackObject <- newObject callback WL_callback{wlid = callback, done = mvar}
    swapMVar callbackObject.done ()
    sendMessage' request display.wlid
  runRequest display request@Request_wl_display_get_registry{registry} = do
    ClientEnv env <- ask
    modifyIORef env.objects (Map.insert registry $ Interface $ WL_registry{wlid = registry})
    sendMessage' request display.wlid

instance Interface' WL_display Server where
  type Event WL_display = Event_wl_display
  type Request WL_display = Request_wl_display
  runEvent display event@Event_wl_display_delete_id{id = did} = do
    ClientServerEnv _ env <- ask
    liftIO $ modifyIORef env.objects (Map.delete did)
    sendMessage' event display.wlid
  runEvent display event@Event_wl_display_error{object_id, code, message} = do
    sendMessage' event display.wlid
  runRequest _display Request_wl_display_sync{callback} = do
    ClientServerEnv _ env <- ask
    mvar <- newEmptyMVar
    callbackObject <- newObject callback WL_callback{wlid = callback, done = mvar}
    -- TODO: synchronize there... somehow
    putMVar mvar ()
    let event = Event_wl_callback_done{callback_data = 0}
    runEvent callbackObject event
  runRequest _display Request_wl_display_get_registry{registry} = do
    registryObject <- newObject registry WL_registry{wlid = registry}
    ClientServerEnv _ env <- ask
    versions <- zip [0 ..] . Map.toList <$> readIORef env.versionTable
    forM_ versions $ \(name, (interface', version)) -> do
      let interface = encodeUtf8 interface'
          event = Event_wl_registry_global{name, interface, version}
      sendMessage' event registry
      modifyIORef env.globals $ BM.insert interface name

-- }}}

-- WL_callback {{{
instance Interface' WL_callback Client where
  type Event WL_callback = Event_wl_callback
  type Request WL_callback = Request_wl_callback
  runEvent callback Event_wl_callback_done{callback_data} = do
    ClientEnv env <- ask
    putMVar callback.done ()
    modifyIORef env.objects (Map.delete callback.wlid)

  runRequest _ _ = pass

instance Interface' WL_callback Server where
  type Event WL_callback = Event_wl_callback
  type Request WL_callback = Request_wl_callback
  runEvent callback event@Event_wl_callback_done{callback_data} = do
    putMVar callback.done ()
    dropObject callback.wlid
    sendMessage' event callback.wlid
  runRequest _ _ = pass

-- }}}

-- WL_registry {{{

instance Interface' WL_registry Client where
  type Event WL_registry = Event_wl_registry
  type Request WL_registry = Request_wl_registry
  runEvent registry Event_wl_registry_global{name, interface, version} = do
    ClientEnv env <- ask
    let interface' = BS.init interface
    modifyIORef env.globals $ BM.insert interface' name
    vertable <- readIORef env.versionTable
    case Map.lookup (BS8.unpack interface') vertable of
      Just clientVer -> do
        when (clientVer > version)
          $ modifyIORef env.versionTable
          $ Map.insert (BS8.unpack interface') version
      Nothing -> pass
  runEvent registry Event_wl_registry_global_remove{name} = do
    ClientEnv env <- ask
    modifyIORef env.globals $ BM.deleteR name

  runRequest registry request@Request_wl_registry_bind{name, id = (interfaceName, interfaceVersion, newId)} = do
    ClientEnv env <- ask
    interfaceFromName name >>= \case
      Just x -> do
        y' <- fromJust . Map.lookup (BS8.unpack x) <$> readIORef env.interfaceTable
        Interface y <- liftIO y'
        modifyIORef env.objects . Map.insert newId $ Interface $ y & wlid .~ newId
      Nothing -> error $ "interface with name `" <> show name <> "` not found."
    sendMessage' request registry.wlid

instance Interface' WL_registry Server where
  type Event WL_registry = Event_wl_registry
  type Request WL_registry = Request_wl_registry
  runEvent registry event@Event_wl_registry_global{name, interface, version} = do
    ClientServerEnv _ env <- ask
    modifyIORef env.globals $ BM.insert interface name
    sendMessage' event registry.wlid
  runEvent registry event@Event_wl_registry_global_remove{name} = do
    ClientServerEnv _ env <- ask
    modifyIORef env.globals $ BM.deleteR name
    sendMessage' event registry.wlid
  runRequest _registry Request_wl_registry_bind{name, id = (interface, version, newId)} = do
    ClientServerEnv _ env <- ask
    interfaceFromName name >>= \case
      Just x -> do
        y' <- fromJust . Map.lookup (BS8.unpack x) <$> readIORef env.interfaceTable
        Interface y <- liftIO y'
        modifyIORef env.objects . Map.insert newId $ Interface $ y & wlid .~ newId
      Nothing -> error $ "interface with name `" <> show name <> "` not found."

-- }}}

-- WL_compositor {{{

instance Interface' WL_compositor Client where
  type Event WL_compositor = Event_wl_compositor
  type Request WL_compositor = Request_wl_compositor
  runRequest compositor request@Request_wl_compositor_create_surface{id = surfaceId} = do
    _ <- newObject surfaceId =<< (defM :: Wayland Client WL_surface)
    sendMessage' request compositor.wlid
  runRequest compositor request@Request_wl_compositor_create_region{id = regionId} = do
    ClientEnv env <- ask
    modifyIORef env.objects $ Map.insert regionId $ Interface $ WL_region{wlid = regionId}
    sendMessage' request compositor.wlid
  runRequest compositor request@Request_wl_compositor_release = do
    sendMessage' request compositor.wlid
  runEvent _ _ = pass

instance Interface' WL_compositor Server where
  type Event WL_compositor = Event_wl_compositor
  type Request WL_compositor = Request_wl_compositor
  runEvent _ _ = pass
  runRequest _compositor Request_wl_compositor_create_surface{id = surfaceId} = do
    _ <- newObject surfaceId =<< (defM :: Wayland Server WL_surface)
    pass
  runRequest _compositor Request_wl_compositor_create_region{id = regionId} = void $ newObject regionId WL_region{wlid = regionId}
  runRequest compositor Request_wl_compositor_release = dropObject compositor.wlid

-- }}}

-- WL_shm_pool {{{
instance Interface' WL_shm_pool Client where
  type Event WL_shm_pool = Event_wl_shm_pool
  type Request WL_shm_pool = Request_wl_shm_pool

  runRequest shm_pool request@Request_wl_shm_pool_create_buffer{id = bufId, offset = offset', width = width', height = height', stride = stride', format = format'} = do
    ClientEnv env <- ask
    let buffer = WL_buffer{wlid = bufId, offset = offset', width = width', height = height', stride = stride', format = format', pool = shm_pool.wlid}
    modifyIORef env.objects $ Map.insert bufId $ Interface buffer
    sendMessage' request shm_pool.wlid
  runRequest shm_pool request@Request_wl_shm_pool_destroy = do
    ClientEnv env <- ask
    dropObject shm_pool.wlid
    sendMessage' request shm_pool.wlid
  runRequest shm_pool request@Request_wl_shm_pool_resize{size = size'} = do
    writeIORef shm_pool.size size'
    sendMessage' request shm_pool.wlid

  runEvent _ _ = pass

instance Interface' WL_shm_pool Server where
  type Event WL_shm_pool = Event_wl_shm_pool
  type Request WL_shm_pool = Request_wl_shm_pool

  runRequest shm_pool Request_wl_shm_pool_create_buffer{id = bufId, offset = offset', width = width', height = height', stride = stride', format = format'} = do
    ClientServerEnv _ env <- ask
    let buffer = WL_buffer{wlid = bufId, offset = offset', width = width', height = height', stride = stride', format = format', pool = shm_pool.wlid}
    modifyIORef env.objects $ Map.insert bufId $ Interface buffer
  runRequest shm_pool request@Request_wl_shm_pool_destroy = do
    dropObject shm_pool.wlid
    sendMessage' request shm_pool.wlid
  runRequest shm_pool Request_wl_shm_pool_resize{size = size'} = do
    liftIO . setFdSize shm_pool.fd $ fromIntegral size'
    oldsize <- readIORef shm_pool.size
    ptr <- readIORef shm_pool.ptr
    liftIO $ munmap ptr $ fromIntegral oldsize
    result <-
      liftIO
        $ try
        $ mmap
          nullPtr
          (fromIntegral size')
          (protRead <> protWrite)
          (mkMmapFlags mapShared mempty)
          shm_pool.fd
          0
    ptr' <- case result of
      Left (e :: SomeException) -> liftIO (traceIO $ "mmap failed: " ++ show e) >> undefined
      Right ptr' -> liftIO (traceIO $ "mmap OK, ptr = " ++ show ptr') $> ptr'
    writeIORef shm_pool.ptr ptr'
    writeIORef shm_pool.size size'
  runEvent _ _ = pass

-- }}}

-- WL_shm {{{
instance Interface' WL_shm Client where
  type Event WL_shm = Event_wl_shm
  type Request WL_shm = Request_wl_shm
  runRequest shm request@Request_wl_shm_create_pool{id = poolId, fd = fd', size = size'} = do
    ClientEnv env <- ask
    sizeRef <- newIORef size'
    ptrRef <- newIORef nullPtr {-IIRC client doesn't need exposed -}
    modifyIORef env.objects $ Map.insert poolId $ Interface $ WL_shm_pool{wlid = poolId, fd = fd', size = sizeRef, ptr = ptrRef}
    sendMessageWithFds' request [fd'] shm.wlid
  runRequest shm request@Request_wl_shm_release{} = do
    ClientEnv env <- ask
    dropObject shm.wlid
    sendMessage' request shm.wlid

  runEvent shm Event_wl_shm_format{format} = modifyIORef shm.formats (format :)

instance Interface' WL_shm Server where
  type Event WL_shm = Event_wl_shm
  type Request WL_shm = Request_wl_shm
  runRequest _shm Request_wl_shm_create_pool{id = poolId, fd = fd', size = size'} = do
    ClientServerEnv _ env <- ask
    result <-
      liftIO
        $ try
        $ mmap
          nullPtr
          (fromIntegral size')
          (protRead <> protWrite)
          (mkMmapFlags mapShared mempty)
          fd'
          0
    ptr' <- case result of
      Left (e :: SomeException) -> liftIO (traceIO $ "mmap failed: " ++ show e) >> undefined
      Right ptr' -> liftIO (traceIO $ "mmap OK, ptr = " ++ show ptr') $> ptr'
    sizeRef <- newIORef size'
    ptrRef <- newIORef ptr'
    modifyIORef env.objects $ Map.insert poolId $ Interface $ WL_shm_pool{wlid = poolId, fd = fd', size = sizeRef, ptr = ptrRef}
  runRequest shm Request_wl_shm_release = do
    ClientServerEnv _ env <- ask
    dropObject shm.wlid
  runEvent shm event@Event_wl_shm_format{format} = do
    sendMessage' event shm.wlid

-- }}}

-- WL_buffer {{{
instance Interface' WL_buffer Client where
  type Event WL_buffer = Event_wl_buffer
  type Request WL_buffer = Request_wl_buffer
  runRequest buffer request@Request_wl_buffer_destroy = do
    dropObject buffer.wlid
    sendMessage' request buffer.wlid
  runEvent buffer Event_wl_buffer_release = pass

instance Interface' WL_buffer Server where
  type Event WL_buffer = Event_wl_buffer
  type Request WL_buffer = Request_wl_buffer
  runRequest buffer Request_wl_buffer_destroy = dropObject buffer.wlid
  runEvent buffer event@Event_wl_buffer_release = do
    sendMessage' event buffer.wlid

-- }}}

-- WL_data_offer {{{
instance Interface' WL_data_offer Client where
  type Event WL_data_offer = Event_wl_data_offer
  type Request WL_data_offer = Request_wl_data_offer
  runRequest _ Request_wl_data_offer_accept{} = pass
  runRequest _ Request_wl_data_offer_receive{} = pass
  runRequest data_offer request@Request_wl_data_offer_destroy{} = do
    dropObject data_offer.wlid
    sendMessage' request data_offer.wlid
  runRequest _ Request_wl_data_offer_finish{} = pass
  runRequest _ Request_wl_data_offer_set_actions{} = pass
  runEvent _ Event_wl_data_offer_offer{} = pass
  runEvent _ Event_wl_data_offer_source_actions{} = pass
  runEvent _ Event_wl_data_offer_action{} = pass

instance Interface' WL_data_offer Server

-- }}}

-- WL_data_source {{{
instance Interface' WL_data_source Client where
  type Event WL_data_source = Event_wl_data_source
  type Request WL_data_source = Request_wl_data_source
  runRequest _ Request_wl_data_source_offer{} = pass
  runRequest data_source request@Request_wl_data_source_destroy{} = do
    dropObject data_source.wlid
    sendMessage' request data_source.wlid
  runRequest _ Request_wl_data_source_set_actions{} = pass
  runEvent _ Event_wl_data_source_target{} = pass
  runEvent _ Event_wl_data_source_send{} = pass
  runEvent _ Event_wl_data_source_cancelled{} = pass
  runEvent _ Event_wl_data_source_dnd_drop_performed{} = pass
  runEvent _ Event_wl_data_source_dnd_finished{} = pass
  runEvent _ Event_wl_data_source_action{} = pass

instance Interface' WL_data_source Server

-- }}}

-- WL_data_device {{{
instance Interface' WL_data_device Client where
  type Event WL_data_device = Event_wl_data_device
  type Request WL_data_device = Request_wl_data_device
  runRequest _ Request_wl_data_device_start_drag{} = pass
  runRequest _ Request_wl_data_device_set_selection{} = pass
  runRequest _ Request_wl_data_device_release{} = pass
  runEvent _ Event_wl_data_device_data_offer{} = pass
  runEvent _ Event_wl_data_device_enter{} = pass
  runEvent _ Event_wl_data_device_leave{} = pass
  runEvent _ Event_wl_data_device_motion{} = pass
  runEvent _ Event_wl_data_device_drop{} = pass
  runEvent _ Event_wl_data_device_selection{} = pass

instance Interface' WL_data_device Server

-- }}}

-- WL_data_device_manager {{{
instance Interface' WL_data_device_manager Client where
  type Event WL_data_device_manager = Event_wl_data_device_manager
  type Request WL_data_device_manager = Request_wl_data_device_manager
  runRequest _ Request_wl_data_device_manager_create_data_source{} = pass
  runRequest _ Request_wl_data_device_manager_get_data_device{} = pass
  runRequest _ Request_wl_data_device_manager_release{} = pass

instance Interface' WL_data_device_manager Server

-- }}}

-- WL_shell {{{
instance Interface' WL_shell Client where
  type Event WL_shell = Event_wl_shell
  type Request WL_shell = Request_wl_shell
  runRequest _ Request_wl_shell_get_shell_surface{} = pass

instance Interface' WL_shell Server

-- }}}

-- WL_shell_surface {{{
instance Interface' WL_shell_surface Client where
  type Event WL_shell_surface = Event_wl_shell_surface
  type Request WL_shell_surface = Request_wl_shell_surface
  runRequest _ Request_wl_shell_surface_pong{} = pass
  runRequest _ Request_wl_shell_surface_move{} = pass
  runRequest _ Request_wl_shell_surface_resize{} = pass
  runRequest _ Request_wl_shell_surface_set_toplevel{} = pass
  runRequest _ Request_wl_shell_surface_set_transient{} = pass
  runRequest _ Request_wl_shell_surface_set_fullscreen{} = pass
  runRequest _ Request_wl_shell_surface_set_popup{} = pass
  runRequest _ Request_wl_shell_surface_set_maximized{} = pass
  runRequest _ Request_wl_shell_surface_set_title{} = pass
  runRequest _ Request_wl_shell_surface_set_class{} = pass
  runEvent _ Event_wl_shell_surface_ping{} = pass
  runEvent _ Event_wl_shell_surface_configure{} = pass
  runEvent _ Event_wl_shell_surface_popup_done{} = pass

instance Interface' WL_shell_surface Server

-- }}}

-- WL_surface {{{
instance Interface' WL_surface Client where
  type Event WL_surface = Event_wl_surface
  type Request WL_surface = Request_wl_surface
  runRequest surface request@Request_wl_surface_destroy{} = do
    dropObject surface.wlid
    sendMessage' request surface.wlid
  runRequest surface request@Request_wl_surface_attach{buffer = bufferId, x, y} = do
    sendMessage' request surface.wlid
  runRequest _ Request_wl_surface_damage{} = pass
  runRequest _ Request_wl_surface_frame{} = pass
  runRequest _ Request_wl_surface_set_opaque_region{} = pass
  runRequest _ Request_wl_surface_set_input_region{} = pass
  runRequest surface request@Request_wl_surface_commit = do
    sendMessage' request surface.wlid
  runRequest _ Request_wl_surface_set_buffer_transform{} = pass
  runRequest _ Request_wl_surface_set_buffer_scale{} = pass
  runRequest surface request@Request_wl_surface_damage_buffer{} = do
    sendMessage' request surface.wlid
  runRequest _ Request_wl_surface_offset{} = pass
  runRequest _ Request_wl_surface_get_release{} = pass
  runEvent _ Event_wl_surface_enter{} = pass
  runEvent _ Event_wl_surface_leave{} = pass
  runEvent _ Event_wl_surface_preferred_buffer_scale{} = pass
  runEvent _ Event_wl_surface_preferred_buffer_transform{} = pass

instance Interface' WL_surface Server where
  type Event WL_surface = Event_wl_surface
  type Request WL_surface = Request_wl_surface
  runRequest surface Request_wl_surface_attach{buffer = bufferId, x, y} = do
    modifyIORef surface.pendingState $ \ss ->
      ss
        { buffer = Just bufferId
        , offset = Just (x, y)
        , damage = []
        , frameCallbacks = []
        }
  runRequest surface Request_wl_surface_commit = do
    ClientServerEnv _ env <- ask
    pending <- readIORef surface.pendingState
    liftIO $ do
      modifyIORef' surface.cuQueue (pending Seq.<|)
      writeIORef surface.pendingState emptyContentUpdate

  runRequest surface Request_wl_surface_destroy = dropObject surface.wlid
  runRequest _surface Request_wl_surface_damage{} = pass
  runRequest _surface Request_wl_surface_damage_buffer{} = pass
  runRequest _surface Request_wl_surface_frame{} = pass
  runRequest _surface Request_wl_surface_set_opaque_region{} = pass
  runRequest _surface Request_wl_surface_set_input_region{} = pass
  runRequest _surface Request_wl_surface_set_buffer_scale{} = pass
  runRequest _surface Request_wl_surface_set_buffer_transform{} = pass
  runRequest _surface Request_wl_surface_offset{} = pass
  runRequest _surface Request_wl_surface_get_release{} = pass
  runEvent _surface Event_wl_surface_enter{} = pass
  runEvent _surface Event_wl_surface_leave{} = pass

-- }}}

-- WL_seat {{{
instance Interface' WL_seat Client where
  type Event WL_seat = Event_wl_seat
  type Request WL_seat = Request_wl_seat
  runRequest _ Request_wl_seat_get_pointer{} = pass
  runRequest _ Request_wl_seat_get_keyboard{} = pass
  runRequest _ Request_wl_seat_get_touch{} = pass
  runRequest _ Request_wl_seat_release{} = pass
  runEvent _ Event_wl_seat_capabilities{} = pass
  runEvent _ Event_wl_seat_name{} = pass

instance Interface' WL_seat Server

-- }}}

-- WL_pointer {{{
instance Interface' WL_pointer Client where
  type Event WL_pointer = Event_wl_pointer
  type Request WL_pointer = Request_wl_pointer
  runRequest _ Request_wl_pointer_set_cursor{} = pass
  runRequest _ Request_wl_pointer_release{} = pass
  runEvent _ Event_wl_pointer_enter{} = pass
  runEvent _ Event_wl_pointer_leave{} = pass
  runEvent _ Event_wl_pointer_motion{} = pass
  runEvent _ Event_wl_pointer_button{} = pass
  runEvent _ Event_wl_pointer_axis{} = pass
  runEvent _ Event_wl_pointer_frame{} = pass
  runEvent _ Event_wl_pointer_axis_source{} = pass
  runEvent _ Event_wl_pointer_axis_stop{} = pass
  runEvent _ Event_wl_pointer_axis_discrete{} = pass
  runEvent _ Event_wl_pointer_axis_value120{} = pass
  runEvent _ Event_wl_pointer_axis_relative_direction{} = pass

instance Interface' WL_pointer Server

-- }}}

-- WL_keyboard {{{
instance Interface' WL_keyboard Client where
  type Event WL_keyboard = Event_wl_keyboard
  type Request WL_keyboard = Request_wl_keyboard
  runRequest _ Request_wl_keyboard_release{} = pass
  runEvent _ Event_wl_keyboard_keymap{} = pass
  runEvent _ Event_wl_keyboard_enter{} = pass
  runEvent _ Event_wl_keyboard_leave{} = pass
  runEvent _ Event_wl_keyboard_key{} = pass
  runEvent _ Event_wl_keyboard_modifiers{} = pass
  runEvent _ Event_wl_keyboard_repeat_info{} = pass

instance Interface' WL_keyboard Server

-- }}}

-- WL_touch {{{
instance Interface' WL_touch Client where
  type Event WL_touch = Event_wl_touch
  type Request WL_touch = Request_wl_touch
  runRequest _ Request_wl_touch_release{} = pass
  runEvent _ Event_wl_touch_down{} = pass
  runEvent _ Event_wl_touch_up{} = pass
  runEvent _ Event_wl_touch_motion{} = pass
  runEvent _ Event_wl_touch_frame{} = pass
  runEvent _ Event_wl_touch_cancel{} = pass
  runEvent _ Event_wl_touch_shape{} = pass
  runEvent _ Event_wl_touch_orientation{} = pass

instance Interface' WL_touch Server

-- }}}

-- WL_output {{{
instance Interface' WL_output Client where
  type Event WL_output = Event_wl_output
  type Request WL_output = Request_wl_output
  runRequest _ Request_wl_output_release{} = pass
  runEvent _ Event_wl_output_geometry{} = pass
  runEvent _ Event_wl_output_mode{} = pass
  runEvent _ Event_wl_output_done{} = pass
  runEvent _ Event_wl_output_scale{} = pass
  runEvent _ Event_wl_output_name{} = pass
  runEvent _ Event_wl_output_description{} = pass

instance Interface' WL_output Server

-- }}}

-- WL_region {{{
instance Interface' WL_region Client where
  type Event WL_region = Event_wl_region
  type Request WL_region = Request_wl_region
  runRequest _ Request_wl_region_destroy{} = pass
  runRequest _ Request_wl_region_add{} = pass
  runRequest _ Request_wl_region_subtract{} = pass

instance Interface' WL_region Server

-- }}}

-- WL_subcompositor {{{
instance Interface' WL_subcompositor Client where
  type Event WL_subcompositor = Event_wl_subcompositor
  type Request WL_subcompositor = Request_wl_subcompositor
  runRequest _ Request_wl_subcompositor_destroy{} = pass
  runRequest _ Request_wl_subcompositor_get_subsurface{} = pass

instance Interface' WL_subcompositor Server

-- }}}

-- WL_subsurface {{{
instance Interface' WL_subsurface Client where
  type Event WL_subsurface = Event_wl_subsurface
  type Request WL_subsurface = Request_wl_subsurface
  runRequest _ Request_wl_subsurface_destroy{} = pass
  runRequest _ Request_wl_subsurface_set_position{} = pass
  runRequest _ Request_wl_subsurface_place_above{} = pass
  runRequest _ Request_wl_subsurface_place_below{} = pass
  runRequest _ Request_wl_subsurface_set_sync{} = pass
  runRequest _ Request_wl_subsurface_set_desync{} = pass

instance Interface' WL_subsurface Server

-- }}}

-- WL_fixes {{{
instance Interface' WL_fixes Client where
  type Event WL_fixes = Event_wl_fixes
  type Request WL_fixes = Request_wl_fixes
  runRequest _ Request_wl_fixes_destroy{} = pass
  runRequest _ Request_wl_fixes_destroy_registry{} = pass

instance Interface' WL_fixes Server

-- }}}

-- }}}

-- Wrapper Functions, for QoL {{{
bindToInterface :: WL_registry -> BS.ByteString -> Wayland Client (Maybe Word32)
bindToInterface registry intName = go 1
  where
    go :: Int -> Wayland Client (Maybe Word32)
    go count = do
      when
        (count >= 10)
        (putTextLn ("ERROR: the wayland global " <> show intName <> " not found") >> exitFailure) -- maybe return Nothing here?
      putTextLn $ mconcat ["Trying to bind to ", show intName, "... (", show count, ")"]
      ClientEnv env <- ask
      glob <- BM.lookup intName <$> readIORef env.globals
      case glob of
        Just x -> do
          new_id <- newObjectId
          ver <- fromJust . Map.lookup (BS8.unpack intName) <$> readIORef env.versionTable
          runRequest registry Request_wl_registry_bind{name = x, id = (intName, ver, new_id)}
          pure $ Just new_id
        Nothing -> liftIO (threadDelay $ 100 * 1000) >> go (count + 1)

-- }}}
-- vim: foldmethod=marker
