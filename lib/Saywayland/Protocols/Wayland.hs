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
import Data.ByteString qualified as BS
import Data.ByteString.Char8 qualified as BS8
import Data.Map qualified as Map
import Data.Maybe (fromJust)
import Data.Sequence qualified as Seq
import Debug.Trace (traceIO)
import Foreign (Ptr, nullPtr)
import MMAP (mapShared, mkMmapFlags, mmap, munmap, protRead, protWrite)
import Protocol
import Relude hiding (get)
import Saywayland.Types
import System.Posix (Fd, setFdSize)
import GHC.IORef (atomicSwapIORef)


$(loadProtocolFileEnums False "protocols/wayland.xml")
--  Nothing or empty list means no change. In order to "reset" values, set them to the defaults - ObjectID `0`, normal transform, etc.
data ContentUpdate = ContentUpdate
  { buffer          :: Maybe (TObjectID Wl_buffer)
  , offset          :: Maybe (Int, Int)
  , damage          :: [Rectangle]
  , damageBuffer    :: [Rectangle]
  , frameCallbacks  :: [TObjectID Wl_callback]
  , opaqueRegion    :: Maybe (TObjectID Wl_region)
  , inputRegion     :: Maybe (TObjectID Wl_region)
  , bufferScale     :: Maybe Int
  , bufferTransform :: Maybe Enum_wl_output_transform
  , bufferRelease   :: Maybe (TObjectID Wl_callback)
  , subsurfaces     :: Maybe SubsurfaceStack
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
  , subsurfaces   = Nothing
  }

-- Interfaces {{{
newtype Wl_display = Wl_display {wlid :: TObjectID Wl_display}

newtype Wl_registry = Wl_registry {wlid :: TObjectID Wl_registry}

data Wl_callback = Wl_callback {wlid :: TObjectID Wl_callback, done :: MVar ()}

newtype Wl_compositor = Wl_compositor {wlid :: TObjectID Wl_compositor}

data Wl_shm_pool = Wl_shm_pool {wlid :: TObjectID Wl_shm_pool, fd :: Fd, size :: IORef Int, ptr :: IORef (Ptr ())}

data Wl_shm = Wl_shm {wlid :: TObjectID Wl_shm, formats :: IORef [Enum_wl_shm_format]}

data Wl_buffer = Wl_buffer
  { wlid    :: TObjectID Wl_buffer
  , offset  :: Int
  , width   :: Int
  , height  :: Int
  , stride  :: Int
  , pool    :: TObjectID Wl_shm_pool
  , format  :: Enum_wl_shm_format
  }

newtype Wl_data_offer = Wl_data_offer {wlid :: TObjectID Wl_data_offer}

newtype Wl_data_source = Wl_data_source {wlid :: TObjectID Wl_data_source}

newtype Wl_data_device = Wl_data_device {wlid :: TObjectID Wl_data_device}

newtype Wl_data_device_manager = Wl_data_device_manager {wlid :: TObjectID Wl_data_device_manager}

newtype Wl_shell = Wl_shell {wlid :: TObjectID Wl_shell}

newtype Wl_shell_surface = Wl_shell_surface {wlid :: TObjectID Wl_shell_surface}

data Wl_region = Wl_region {wlid :: TObjectID Wl_region, included :: IORef [Rectangle], excluded :: IORef [Rectangle]}

data SubsurfaceStack = SubsurfaceStack
  { above :: Seq.Seq (TObjectID Wl_surface)
  , below :: Seq.Seq (TObjectID Wl_surface)
  }

data SurfaceState = SurfaceState
  { sBuffer          :: TObjectID Wl_buffer
  , sBufferOffset    :: (Int, Int)
  , sDamage          :: [Rectangle]
  , sCallbacks       :: [TObjectID Wl_callback]
  , sOpaqueRegion    :: TObjectID Wl_region
  , sInputRegion     :: TObjectID Wl_region
  , sBufferScale     :: Int
  , sBufferTransform :: Enum_wl_output_transform
  , sSubsurfaces     :: SubsurfaceStack
  }

data Wl_surface = Wl_surface
  { wlid          :: TObjectID Wl_surface
  , pendingState  :: IORef ContentUpdate
  , cuQueue       :: IORef (Seq.Seq ContentUpdate)
  , role          :: IORef SurfaceRole
  , state         :: IORef SurfaceState
  }

newtype Wl_seat = Wl_seat {wlid :: TObjectID Wl_seat}

newtype Wl_pointer = Wl_pointer {wlid :: TObjectID Wl_pointer}

newtype Wl_keyboard = Wl_keyboard {wlid :: TObjectID Wl_keyboard}

newtype Wl_touch = Wl_touch {wlid :: TObjectID Wl_touch}

newtype Wl_output = Wl_output {wlid :: TObjectID Wl_output}

newtype Wl_subcompositor = Wl_subcompositor {wlid :: TObjectID Wl_subcompositor}

data Wl_subsurface = Wl_subsurface {wlid :: TObjectID Wl_subsurface, surface :: TObjectID Wl_surface, parent :: TObjectID Wl_surface, position :: IORef (Int, Int), synchronized :: IORef Bool}

newtype Wl_fixes = Wl_fixes {wlid :: TObjectID Wl_fixes}


$(loadProtocolFile wlFormatter False "protocols/wayland.xml")

$(concat <$> mapM makeFieldsId [''Wl_display, ''Wl_registry, ''Wl_callback, ''Wl_compositor, ''Wl_shm
    , ''Wl_shm_pool, ''Wl_buffer, ''Wl_data_offer, ''Wl_data_source
    , ''Wl_data_device, ''Wl_data_device_manager, ''Wl_shell, ''Wl_shell_surface
    , ''Wl_region, ''Wl_surface, ''Wl_seat, ''Wl_pointer, ''Wl_keyboard, ''Wl_touch
    , ''Wl_output, ''Wl_subcompositor, ''Wl_subsurface, ''Wl_fixes])

-- DefaultIO instances {{{
instance DefaultIO Wl_display where
  defM = pure Wl_display{wlid = TObjectID wlDisplayID}

instance DefaultIO Wl_registry where
  defM = pure Wl_registry{wlid = 0}

instance DefaultIO Wl_buffer where
  defM = pure Wl_buffer{wlid = 0, offset = 0, width = 0, height = 0, stride = 0, format = Enum_wl_shm_format_argb8888,pool=0}

instance DefaultIO Wl_region where
  defM = do
    included <- newIORef []
    excluded <- newIORef []
    pure Wl_region{wlid = 0, included, excluded}

instance DefaultIO Wl_callback where
  defM = newEmptyMVar <&> Wl_callback 0

instance DefaultIO Wl_compositor where
  defM = pure Wl_compositor{wlid = 0}

instance DefaultIO Wl_shm_pool where
  defM = do
    ref <- newIORef 0
    ptrRef <- newIORef nullPtr
    pure $ Wl_shm_pool 0 0 ref ptrRef

instance DefaultIO Wl_shm where
  defM = newIORef [] <&> Wl_shm 0

instance DefaultIO Wl_surface where
  defM = do
    let wlid = 0
    role <- newIORef $ SurfaceRole ()
    pendingState <- newIORef emptyContentUpdate
    cuQueue <- newIORef Seq.Empty
    state <- newIORef  $ SurfaceState
      { sBuffer       = 0
      , sBufferOffset = (0,0)
      , sDamage       = []
      , sCallbacks    = []
      , sOpaqueRegion = 0
      , sInputRegion  = 0
      , sBufferScale  = 1
      , sBufferTransform  = Enum_wl_output_transform_normal
      , sSubsurfaces = SubsurfaceStack {above=Seq.empty, below=Seq.empty}
      }
    pure Wl_surface{..}

instance DefaultIO Wl_data_offer where
  defM = pure Wl_data_offer{wlid = 0}

instance DefaultIO Wl_data_device where
  defM = pure Wl_data_device{wlid = 0}

instance DefaultIO Wl_data_device_manager where
  defM = pure Wl_data_device_manager{wlid = 0}

instance DefaultIO Wl_data_source where
  defM = pure Wl_data_source{wlid = 0}

instance DefaultIO Wl_shell where
  defM = pure Wl_shell{wlid = 0}

instance DefaultIO Wl_shell_surface where
  defM = pure Wl_shell_surface{wlid = 0}

instance DefaultIO Wl_seat where
  defM = pure Wl_seat{wlid = 0}

instance DefaultIO Wl_pointer where
  defM = pure Wl_pointer{wlid = 0}

instance DefaultIO Wl_keyboard where
  defM = pure Wl_keyboard{wlid = 0}

instance DefaultIO Wl_touch where
  defM = pure Wl_touch{wlid = 0}

instance DefaultIO Wl_output where
  defM = pure Wl_output{wlid = 0}

instance DefaultIO Wl_subcompositor where
  defM = pure Wl_subcompositor{wlid = 0}

instance DefaultIO Wl_subsurface where
  defM = do
    position <- newIORef (0,0)
    synchronized <- newIORef True
    pure Wl_subsurface{wlid = 0, surface = 0, parent = 0, position, synchronized}

instance DefaultIO Wl_fixes where
  defM = pure Wl_fixes{wlid = 0}
-- }}}
-- Tables {{{
$(generateTables False wlFormatter "protocols/wayland.xml")
-- }}}
-- }}}

-- | function that removes interface behind the provided id from the object map AND, if running on server, sends the delete_id event.
dropObject :: TObjectID a -> Wayland p ()
dropObject (TObjectID i) = ask >>= \case
    ClientEnv env -> modifyIORef env.objects $ Map.delete i
    ClientServerEnv _ env _ -> do
      modifyIORef env.objects $ Map.delete i
      Just wldisplay <- getInterface' @Wl_display 1
      runEvent wldisplay $ Event_wl_display_delete_id i

-- | send an error message to the client.
sendError :: TObjectID a -> Word32 -> BS.ByteString -> Wayland Server ()
sendError (TObjectID i) code msg = do
  Just wldisplay <- getInterface' @Wl_display 1
  runEvent wldisplay $ Event_wl_display_error i code msg
  


-- Interface Implementations {{{

-- Wl_display {{{
instance Interface' Wl_display Client where
  type Event Wl_display = Event_wl_display
  type Request Wl_display = Request_wl_display
  runEvent _display (Event_wl_display_delete_id did) = do
    ClientEnv env <- ask
    liftIO $ modifyIORef env.objects (Map.delete did)
  runEvent _display (Event_wl_display_error object_id code message) = do
    liftIO $ print $ "Unhandled error from `" <> show object_id <> "`: [" <> show code <> "] " <> message
  runRequest display request@(Request_wl_display_sync callback) = do
    mvar <- newEmptyMVar
    callbackObject <- newObject callback Wl_callback{wlid = callback, done = mvar}
    swapMVar callbackObject.done ()
    sendMessage' request display.wlid
  runRequest display request@(Request_wl_display_get_registry registry) = do
    _registry <- newObject registry Wl_registry{wlid = registry}
    sendMessage' request display.wlid

instance Interface' Wl_display Server where
  type Event Wl_display = Event_wl_display
  type Request Wl_display = Request_wl_display
  runEvent display event@(Event_wl_display_delete_id did) = do
    ClientServerEnv _ env _ <- ask
    liftIO $ modifyIORef env.objects (Map.delete did)
    sendMessage' event display.wlid
  runEvent display event@(Event_wl_display_error object_id code message) = do
    sendMessage' event display.wlid
  runRequest _display (Request_wl_display_sync callback) = do
    mvar <- newEmptyMVar
    callbackObject <- newObject callback Wl_callback{wlid = callback, done = mvar}
    -- TODO: synchronize there... somehow
    putMVar mvar ()
    let event = Event_wl_callback_done 0
    runEvent callbackObject event
  runRequest _display (Request_wl_display_get_registry registry) = do
    registryObject <- newObject registry Wl_registry{wlid = registry}
    ClientServerEnv _ env _ <- ask
    versions <- zip [0 ..] . Map.toList <$> readIORef env.versionTable
    forM_ versions $ \(name, (interface', version)) -> do
      let interface = encodeUtf8 interface'
          event = Event_wl_registry_global name interface version
      sendMessage' event registry
      modifyIORef env.globals $ BM.insert interface name

-- }}}

-- Wl_callback {{{
instance Interface' Wl_callback Client where
  type Event Wl_callback = Event_wl_callback
  type Request Wl_callback = Request_wl_callback
  runEvent callback (Event_wl_callback_done callback_data) = do
    ClientEnv env <- ask
    putMVar callback.done ()
    dropObject callback.wlid

  runRequest _ _ = pass

instance Interface' Wl_callback Server where
  type Event Wl_callback = Event_wl_callback
  type Request Wl_callback = Request_wl_callback
  runEvent callback event@(Event_wl_callback_done callback_data) = do
    putMVar callback.done ()
    dropObject callback.wlid
    sendMessage' event callback.wlid
  runRequest _ _ = pass
-- }}}

-- Wl_registry {{{
instance Interface' Wl_registry Client where
  type Event Wl_registry = Event_wl_registry
  type Request Wl_registry = Request_wl_registry
  runEvent registry (Event_wl_registry_global name interface version) = do
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
  runEvent registry (Event_wl_registry_global_remove name) = do
    ClientEnv env <- ask
    modifyIORef env.globals $ BM.deleteR name

  runRequest registry request@(Request_wl_registry_bind name (interfaceName, interfaceVersion, newId)) = do
    ClientEnv env <- ask
    interfaceFromName name >>= \case
      Just x -> do
        y' <- fromJust . Map.lookup (BS8.unpack x) <$> readIORef env.interfaceTable
        Interface y <- liftIO y'
        void $ newObject (TObjectID newId) $ y & wlid .~ TObjectID newId
      Nothing -> error $ "interface with name `" <> show name <> "` not found."
    sendMessage' request registry.wlid

instance Interface' Wl_registry Server where
  type Event Wl_registry = Event_wl_registry
  type Request Wl_registry = Request_wl_registry
  runEvent registry event@(Event_wl_registry_global name interface version) = do
    ClientServerEnv _ env _ <- ask
    modifyIORef env.globals $ BM.insert interface name
    sendMessage' event registry.wlid
  runEvent registry event@(Event_wl_registry_global_remove name) = do
    ClientServerEnv _ env _ <- ask
    modifyIORef env.globals $ BM.deleteR name
    sendMessage' event registry.wlid
  runRequest _registry (Request_wl_registry_bind name (interface, version, newId)) = do
    ClientServerEnv _ env _ <- ask
    interfaceFromName name >>= \case
      Just x -> do
        y' <- fromJust . Map.lookup (BS8.unpack x) <$> readIORef env.interfaceTable
        Interface y <- liftIO y'
        void $ newObject (TObjectID newId) $ y & wlid .~ TObjectID newId
      Nothing -> error $ "interface with name `" <> show name <> "` not found."
-- }}}

-- Wl_compositor {{{
instance Interface' Wl_compositor Client where
  type Event Wl_compositor = Event_wl_compositor
  type Request Wl_compositor = Request_wl_compositor
  runRequest compositor request@(Request_wl_compositor_create_surface surfaceId) = do
    void $ newObject surfaceId . (wlid .~ surfaceId) =<< (defM :: Wayland Client Wl_surface)
    sendMessage' request compositor.wlid
  runRequest compositor request@(Request_wl_compositor_create_region regionId) = do
    void $ newObject regionId . (wlid .~ regionId) =<< (defM :: Wayland Client Wl_region)
    sendMessage' request compositor.wlid
  runRequest compositor request@Request_wl_compositor_release = do
    dropObject compositor.wlid
    sendMessage' request compositor.wlid
  runEvent _ _ = pass

instance Interface' Wl_compositor Server where
  type Event Wl_compositor = Event_wl_compositor
  type Request Wl_compositor = Request_wl_compositor
  runEvent _ _ = pass
  runRequest _compositor (Request_wl_compositor_create_surface surfaceId) = void $ newObject surfaceId . (wlid .~ surfaceId) =<< (defM :: Wayland Server Wl_surface)
  runRequest _compositor (Request_wl_compositor_create_region regionId) = void $ newObject regionId . (wlid .~ regionId) =<< (defM :: Wayland Server Wl_region)
  runRequest compositor Request_wl_compositor_release = dropObject compositor.wlid
-- }}}

-- Wl_shm_pool {{{
instance Interface' Wl_shm_pool Client where
  type Event Wl_shm_pool = Event_wl_shm_pool
  type Request Wl_shm_pool = Request_wl_shm_pool

  runRequest shm_pool request@(Request_wl_shm_pool_create_buffer bufId offset' width' height' stride' format') = do
    let buffer = Wl_buffer{wlid = bufId, offset = offset', width = width', height = height', stride = stride', format = format', pool = shm_pool.wlid}
    void $ newObject bufId buffer
    sendMessage' request shm_pool.wlid
  runRequest shm_pool request@Request_wl_shm_pool_destroy = do
    dropObject shm_pool.wlid
    sendMessage' request shm_pool.wlid
  runRequest shm_pool request@(Request_wl_shm_pool_resize size') = do
    writeIORef shm_pool.size size'
    sendMessage' request shm_pool.wlid

  runEvent _ _ = pass

instance Interface' Wl_shm_pool Server where
  type Event Wl_shm_pool = Event_wl_shm_pool
  type Request Wl_shm_pool = Request_wl_shm_pool

  runRequest shm_pool (Request_wl_shm_pool_create_buffer bufId offset' width' height' stride' format') = do
    ClientServerEnv _ env _ <- ask
    let buffer = Wl_buffer{wlid = bufId, offset = offset', width = width', height = height', stride = stride', format = format', pool = shm_pool.wlid}
    void $ newObject bufId buffer
  runRequest shm_pool request@Request_wl_shm_pool_destroy = do
    dropObject shm_pool.wlid
    sendMessage' request shm_pool.wlid
  runRequest shm_pool (Request_wl_shm_pool_resize size') = do
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

-- Wl_shm {{{
instance Interface' Wl_shm Client where
  type Event Wl_shm = Event_wl_shm
  type Request Wl_shm = Request_wl_shm
  runRequest shm request@(Request_wl_shm_create_pool poolId fd' size') = do
    sizeRef <- newIORef size'
    ptrRef <- newIORef nullPtr {-IIRC client doesn't need exposed -}
    void $ newObject poolId $ Wl_shm_pool{wlid = poolId, fd = fd', size = sizeRef, ptr = ptrRef}
    sendMessageWithFds' request [fd'] shm.wlid
  runRequest shm request@(Request_wl_shm_release{}) = do
    dropObject shm.wlid
    sendMessage' request shm.wlid

  runEvent shm (Event_wl_shm_format format) = modifyIORef shm.formats (format :)

instance Interface' Wl_shm Server where
  type Event Wl_shm = Event_wl_shm
  type Request Wl_shm = Request_wl_shm
  runRequest _shm (Request_wl_shm_create_pool poolId fd' size') = do
    ClientServerEnv _ env _ <- ask
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
    void $ newObject poolId $ Wl_shm_pool{wlid = poolId, fd = fd', size = sizeRef, ptr = ptrRef}
  runRequest shm Request_wl_shm_release = do
    ClientServerEnv _ env _ <- ask
    dropObject shm.wlid
  runEvent shm event@(Event_wl_shm_format format) = do
    sendMessage' event shm.wlid

-- }}}

-- Wl_buffer {{{
instance Interface' Wl_buffer Client where
  type Event Wl_buffer = Event_wl_buffer
  type Request Wl_buffer = Request_wl_buffer
  runRequest buffer request@Request_wl_buffer_destroy = do
    dropObject buffer.wlid
    sendMessage' request buffer.wlid
  runEvent buffer Event_wl_buffer_release = pass

instance Interface' Wl_buffer Server where
  type Event Wl_buffer = Event_wl_buffer
  type Request Wl_buffer = Request_wl_buffer
  runRequest buffer Request_wl_buffer_destroy = dropObject buffer.wlid
  runEvent buffer event@Event_wl_buffer_release = do
    sendMessage' event buffer.wlid

-- }}}

-- Wl_data_offer {{{
instance Interface' Wl_data_offer Client where
  type Event Wl_data_offer = Event_wl_data_offer
  type Request Wl_data_offer = Request_wl_data_offer
  runRequest _ (Request_wl_data_offer_accept{}) = pass
  runRequest _ (Request_wl_data_offer_receive{}) = pass
  runRequest data_offer request@(Request_wl_data_offer_destroy{}) = do
    dropObject data_offer.wlid
    sendMessage' request data_offer.wlid
  runRequest _ (Request_wl_data_offer_finish{}) = pass
  runRequest _ (Request_wl_data_offer_set_actions{}) = pass
  runEvent _ (Event_wl_data_offer_offer{}) = pass
  runEvent _ (Event_wl_data_offer_source_actions{}) = pass
  runEvent _ (Event_wl_data_offer_action{}) = pass

instance Interface' Wl_data_offer Server

-- }}}

-- Wl_data_source {{{
instance Interface' Wl_data_source Client where
  type Event Wl_data_source = Event_wl_data_source
  type Request Wl_data_source = Request_wl_data_source
  runRequest _ (Request_wl_data_source_offer{}) = pass
  runRequest data_source request@(Request_wl_data_source_destroy{}) = do
    dropObject data_source.wlid
    sendMessage' request data_source.wlid
  runRequest _ (Request_wl_data_source_set_actions{}) = pass
  runEvent _ (Event_wl_data_source_target{}) = pass
  runEvent _ (Event_wl_data_source_send{}) = pass
  runEvent _ (Event_wl_data_source_cancelled{}) = pass
  runEvent _ (Event_wl_data_source_dnd_drop_performed{}) = pass
  runEvent _ (Event_wl_data_source_dnd_finished{}) = pass
  runEvent _ (Event_wl_data_source_action{}) = pass

instance Interface' Wl_data_source Server

-- }}}

-- Wl_data_device {{{
instance Interface' Wl_data_device Client where
  type Event Wl_data_device = Event_wl_data_device
  type Request Wl_data_device = Request_wl_data_device
  runRequest _ (Request_wl_data_device_start_drag{}) = pass
  runRequest _ (Request_wl_data_device_set_selection{}) = pass
  runRequest _ (Request_wl_data_device_release{}) = pass
  runEvent _ (Event_wl_data_device_data_offer{}) = pass
  runEvent _ (Event_wl_data_device_enter{}) = pass
  runEvent _ (Event_wl_data_device_leave{}) = pass
  runEvent _ (Event_wl_data_device_motion{}) = pass
  runEvent _ (Event_wl_data_device_drop{}) = pass
  runEvent _ (Event_wl_data_device_selection{}) = pass

instance Interface' Wl_data_device Server

-- }}}

-- Wl_data_device_manager {{{
instance Interface' Wl_data_device_manager Client where
  type Event Wl_data_device_manager = Event_wl_data_device_manager
  type Request Wl_data_device_manager = Request_wl_data_device_manager
  runRequest _ (Request_wl_data_device_manager_create_data_source{}) = pass
  runRequest _ (Request_wl_data_device_manager_get_data_device{}) = pass
  runRequest _ (Request_wl_data_device_manager_release{}) = pass

instance Interface' Wl_data_device_manager Server

-- }}}

-- Wl_shell {{{
instance Interface' Wl_shell Client where
  type Event Wl_shell = Event_wl_shell
  type Request Wl_shell = Request_wl_shell
  runRequest _ (Request_wl_shell_get_shell_surface{}) = pass

instance Interface' Wl_shell Server

-- }}}

-- Wl_shell_surface {{{
instance Interface' Wl_shell_surface Client where
  type Event Wl_shell_surface = Event_wl_shell_surface
  type Request Wl_shell_surface = Request_wl_shell_surface
  runRequest _ (Request_wl_shell_surface_pong{}) = pass
  runRequest _ (Request_wl_shell_surface_move{}) = pass
  runRequest _ (Request_wl_shell_surface_resize{}) = pass
  runRequest _ (Request_wl_shell_surface_set_toplevel{}) = pass
  runRequest _ (Request_wl_shell_surface_set_transient{}) = pass
  runRequest _ (Request_wl_shell_surface_set_fullscreen{}) = pass
  runRequest _ (Request_wl_shell_surface_set_popup{}) = pass
  runRequest _ (Request_wl_shell_surface_set_maximized{}) = pass
  runRequest _ (Request_wl_shell_surface_set_title{}) = pass
  runRequest _ (Request_wl_shell_surface_set_class{}) = pass
  runEvent _ (Event_wl_shell_surface_ping{}) = pass
  runEvent _ (Event_wl_shell_surface_configure{}) = pass
  runEvent _ (Event_wl_shell_surface_popup_done{}) = pass

instance Interface' Wl_shell_surface Server

-- }}}

-- Wl_surface {{{
instance Interface' Wl_surface Client where
  type Event Wl_surface = Event_wl_surface
  type Request Wl_surface = Request_wl_surface
  runRequest surface request@(Request_wl_surface_destroy{}) = do
    dropObject surface.wlid
    sendMessage' request surface.wlid
  runRequest surface request@(Request_wl_surface_attach bufferId x y) = do
    atomicModifyIORef surface.pendingState $ \s -> (s {buffer = Just bufferId, offset = Just (x,y)},())
    sendMessage' request surface.wlid
  runRequest surface request@(Request_wl_surface_damage x y w h) = do
    liftIO $ traceIO "New clients should not use this request (wl_surface.damage). Instead damage can be posted with wl_surface.damage_buffer which uses buffer coordinates instead of surface coordinates."
    atomicModifyIORef surface.pendingState $ \s -> (s {damage = Rectangle{position=(x,y), size=(w,h)}:s.damage},())
    sendMessage' request surface.wlid
  runRequest surface request@(Request_wl_surface_frame cb) = do
    atomicModifyIORef surface.pendingState $ \s -> (s {frameCallbacks = cb:s.frameCallbacks},())
    sendMessage' request surface.wlid
  runRequest surface request@(Request_wl_surface_set_opaque_region region) = do
    atomicModifyIORef surface.pendingState $ \s -> (s {opaqueRegion = Just region},())
    sendMessage' request surface.wlid
  runRequest surface request@(Request_wl_surface_set_input_region region) = do
    atomicModifyIORef surface.pendingState $ \s -> (s {inputRegion = Just region},())
    sendMessage' request surface.wlid
  runRequest surface request@(Request_wl_surface_set_buffer_transform transform) = do
    atomicModifyIORef surface.pendingState $ \s -> (s {bufferTransform = Just transform},())
    sendMessage' request surface.wlid
  runRequest surface request@(Request_wl_surface_set_buffer_scale scale) = do
    atomicModifyIORef surface.pendingState $ \s -> (s {bufferScale = Just scale},())
    sendMessage' request surface.wlid
  runRequest surface request@(Request_wl_surface_damage_buffer x y w h) = do
    atomicModifyIORef surface.pendingState $ \s -> (s {damageBuffer = Rectangle{position=(x,y), size=(w,h)}:s.damageBuffer},())
    sendMessage' request surface.wlid
  runRequest surface request@(Request_wl_surface_offset x y) = do
    atomicModifyIORef surface.pendingState $ \s -> ((s :: ContentUpdate) {offset = Just (x,y)},())
    sendMessage' request surface.wlid
  runRequest surface request@(Request_wl_surface_get_release release) = do
    atomicModifyIORef surface.pendingState $ \s -> (s{bufferRelease = Just release},())
    sendMessage' request surface.wlid
  runRequest surface request@Request_wl_surface_commit = do
    cu <- liftIO $ atomicSwapIORef surface.pendingState emptyContentUpdate
    atomicModifyIORef surface.cuQueue $ (,()) . (cu Seq.<|)
    sendMessage' request surface.wlid
  runEvent _ (Event_wl_surface_enter _) = pass
  runEvent _ (Event_wl_surface_leave _) = pass
  runEvent _ (Event_wl_surface_preferred_buffer_scale _) = pass
  runEvent _ (Event_wl_surface_preferred_buffer_transform _) = pass

instance Interface' Wl_surface Server where
  type Event Wl_surface = Event_wl_surface
  type Request Wl_surface = Request_wl_surface
  runRequest surface (Request_wl_surface_attach bufferId x y) =
    atomicModifyIORef surface.pendingState $ \s -> (s {buffer = Just bufferId, offset = Just (x,y)},())
  runRequest surface Request_wl_surface_destroy = dropObject surface.wlid
  runRequest surface (Request_wl_surface_damage x y w h) =
    atomicModifyIORef surface.pendingState $ \s -> (s {damage = Rectangle {position=(x,y), size=(w,h)}:s.damage}, ())
  runRequest surface (Request_wl_surface_damage_buffer x y w h) =
    atomicModifyIORef surface.pendingState $ \s -> (s {damageBuffer = Rectangle {position=(x,y), size=(w,h)}:s.damageBuffer}, ())
  runRequest surface (Request_wl_surface_frame cb) =
    atomicModifyIORef surface.pendingState $ \s -> (s {frameCallbacks = cb:s.frameCallbacks}, ())
  runRequest surface (Request_wl_surface_set_opaque_region region) =
    atomicModifyIORef surface.pendingState $ \s -> (s {opaqueRegion = Just region}, ())
  runRequest surface (Request_wl_surface_set_input_region region) =
    atomicModifyIORef surface.pendingState $ \s -> (s {inputRegion = Just region}, ())
  runRequest surface (Request_wl_surface_set_buffer_scale scale) =
    atomicModifyIORef surface.pendingState $ \s -> (s {bufferScale = Just scale}, ())
  runRequest surface (Request_wl_surface_set_buffer_transform transform') =
    atomicModifyIORef surface.pendingState $ \s -> (s {bufferTransform = Just transform'}, ())
  runRequest surface (Request_wl_surface_offset x y) =
    atomicModifyIORef surface.pendingState $ \s -> ((s :: ContentUpdate) {offset = Just (x,y)}, ())
  runRequest surface (Request_wl_surface_get_release release) =
    atomicModifyIORef surface.pendingState $ \s -> ((s :: ContentUpdate) {bufferRelease = Just release}, ())
  runRequest surface Request_wl_surface_commit = liftIO $ do
    cu <- atomicSwapIORef surface.pendingState emptyContentUpdate
    atomicModifyIORef' surface.cuQueue $ (,()) . (cu Seq.<|)
  runEvent _surface (Event_wl_surface_enter _) = pass
  runEvent _surface (Event_wl_surface_leave _) = pass
  runEvent _surface (Event_wl_surface_preferred_buffer_scale _) = pass
  runEvent _surface (Event_wl_surface_preferred_buffer_transform _) = pass

-- }}}

-- Wl_seat {{{
instance Interface' Wl_seat Client where
  type Event Wl_seat = Event_wl_seat
  type Request Wl_seat = Request_wl_seat
  runRequest _ (Request_wl_seat_get_pointer{}) = pass
  runRequest _ (Request_wl_seat_get_keyboard{}) = pass
  runRequest _ (Request_wl_seat_get_touch{}) = pass
  runRequest _ (Request_wl_seat_release{}) = pass
  runEvent _ (Event_wl_seat_capabilities _) = pass
  runEvent _ (Event_wl_seat_name _) = pass

instance Interface' Wl_seat Server

-- }}}

-- Wl_pointer {{{
instance Interface' Wl_pointer Client where
  type Event Wl_pointer = Event_wl_pointer
  type Request Wl_pointer = Request_wl_pointer
  runRequest _ (Request_wl_pointer_set_cursor{}) = pass
  runRequest _ (Request_wl_pointer_release{}) = pass
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

instance Interface' Wl_pointer Server

-- }}}

-- Wl_keyboard {{{
instance Interface' Wl_keyboard Client where
  type Event Wl_keyboard = Event_wl_keyboard
  type Request Wl_keyboard = Request_wl_keyboard
  runRequest _ (Request_wl_keyboard_release{}) = pass
  runEvent _ (Event_wl_keyboard_keymap{}) = pass
  runEvent _ (Event_wl_keyboard_enter{}) = pass
  runEvent _ (Event_wl_keyboard_leave{}) = pass
  runEvent _ (Event_wl_keyboard_key{}) = pass
  runEvent _ (Event_wl_keyboard_modifiers{}) = pass
  runEvent _ (Event_wl_keyboard_repeat_info{}) = pass

instance Interface' Wl_keyboard Server

-- }}}

-- Wl_touch {{{
instance Interface' Wl_touch Client where
  type Event Wl_touch = Event_wl_touch
  type Request Wl_touch = Request_wl_touch
  runRequest _ (Request_wl_touch_release{}) = pass
  runEvent _ (Event_wl_touch_down{}) = pass
  runEvent _ (Event_wl_touch_up{}) = pass
  runEvent _ (Event_wl_touch_motion{}) = pass
  runEvent _ (Event_wl_touch_frame{}) = pass
  runEvent _ (Event_wl_touch_cancel{}) = pass
  runEvent _ (Event_wl_touch_shape{}) = pass
  runEvent _ (Event_wl_touch_orientation{}) = pass

instance Interface' Wl_touch Server

-- }}}

-- Wl_output {{{
instance Interface' Wl_output Client where
  type Event Wl_output = Event_wl_output
  type Request Wl_output = Request_wl_output
  runRequest _ (Request_wl_output_release{}) = pass
  runEvent _ (Event_wl_output_geometry{}) = pass
  runEvent _ (Event_wl_output_mode{}) = pass
  runEvent _ (Event_wl_output_done{}) = pass
  runEvent _ (Event_wl_output_scale{}) = pass
  runEvent _ (Event_wl_output_name{}) = pass
  runEvent _ (Event_wl_output_description{}) = pass

instance Interface' Wl_output Server

-- }}}

-- Wl_region {{{
instance Interface' Wl_region Client where
  type Event Wl_region = Event_wl_region
  type Request Wl_region = Request_wl_region
  runRequest _ (Request_wl_region_destroy{}) = pass
  runRequest _ (Request_wl_region_add{}) = pass
  runRequest _ (Request_wl_region_subtract{}) = pass
  runEvent _ _ = pass

instance Interface' Wl_region Server where
  type Event Wl_region = Event_wl_region
  type Request Wl_region = Request_wl_region
  runRequest _ (Request_wl_region_destroy{}) = pass
  runRequest _ (Request_wl_region_add{}) = pass
  runRequest _ (Request_wl_region_subtract{}) = pass
  runEvent _ _ = pass

-- }}}

-- Wl_subcompositor {{{
instance Interface' Wl_subcompositor Client where
  type Event Wl_subcompositor = Event_wl_subcompositor
  type Request Wl_subcompositor = Request_wl_subcompositor
  runRequest subcompositor request@Request_wl_subcompositor_destroy = do
    dropObject subcompositor.wlid
    sendMessage' request subcompositor.wlid
  runRequest _ (Request_wl_subcompositor_get_subsurface subsurface surface' parent') = do
    obj <- (wlid .~ subsurface) . (surface .~ surface') . (parent .~ parent') <$> (defM :: Wayland Client Wl_subsurface)
    void $ newObject subsurface obj 
  runEvent _ _ = pass

instance Interface' Wl_subcompositor Server where
  type Event Wl_subcompositor = Event_wl_subcompositor
  type Request Wl_subcompositor = Request_wl_subcompositor
  runEvent _ _ = pass
  runRequest subcompositor Request_wl_subcompositor_destroy = dropObject subcompositor.wlid
  runRequest _subcompositor (Request_wl_subcompositor_get_subsurface subsurface surface' parent') = do
    obj <- (wlid .~ subsurface) . (surface .~ surface') . (parent .~ parent') <$> (defM :: Wayland Server Wl_subsurface)
    void $ newObject subsurface obj

-- }}}

-- Wl_subsurface {{{

{-
insertSibling :: TObjectID Wl_surface -> TObjectID Wl_surface -> Bool -> SurfaceState -> (SurfaceState, Bool)
insertSibling what relative_to isabove state'@SurfaceState{sSubsurfaces = SubsurfaceStack{below, above}} = (state'{sSubsurfaces=SubsurfaceStack{below=below',above=above'}},ab || bb)
  where
    (below', bb) = insertSibling' below
    (above', ab) = insertSibling' above
    insertSibling' :: Seq.Seq (TObjectID Wl_surface) -> (Seq.Seq (TObjectID Wl_surface), Bool)
    insertSibling' seq' = case i of
      Just x -> ((Seq.take x' seq' Seq.:|> what) <> Seq.drop x' seq', True)
        where x' = bool x (x + 1) isabove
      Nothing-> (seq', False)
      where
        i = Seq.findIndexL (== relative_to) seq'
-}

instance Interface' Wl_subsurface Client where
  type Event Wl_subsurface = Event_wl_subsurface
  type Request Wl_subsurface = Request_wl_subsurface
  runRequest subsurface request@Request_wl_subsurface_destroy = do
    dropObject subsurface.wlid
    sendMessage' request subsurface.wlid
  runRequest subsurface request@(Request_wl_subsurface_set_position x y) = do
    atomicWriteIORef subsurface.position (x,y)
    sendMessage' request subsurface.wlid
  runRequest subsurface request@(Request_wl_subsurface_place_below sibling) = do
    parentSurface' <- getInterface subsurface.parent
    case parentSurface' of
      Nothing -> error "invalid parent surface"
      Just parentSurface -> do
        b <- atomicModifyIORef parentSurface.state $ \s ->
          let joint = s.sSubsurfaces.below <> s.sSubsurfaces.above
              (before, after) = Seq.breakl (== sibling) joint
              joint2 = (before Seq.|> subsurface.surface) <> after
              (below, above) = Seq.breakl (== subsurface.parent) joint2
          in bool (s {sSubsurfaces = SubsurfaceStack{below, above}}, True) (s, False) (after == Seq.empty)
        unless b $ error "bad_surface: wl_surface is not a sibling or the parent"
    sendMessage' request subsurface.wlid
  runRequest subsurface request@(Request_wl_subsurface_place_above sibling) = do
    parentSurface' <- getInterface subsurface.parent
    case parentSurface' of
      Nothing -> error "invalid parent surface"
      Just parentSurface -> do
        b <- atomicModifyIORef parentSurface.state $ \s ->
          let joint = s.sSubsurfaces.below <> s.sSubsurfaces.above
              (before, after) = Seq.breakl (== sibling) joint
              joint2 = (before Seq.|> fromJust (after Seq.!? 0) Seq.|> subsurface.surface) <> Seq.drop 1 after
              (below, above) = Seq.breakl (== subsurface.parent) joint2
          in bool (s {sSubsurfaces = SubsurfaceStack{below, above}}, True) (s, False) (after == Seq.empty)
        unless b $ error "bad_surface: wl_surface is not a sibling or the parent"
    sendMessage' request subsurface.wlid
  runRequest subsurface request@Request_wl_subsurface_set_sync = do
    atomicWriteIORef subsurface.synchronized True
    sendMessage' request subsurface.wlid
  runRequest subsurface request@Request_wl_subsurface_set_desync = do
    atomicWriteIORef subsurface.synchronized False
    sendMessage' request subsurface.wlid
  runEvent _ _ = pass

instance Interface' Wl_subsurface Server where
  type Event Wl_subsurface = Event_wl_subsurface
  type Request Wl_subsurface = Request_wl_subsurface
  runRequest subsurface Request_wl_subsurface_destroy = dropObject subsurface.wlid
  runRequest subsurface (Request_wl_subsurface_set_position x y) = atomicWriteIORef subsurface.position (x,y)
  runRequest subsurface (Request_wl_subsurface_place_below sibling) = do
    parentSurface' <- getInterface subsurface.parent
    case parentSurface' of
      Nothing -> error "invalid parent surface"
      Just parentSurface -> do
        b <- atomicModifyIORef parentSurface.state $ \s ->
          let joint = s.sSubsurfaces.below <> s.sSubsurfaces.above
              (before, after) = Seq.breakl (== sibling) joint
              joint2 = (before Seq.|> subsurface.surface) <> after
              (below, above) = Seq.breakl (== subsurface.parent) joint2
          in bool (s {sSubsurfaces = SubsurfaceStack{below, above}}, True) (s, False) (after == Seq.empty)
        unless b $ sendError subsurface.wlid 0 "wl_surface is not a sibling or the parent"
  runRequest subsurface (Request_wl_subsurface_place_above sibling) = do
    parentSurface' <- getInterface subsurface.parent
    case parentSurface' of
      Nothing -> error "invalid parent surface"
      Just parentSurface -> do
        b <- atomicModifyIORef parentSurface.state $ \s ->
          let joint = s.sSubsurfaces.below <> s.sSubsurfaces.above
              (before, after) = Seq.breakl (== sibling) joint
              joint2 = (before Seq.|> fromJust (after Seq.!? 0) Seq.|> subsurface.surface) <> Seq.drop 1 after
              (below, above) = Seq.breakl (== subsurface.parent) joint2
          in bool (s {sSubsurfaces = SubsurfaceStack{below, above}}, True) (s, False) (after == Seq.empty)
        unless b $ sendError subsurface.wlid 0 "wl_surface is not a sibling or the parent"
  runRequest subsurface (Request_wl_subsurface_set_sync{}) = atomicWriteIORef subsurface.synchronized True
  runRequest subsurface (Request_wl_subsurface_set_desync{}) = atomicWriteIORef subsurface.synchronized False
  runEvent _ _ = pass

-- }}}

-- Wl_fixes {{{
instance Interface' Wl_fixes Client where
  type Event Wl_fixes = Event_wl_fixes
  type Request Wl_fixes = Request_wl_fixes
  runRequest _ (Request_wl_fixes_destroy{}) = pass
  runRequest _ (Request_wl_fixes_destroy_registry{}) = pass

instance Interface' Wl_fixes Server

-- }}}

-- }}}

-- Wrapper Functions, for QoL {{{
bindToInterface :: Wl_registry -> BS.ByteString -> Wayland Client (Maybe Word32)
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
          runRequest registry (Request_wl_registry_bind x(intName, ver, new_id))
          pure $ Just new_id
        Nothing -> liftIO (threadDelay $ 100 * 1000) >> go (count + 1)

-- }}}
-- vim: foldmethod=marker
