{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Sayland.Protocols.Wayland (module Sayland.Protocols.Wayland) where

-- Module implementing some interfaces using classes defined by the `Protocol` module.

import Control.Concurrent (threadDelay)
import Control.Exception (try)
import Data.Bimap qualified as BM
import Data.Data (cast)
import Data.Map qualified as Map
import Data.Maybe (fromJust)
import Data.Sequence qualified as Seq
import Debug.Trace (traceIO)
import Foreign (Ptr, nullPtr)
import GHC.IORef (atomicSwapIORef)
import MMAP (mapShared, mkMmapFlags, mmap, munmap, protRead, protWrite)
import Relude hiding (get, state)
import Relude.Extra.Tuple (dup)
import Sayland.Codegen
import Sayland.Internal.Utils
import Sayland.Types
import Sayland.Utils
import Sayland.Wire.Types
import System.Posix (Fd, setFdSize)

$(loadProtocolFileEnums False "protocols/wayland.xml")

-- | Constant representing the wl_display ID which is always 1 in Wayland.
wlDisplayId :: TObjectID Wl_display
wlDisplayId = 1

-- Nothing or empty list means no change. In order to "reset" values, set them to the defaults - ObjectID `0`, normal transform, etc.
data ContentUpdate = ContentUpdate
  { cuSurface :: TObjectID Wl_surface
  , cuBuffer :: Maybe (TObjectID Wl_buffer)
  , cuOffset :: Maybe (Int32, Int32)
  , cuDamage :: [Rectangle]
  , cuDamageBuffer :: [Rectangle]
  , cuFrameCallbacks :: [TObjectID Wl_callback]
  , cuOpaqueRegion :: Maybe (TObjectID Wl_region)
  , cuInputRegion :: Maybe (TObjectID Wl_region)
  , cuBufferScale :: Maybe Int32
  , cuBufferTransform :: Maybe Enum_wl_output_transform
  , cuBufferRelease :: Maybe (TObjectID Wl_callback)
  , cuSubsurfaces :: Maybe SubsurfaceStack
  , cuFifoBarrier :: Bool
  , cuFifoWaitBarrier :: Bool
  , cuSlaveCUs :: [ContentUpdate]
  }
  deriving stock (Eq, Ord)

data SurfaceRole where SurfaceRole :: (Typeable a) => a -> SurfaceRole

-- Used both as a template for content updates, and to indicate no change.
emptyContentUpdate :: ContentUpdate
emptyContentUpdate =
  ContentUpdate
    { cuSurface = 0
    , cuBuffer = Nothing
    , cuOffset = Just (0, 0)
    , cuDamage = []
    , cuDamageBuffer = []
    , cuFrameCallbacks = []
    , cuOpaqueRegion = Nothing
    , cuInputRegion = Nothing
    , cuBufferScale = Nothing
    , cuBufferTransform = Nothing
    , cuBufferRelease = Nothing
    , cuSubsurfaces = Nothing
    , cuFifoBarrier = False
    , cuFifoWaitBarrier = False
    , cuSlaveCUs = []
    }

-- Interfaces {{{
newtype Wl_display = Wl_display {wlid :: TObjectID Wl_display}

newtype Wl_registry = Wl_registry {wlid :: TObjectID Wl_registry}

data Wl_callback = Wl_callback {wlid :: TObjectID Wl_callback, done :: MVar ()}

newtype Wl_compositor = Wl_compositor {wlid :: TObjectID Wl_compositor}

data Wl_shm_pool = Wl_shm_pool {wlid :: TObjectID Wl_shm_pool, fd :: Fd, size :: IORef WlInt, ptr :: IORef (Ptr ())}

data Wl_shm = Wl_shm {wlid :: TObjectID Wl_shm, formats :: IORef [Enum_wl_shm_format]}

data Wl_buffer = Wl_buffer
  { wlid :: TObjectID Wl_buffer
  , offset :: WlInt
  , width :: WlInt
  , height :: WlInt
  , stride :: WlInt
  , pool :: TObjectID Wl_shm_pool
  , format :: Enum_wl_shm_format
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
  deriving stock (Eq, Ord)

data SurfaceState = SurfaceState
  { sBuffer :: TObjectID Wl_buffer
  , sBufferOffset :: (Int, Int)
  , sDamage :: [Rectangle]
  , sCallbacks :: [TObjectID Wl_callback]
  , sOpaqueRegion :: TObjectID Wl_region
  , sInputRegion :: TObjectID Wl_region
  , sBufferScale :: Int
  , sBufferTransform :: Enum_wl_output_transform
  , sSubsurfaces :: SubsurfaceStack
  , sFifoBarrier :: Bool
  }

data Wl_surface = Wl_surface
  { wlid :: TObjectID Wl_surface
  , pendingState :: IORef ContentUpdate
  , cuQueue :: IORef (Seq.Seq ContentUpdate)
  , role :: IORef SurfaceRole
  , state :: IORef SurfaceState
  }

data Wl_subsurface = Wl_subsurface
  { wlid :: TObjectID Wl_subsurface
  , surface :: TObjectID Wl_surface
  , parent :: TObjectID Wl_surface
  , position :: IORef (Int32, Int32)
  , synchronized :: IORef Bool
  }

newtype Wl_seat = Wl_seat {wlid :: TObjectID Wl_seat}

newtype Wl_pointer = Wl_pointer {wlid :: TObjectID Wl_pointer}

newtype Wl_keyboard = Wl_keyboard {wlid :: TObjectID Wl_keyboard}

newtype Wl_touch = Wl_touch {wlid :: TObjectID Wl_touch}

newtype Wl_output = Wl_output {wlid :: TObjectID Wl_output}

newtype Wl_subcompositor = Wl_subcompositor {wlid :: TObjectID Wl_subcompositor}

newtype Wl_fixes = Wl_fixes {wlid :: TObjectID Wl_fixes}

$(loadProtocolFile wlFormatter False "protocols/wayland.xml")

-- NewInterface instances {{{

instance NewInterface Wl_buffer where
  newInterface i = pure Wl_buffer{wlid = i, offset = 0, width = 0, height = 0, stride = 0, format = Enum_wl_shm_format_argb8888, pool = 0}

instance NewInterface Wl_region where
  newInterface i = do
    included <- newIORef []
    excluded <- newIORef []
    pure Wl_region{wlid = i, included, excluded}

instance NewInterface Wl_callback where
  newInterface _i = newEmptyMVar <&> Wl_callback 0

instance NewInterface Wl_shm_pool where
  newInterface i = do
    ref <- newIORef 0
    ptrRef <- newIORef nullPtr
    pure $ Wl_shm_pool i 0 ref ptrRef

instance NewInterface Wl_shm where
  newInterface i = newIORef [] <&> Wl_shm i

instance NewInterface Wl_surface where
  newInterface i = do
    let wlid :: TObjectID Wl_surface = i
    role <- newIORef $ SurfaceRole ()
    pendingState <- newIORef emptyContentUpdate
    cuQueue <- newIORef Seq.Empty
    state <-
      newIORef
        $ SurfaceState
          { sBuffer = 0
          , sBufferOffset = (0, 0)
          , sDamage = []
          , sCallbacks = []
          , sOpaqueRegion = 0
          , sInputRegion = 0
          , sBufferScale = 1
          , sBufferTransform = Enum_wl_output_transform_normal
          , sSubsurfaces = SubsurfaceStack{above = Seq.empty, below = Seq.empty}
          , sFifoBarrier = False
          }
    pure Wl_surface{..}

instance NewInterface Wl_subsurface where
  newInterface i = do
    position <- newIORef (0, 0)
    synchronized <- newIORef True
    pure Wl_subsurface{wlid = i, surface = 0, parent = 0, position, synchronized}

-- }}}
-- Tables {{{
$(generateTables False wlFormatter "protocols/wayland.xml")

-- }}}
-- }}}

-- | function that removes interface behind the provided id from the object map AND, if running on server, sends the delete_id event.
dropObject :: TObjectID a -> Wayland p ()
dropObject (TObjectID i) =
  ask >>= \case
    ClientEnv env -> modifyIORef env.objects $ Map.delete i
    ClientServerEnv _ env _ -> do
      void $ atomicModifyIORef' env.objects (dup . Map.delete i)
      Just wldisplay <- getInterface wlDisplayId
      runEvent wldisplay $ Event_wl_display_delete_id i

-- | send an error message to the client.
sendError :: TObjectID a -> ObjectID -> WlString -> Wayland Server ()
sendError (TObjectID i) code msg = do
  Just wldisplay <- getInterface wlDisplayId
  runEvent wldisplay $ Event_wl_display_error i code msg

-- Interface Implementations {{{

-- Wl_display {{{
instance Interface' Wl_display Client where
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
  runEvent display event@(Event_wl_display_delete_id did) = do
    ClientServerEnv _ env _ <- ask
    liftIO $ modifyIORef env.objects (Map.delete did)
    sendMessage' event display.wlid
  runEvent display event@(Event_wl_display_error _object_id _code _message) = do
    sendMessage' event display.wlid
  runRequest _display (Request_wl_display_sync callback) = do
    mvar <- newEmptyMVar
    callbackObject <- newObject callback Wl_callback{wlid = callback, done = mvar}
    -- before calling this event, the compositor must process all previous requests. In a single-threaded compositor it is a no-op.
    -- in a multi-threaded compositor this becomes a problem.
    runEvent callbackObject $ Event_wl_callback_done 0
  runRequest _display (Request_wl_display_get_registry registry) = do
    ClientServerEnv _ env _ <- ask
    versions <- zip [0 ..] . Map.toList <$> readIORef env.versionTable
    _registry <- newObject registry Wl_registry{wlid = registry}
    forM_ versions $ \(name', (interface, version)) -> do
      let name = WlUInt name'
          event = Event_wl_registry_global name interface version
      sendMessage' event registry
      modifyIORef env.globals $ BM.insert interface (coerce name)

-- }}}

-- Wl_callback {{{
instance Interface' Wl_callback Client where
  runEvent callback (Event_wl_callback_done _callback_data) = do
    ClientEnv _env <- ask
    putMVar callback.done ()
    dropObject callback.wlid

  runRequest _ _ = pass

instance Interface' Wl_callback Server where
  runEvent callback event@(Event_wl_callback_done _callback_data) = do
    putMVar callback.done ()
    sendMessage' event callback.wlid
    dropObject callback.wlid
  runRequest _ _ = pass

-- }}}

-- Wl_registry {{{
instance Interface' Wl_registry Client where
  runEvent _registry (Event_wl_registry_global name interface version) = do
    ClientEnv env <- ask
    modifyIORef env.globals $ BM.insert interface (coerce name)
    vertable <- readIORef env.versionTable
    case Map.lookup interface vertable of
      Just clientVer -> do
        when (clientVer > version)
          $ modifyIORef env.versionTable
          $ Map.insert interface version
      Nothing -> pass
  runEvent _registry (Event_wl_registry_global_remove name) = do
    ClientEnv env <- ask
    modifyIORef env.globals $ BM.deleteR (coerce name)

  runRequest registry request@(Request_wl_registry_bind name (WlNewId _ _ newId)) = do
    ClientEnv env <- ask
    interfaceFromName (coerce name) >>= \case
      Just x -> do
        y' <- fromJust . Map.lookup x <$> readIORef env.interfaceTable
        Interface y <- liftIO (y' newId)
        void $ newObject (TObjectID newId) y
      Nothing -> error $ "interface with name `" <> show name <> "` not found."
    sendMessage' request registry.wlid

instance Interface' Wl_registry Server where
  runEvent registry event@(Event_wl_registry_global name interface _version) = do
    ClientServerEnv _ env _ <- ask
    modifyIORef env.globals $ BM.insert interface (coerce name)
    sendMessage' event registry.wlid
  runEvent registry event@(Event_wl_registry_global_remove name) = do
    ClientServerEnv _ env _ <- ask
    modifyIORef env.globals $ BM.deleteR (coerce name)
    sendMessage' event registry.wlid
  runRequest _registry (Request_wl_registry_bind name (WlNewId _ _ newId)) = do
    ClientServerEnv _ env _ <- ask
    interfaceFromName (coerce name) >>= \case
      Just x -> do
        y' <- fromJust . Map.lookup x <$> readIORef env.interfaceTable
        Interface y <- liftIO (y' newId)
        void $ newObject (TObjectID newId) y
      Nothing -> error $ "interface with name `" <> show name <> "` not found."

-- }}}

-- Wl_compositor {{{
instance Interface' Wl_compositor Client where
  runRequest compositor request@(Request_wl_compositor_create_surface surfaceId) = do
    void $ newObject surfaceId =<< (newInterface surfaceId :: Wayland Client Wl_surface)
    sendMessage' request compositor.wlid
  runRequest compositor request@(Request_wl_compositor_create_region regionId) = do
    void $ newObject regionId =<< (newInterface regionId :: Wayland Client Wl_region)
    sendMessage' request compositor.wlid
  runRequest compositor request@Request_wl_compositor_release = do
    sendMessage' request compositor.wlid
    dropObject compositor.wlid
  runEvent _ _ = pass

instance Interface' Wl_compositor Server where
  runEvent _ _ = pass
  runRequest _compositor (Request_wl_compositor_create_surface surfaceId) = void $ newObject surfaceId =<< newInterface surfaceId
  runRequest _compositor (Request_wl_compositor_create_region regionId) = void $ newObject regionId =<< newInterface regionId
  runRequest compositor Request_wl_compositor_release = dropObject compositor.wlid

-- }}}

-- Wl_shm_pool {{{
instance Interface' Wl_shm_pool Client where
  runRequest shm_pool request@(Request_wl_shm_pool_create_buffer bufId offset' width' height' stride' format') = do
    let buffer = Wl_buffer{wlid = bufId, offset = offset', width = width', height = height', stride = stride', format = format', pool = shm_pool.wlid}
    void $ newObject bufId buffer
    sendMessage' request shm_pool.wlid
  runRequest shm_pool request@Request_wl_shm_pool_destroy = do
    sendMessage' request shm_pool.wlid
    dropObject shm_pool.wlid
  runRequest shm_pool request@(Request_wl_shm_pool_resize size') = do
    writeIORef shm_pool.size size'
    sendMessage' request shm_pool.wlid

  runEvent _ _ = pass

instance Interface' Wl_shm_pool Server where
  runRequest shm_pool (Request_wl_shm_pool_create_buffer bufId offset width height stride format) = do
    ClientServerEnv{} <- ask
    let buffer = Wl_buffer{wlid = bufId, offset = offset, width = width, height = height, stride = stride, format = format, pool = shm_pool.wlid}
    void $ newObject bufId buffer
  runRequest shm_pool request@Request_wl_shm_pool_destroy = do
    sendMessage' request shm_pool.wlid
    dropObject shm_pool.wlid
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
  runRequest shm request@(Request_wl_shm_create_pool poolId (WlFd fd) size) = do
    sizeRef <- newIORef size
    ptrRef <- newIORef nullPtr {-IIRC client doesn't need exposed -}
    void $ newObject poolId $ Wl_shm_pool{wlid = poolId, fd = fd, size = sizeRef, ptr = ptrRef}
    sendMessage' request shm.wlid
  runRequest shm request@(Request_wl_shm_release{}) = do
    sendMessage' request shm.wlid
    dropObject shm.wlid

  runEvent shm (Event_wl_shm_format format) = modifyIORef shm.formats (format :)

instance Interface' Wl_shm Server where
  runRequest _shm (Request_wl_shm_create_pool poolId (WlFd fd) size') = do
    ClientServerEnv{} <- ask
    result <-
      liftIO
        $ try
        $ mmap
          nullPtr
          (fromIntegral size')
          (protRead <> protWrite)
          (mkMmapFlags mapShared mempty)
          fd
          0
    ptr' <- case result of
      Left (e :: SomeException) -> liftIO (traceIO $ "mmap failed: " ++ show e) >> undefined
      Right ptr' -> liftIO (traceIO $ "mmap OK, ptr = " ++ show ptr') $> ptr'
    sizeRef <- newIORef size'
    ptrRef <- newIORef ptr'
    void $ newObject poolId $ Wl_shm_pool{wlid = poolId, fd = fd, size = sizeRef, ptr = ptrRef}
  runRequest shm Request_wl_shm_release = do
    ClientServerEnv{} <- ask
    dropObject shm.wlid
  runEvent shm event@(Event_wl_shm_format _format) = do
    sendMessage' event shm.wlid

-- }}}

-- Wl_buffer {{{
instance Interface' Wl_buffer Client where
  runRequest buffer request@Request_wl_buffer_destroy = do
    sendMessage' request buffer.wlid
    dropObject buffer.wlid
  runEvent _buffer Event_wl_buffer_release = pass

instance Interface' Wl_buffer Server where
  runRequest buffer Request_wl_buffer_destroy = dropObject buffer.wlid
  runEvent buffer event@Event_wl_buffer_release = do
    sendMessage' event buffer.wlid

-- }}}

-- Wl_data_offer {{{
instance Interface' Wl_data_offer Client where
  runRequest _ (Request_wl_data_offer_accept{}) = pass
  runRequest _ (Request_wl_data_offer_receive{}) = pass
  runRequest data_offer request@(Request_wl_data_offer_destroy{}) = do
    sendMessage' request data_offer.wlid
    dropObject data_offer.wlid
  runRequest _ (Request_wl_data_offer_finish{}) = pass
  runRequest _ (Request_wl_data_offer_set_actions{}) = pass
  runEvent _ (Event_wl_data_offer_offer{}) = pass
  runEvent _ (Event_wl_data_offer_source_actions{}) = pass
  runEvent _ (Event_wl_data_offer_action{}) = pass

instance Interface' Wl_data_offer Server

-- }}}

-- Wl_data_source {{{
instance Interface' Wl_data_source Client where
  runRequest _ (Request_wl_data_source_offer{}) = pass
  runRequest data_source request@(Request_wl_data_source_destroy{}) = do
    sendMessage' request data_source.wlid
    dropObject data_source.wlid
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
  runRequest _ (Request_wl_data_device_manager_create_data_source{}) = pass
  runRequest _ (Request_wl_data_device_manager_get_data_device{}) = pass
  runRequest _ (Request_wl_data_device_manager_release{}) = pass

instance Interface' Wl_data_device_manager Server

-- }}}

-- Wl_shell {{{
instance Interface' Wl_shell Client where
  runRequest _ (Request_wl_shell_get_shell_surface{}) = pass

instance Interface' Wl_shell Server

-- }}}

-- Wl_shell_surface {{{
instance Interface' Wl_shell_surface Client where
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
  runRequest surface' request@(Request_wl_surface_destroy{}) = do
    sendMessage' request surface'.wlid
    dropObject surface'.wlid
  runRequest surface' request@(Request_wl_surface_attach bufferId (WlInt x) (WlInt y)) = do
    atomicModifyIORef surface'.pendingState $ \s -> (s{cuBuffer = Just bufferId, cuOffset = Just (x, y)}, ())
    sendMessage' request surface'.wlid
  runRequest surface' request@(Request_wl_surface_damage (WlInt x) (WlInt y) (WlInt w) (WlInt h)) = do
    liftIO $ traceIO "New clients should not use this request (wl_surface.damage). Instead damage can be posted with wl_surface.damage_buffer which uses buffer coordinates instead of surface' coordinates."
    atomicModifyIORef surface'.pendingState $ \s -> (s{cuDamage = Rectangle{position = (x, y), size = (w, h)} : s.cuDamage}, ())
    sendMessage' request surface'.wlid
  runRequest surface' request@(Request_wl_surface_frame cb) = do
    atomicModifyIORef surface'.pendingState $ \s -> (s{cuFrameCallbacks = cb : s.cuFrameCallbacks}, ())
    sendMessage' request surface'.wlid
  runRequest surface' request@(Request_wl_surface_set_opaque_region region) = do
    atomicModifyIORef surface'.pendingState $ \s -> (s{cuOpaqueRegion = Just region}, ())
    sendMessage' request surface'.wlid
  runRequest surface' request@(Request_wl_surface_set_input_region region) = do
    atomicModifyIORef surface'.pendingState $ \s -> (s{cuInputRegion = Just region}, ())
    sendMessage' request surface'.wlid
  runRequest surface' request@(Request_wl_surface_set_buffer_transform transform) = do
    atomicModifyIORef surface'.pendingState $ \s -> (s{cuBufferTransform = Just transform}, ())
    sendMessage' request surface'.wlid
  runRequest surface' request@(Request_wl_surface_set_buffer_scale (WlInt scale)) = do
    atomicModifyIORef surface'.pendingState $ \s -> (s{cuBufferScale = Just scale}, ())
    sendMessage' request surface'.wlid
  runRequest surface' request@(Request_wl_surface_damage_buffer (WlInt x) (WlInt y) (WlInt w) (WlInt h)) = do
    atomicModifyIORef surface'.pendingState $ \s -> (s{cuDamageBuffer = Rectangle{position = (x, y), size = (w, h)} : s.cuDamageBuffer}, ())
    sendMessage' request surface'.wlid
  runRequest surface' request@(Request_wl_surface_offset (WlInt x) (WlInt y)) = do
    atomicModifyIORef surface'.pendingState $ \s -> (s{cuOffset = Just (x, y)}, ())
    sendMessage' request surface'.wlid
  runRequest surface' request@(Request_wl_surface_get_release release) = do
    atomicModifyIORef surface'.pendingState $ \s -> (s{cuBufferRelease = Just release}, ())
    sendMessage' request surface'.wlid
  runRequest surface' request@Request_wl_surface_commit = do
    cu <- liftIO $ atomicSwapIORef surface'.pendingState emptyContentUpdate{cuSurface = surface'.wlid}
    atomicModifyIORef surface'.cuQueue $ (,()) . (cu{cuSurface = surface'.wlid} Seq.<|)
    SurfaceRole role <- readIORef surface'.role
    case cast role of
      Just (x :: Wl_subsurface) -> do
        sync <- readIORef x.synchronized
        when sync $ getInterface x.surface >>= \case
          Just x' -> atomicModifyIORef x'.pendingState $ \s -> (s{cuSlaveCUs = cu : s.cuSlaveCUs}, ())
          Nothing -> writeIORef surface'.role $ SurfaceRole ()
      Nothing -> pass
    sendMessage' request surface'.wlid
  runEvent _ (Event_wl_surface_enter _) = pass
  runEvent _ (Event_wl_surface_leave _) = pass
  runEvent _ (Event_wl_surface_preferred_buffer_scale _) = pass
  runEvent _ (Event_wl_surface_preferred_buffer_transform _) = pass

instance Interface' Wl_surface Server where
  runRequest surface (Request_wl_surface_attach bufferId (WlInt x) (WlInt y)) =
    atomicModifyIORef surface.pendingState $ \s -> (s{cuBuffer = Just bufferId, cuOffset = Just (x, y)}, ())
  runRequest surface Request_wl_surface_destroy = dropObject surface.wlid
  runRequest surface (Request_wl_surface_damage (WlInt x) (WlInt y) (WlInt w) (WlInt h)) =
    atomicModifyIORef surface.pendingState $ \s -> (s{cuDamage = Rectangle{position = (x, y), size = (w, h)} : s.cuDamage}, ())
  runRequest surface (Request_wl_surface_damage_buffer (WlInt x) (WlInt y) (WlInt w) (WlInt h)) =
    atomicModifyIORef surface.pendingState $ \s -> (s{cuDamageBuffer = Rectangle{position = (x, y), size = (w, h)} : s.cuDamageBuffer}, ())
  runRequest surface (Request_wl_surface_frame cb) =
    atomicModifyIORef surface.pendingState $ \s -> (s{cuFrameCallbacks = cb : s.cuFrameCallbacks}, ())
  runRequest surface (Request_wl_surface_set_opaque_region region) =
    atomicModifyIORef surface.pendingState $ \s -> (s{cuOpaqueRegion = Just region}, ())
  runRequest surface (Request_wl_surface_set_input_region region) =
    atomicModifyIORef surface.pendingState $ \s -> (s{cuInputRegion = Just region}, ())
  runRequest surface (Request_wl_surface_set_buffer_scale (WlInt scale)) =
    atomicModifyIORef surface.pendingState $ \s -> (s{cuBufferScale = Just scale}, ())
  runRequest surface (Request_wl_surface_set_buffer_transform transform') =
    atomicModifyIORef surface.pendingState $ \s -> (s{cuBufferTransform = Just transform'}, ())
  runRequest surface (Request_wl_surface_offset (WlInt x) (WlInt y)) =
    atomicModifyIORef surface.pendingState $ \s -> (s{cuOffset = Just (x, y)}, ())
  runRequest surface (Request_wl_surface_get_release release) =
    atomicModifyIORef surface.pendingState $ \s -> ((s :: ContentUpdate){cuBufferRelease = Just release}, ())
  runRequest surface Request_wl_surface_commit = do
    cu <- liftIO $ atomicSwapIORef surface.pendingState emptyContentUpdate{cuSurface = surface.wlid}
    SurfaceRole role <- readIORef surface.role
    case cast role of
      Just (x :: Wl_subsurface) -> do
        sync <- readIORef x.synchronized
        when sync $ getInterface x.surface >>= \case
          Just x' -> atomicModifyIORef x'.pendingState $ \s -> (s{cuSlaveCUs = cu : s.cuSlaveCUs}, ())
          Nothing -> writeIORef surface.role $ SurfaceRole ()
      Nothing -> pass
    atomicModifyIORef' surface.cuQueue $ (,()) . (cu{cuSurface = surface.wlid} Seq.<|)
  runEvent _surface (Event_wl_surface_enter _) = pass
  runEvent _surface (Event_wl_surface_leave _) = pass
  runEvent _surface (Event_wl_surface_preferred_buffer_scale _) = pass
  runEvent _surface (Event_wl_surface_preferred_buffer_transform _) = pass

-- }}}

-- Wl_seat {{{
instance Interface' Wl_seat Client where
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
  runRequest _ (Request_wl_region_destroy{}) = pass
  runRequest _ (Request_wl_region_add{}) = pass
  runRequest _ (Request_wl_region_subtract{}) = pass
  runEvent _ _ = pass

instance Interface' Wl_region Server where
  runRequest _ (Request_wl_region_destroy{}) = pass
  runRequest _ (Request_wl_region_add{}) = pass
  runRequest _ (Request_wl_region_subtract{}) = pass
  runEvent _ _ = pass

-- }}}

-- Wl_subcompositor {{{
instance Interface' Wl_subcompositor Client where
  runRequest subcompositor request@Request_wl_subcompositor_destroy = do
    sendMessage' request subcompositor.wlid
    dropObject subcompositor.wlid
  runRequest subcompositor request@(Request_wl_subcompositor_get_subsurface subsurface surface parent) = do
    surfaceObj' <- getInterface surface
    case surfaceObj' of
      Nothing -> error "wl_subcompositor: bad_surface"
      Just surfaceObj -> do
        SurfaceRole role' <- readIORef surfaceObj.role
        case cast role' of
          Just () -> do
            obj <- newInterface subsurface :: Wayland Client Wl_subsurface
            void $ newObject subsurface obj{surface = surface, parent = parent}
            writeIORef surfaceObj.role $ SurfaceRole subsurface
            sendMessage' request subcompositor.wlid
          _ -> error "surface already has a role assigned"
  runEvent _ _ = pass

instance Interface' Wl_subcompositor Server where
  runEvent _ _ = pass
  runRequest subcompositor Request_wl_subcompositor_destroy = dropObject subcompositor.wlid
  runRequest subcompositor (Request_wl_subcompositor_get_subsurface subsurface surface parent) = do
    surfaceObj' <- getInterface surface
    case surfaceObj' of
      Nothing -> sendError subcompositor.wlid 0 "bad_surface"
      Just surfaceObj -> do
        SurfaceRole role' <- readIORef surfaceObj.role
        case cast role' of
          Just () -> do
            obj <- newInterface subsurface :: Wayland Server Wl_subsurface
            void $ newObject subsurface obj{surface = surface, parent = parent}
            writeIORef surfaceObj.role $ SurfaceRole subsurface
          _ -> sendError subcompositor.wlid 0 "bad_surface"

-- }}}

-- Wl_subsurface {{{

instance Interface' Wl_subsurface Client where
  runRequest subsurface request@Request_wl_subsurface_destroy = do
    surfaceObj' <- getInterface subsurface.surface
    case surfaceObj' of
      Just surfaceObj -> do
        writeIORef surfaceObj.role $ SurfaceRole ()
      Nothing -> pass
    parentObj' <- getInterface subsurface.parent
    case parentObj' of
      Just parentObj -> do
        writeIORef parentObj.role $ SurfaceRole ()
        modifyIORef parentObj.state $ \state' ->
          state'
            { sSubsurfaces =
                state'.sSubsurfaces
                  { above = Seq.filter (/= subsurface.surface) state'.sSubsurfaces.above
                  , below = Seq.filter (/= subsurface.surface) state'.sSubsurfaces.below
                  }
            }
      Nothing -> pass
    sendMessage' request subsurface.wlid
    dropObject subsurface.wlid
  runRequest subsurface request@(Request_wl_subsurface_set_position (WlInt x) (WlInt y)) = do
    atomicWriteIORef subsurface.position (x, y)
    sendMessage' request subsurface.wlid
  runRequest subsurface request@(Request_wl_subsurface_place_below sibling) = do
    parentSurface' <- getInterface subsurface.parent
    case parentSurface' of
      Nothing -> error "invalid parent surface"
      Just parentSurface -> do
        state' <- readIORef parentSurface.state
        b <- atomicModifyIORef parentSurface.pendingState $ \s ->
          let stack = fromMaybe state'.sSubsurfaces s.cuSubsurfaces
              joint = stack.below <> stack.above
              (before, after) = Seq.breakl (== sibling) joint
              joint2 = (before Seq.|> subsurface.surface) <> after
              (below, above) = Seq.breakl (== subsurface.parent) joint2
           in bool (s{cuSubsurfaces = Just SubsurfaceStack{below, above}}, True) (s, False) (after == Seq.empty)
        unless b $ error "bad_surface: wl_surface is not a sibling or the parent"
    sendMessage' request subsurface.wlid
  runRequest subsurface request@(Request_wl_subsurface_place_above sibling) = do
    parentSurface' <- getInterface subsurface.parent
    case parentSurface' of
      Nothing -> error "invalid parent surface"
      Just parentSurface -> do
        state' <- readIORef parentSurface.state
        b <- atomicModifyIORef parentSurface.pendingState $ \s ->
          let stack = fromMaybe state'.sSubsurfaces s.cuSubsurfaces
              joint = stack.below <> stack.above
              (before, after) = Seq.breakl (== sibling) joint
              joint2 = (before Seq.|> fromJust (after Seq.!? 0) Seq.|> subsurface.surface) <> Seq.drop 1 after
              (below, above) = Seq.breakl (== subsurface.parent) joint2
           in bool (s{cuSubsurfaces = Just SubsurfaceStack{below, above}}, True) (s, False) (after == Seq.empty)
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
  runRequest subsurface Request_wl_subsurface_destroy = do
    surfaceObj' <- getInterface subsurface.surface
    case surfaceObj' of
      Just surfaceObj -> do
        writeIORef surfaceObj.role $ SurfaceRole ()
      Nothing -> pass
    parentObj' <- getInterface subsurface.parent
    case parentObj' of
      Just parentObj -> do
        writeIORef parentObj.role $ SurfaceRole ()
        modifyIORef parentObj.state $ \state' ->
          state'
            { sSubsurfaces =
                state'.sSubsurfaces
                  { above = Seq.filter (/= subsurface.surface) state'.sSubsurfaces.above
                  , below = Seq.filter (/= subsurface.surface) state'.sSubsurfaces.below
                  }
            }
      Nothing -> pass
    dropObject subsurface.wlid
  runRequest subsurface (Request_wl_subsurface_set_position (WlInt x) (WlInt y)) = atomicWriteIORef subsurface.position (x, y)
  runRequest subsurface (Request_wl_subsurface_place_below sibling) = do
    parentSurface' <- getInterface subsurface.parent
    case parentSurface' of
      Nothing -> sendError subsurface.wlid 0 "invalid parent surface"
      Just parentSurface -> do
        state' <- readIORef parentSurface.state
        b <- atomicModifyIORef parentSurface.pendingState $ \s ->
          let stack = fromMaybe state'.sSubsurfaces s.cuSubsurfaces
              joint = stack.below <> stack.above
              (before, after) = Seq.breakl (== sibling) joint
              joint2 = (before Seq.|> subsurface.surface) <> after
              (below, above) = Seq.breakl (== subsurface.parent) joint2
           in bool (s{cuSubsurfaces = Just SubsurfaceStack{below, above}}, True) (s, False) (after == Seq.empty)
        unless b $ sendError subsurface.wlid 0 "wl_surface is not a sibling or the parent"
  runRequest subsurface (Request_wl_subsurface_place_above sibling) = do
    parentSurface' <- getInterface subsurface.parent
    case parentSurface' of
      Nothing -> sendError subsurface.wlid 0 "invalid parent surface"
      Just parentSurface -> do
        state' <- readIORef parentSurface.state
        b <- atomicModifyIORef parentSurface.pendingState $ \s ->
          let stack = fromMaybe state'.sSubsurfaces s.cuSubsurfaces
              joint = stack.below <> stack.above
              (before, after) = Seq.breakl (== sibling) joint
              joint2 = (before Seq.|> fromJust (after Seq.!? 0) Seq.|> subsurface.surface) <> Seq.drop 1 after
              (below, above) = Seq.breakl (== subsurface.parent) joint2
           in bool (s{cuSubsurfaces = Just SubsurfaceStack{below, above}}, True) (s, False) (after == Seq.empty)
        unless b $ sendError subsurface.wlid 0 "wl_surface is not a sibling or the parent"
  runRequest subsurface (Request_wl_subsurface_set_sync{}) = atomicWriteIORef subsurface.synchronized True
  runRequest subsurface (Request_wl_subsurface_set_desync{}) = atomicWriteIORef subsurface.synchronized False
  runEvent _ _ = pass

-- }}}

-- Wl_fixes {{{
instance Interface' Wl_fixes Client where
  runRequest _ (Request_wl_fixes_destroy{}) = pass
  runRequest _ (Request_wl_fixes_destroy_registry{}) = pass

instance Interface' Wl_fixes Server

-- }}}

-- Wrapper Functions, for QoL {{{

-- Bind to global
bindToInterface :: Wl_registry -> WlString -> Wayland Client (Maybe ObjectID)
bindToInterface registry name = go 1
  where
    go :: Int -> Wayland Client (Maybe ObjectID)
    go count = do
      when
        (count >= 10)
        (putTextLn ("ERROR: the wayland global " <> show name <> " not found") >> exitFailure) -- maybe return Nothing here?
      putTextLn $ mconcat ["Trying to bind to ", show name, "... (", show count, ")"]
      ClientEnv env <- ask
      glob :: Maybe GlobalName <- BM.lookup name <$> readIORef env.globals
      case glob of
        Just x -> do
          new_id <- newObjectId
          ver <- fromJust . Map.lookup name <$> readIORef env.versionTable
          let wlNewId = WlNewId name ver new_id
          runRequest registry (Request_wl_registry_bind (coerce x) wlNewId)
          pure $ Just new_id
        Nothing -> liftIO (threadDelay $ 100 * 1000) >> go (count + 1)

-- }}}
-- vim: foldmethod=marker
