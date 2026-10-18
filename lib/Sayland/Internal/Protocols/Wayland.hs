{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_HADDOCK not-home #-}

-- | Description: Internals of Sayland.Protocols.Wayland
module Sayland.Internal.Protocols.Wayland (module Sayland.Internal.Protocols.Wayland) where

import Control.Applicative ((<|>))
import Control.Monad
import Control.Monad.IO.Class
import Data.Bifunctor (bimap)
import Data.Bool
import Data.Foldable
import Data.Function
import Data.Functor
import Data.Int
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Maybe
import Data.Sequence qualified as Seq
import Data.Text qualified as T
import Data.Typeable
import Foreign (Ptr, nullPtr)
import GHC.IORef (atomicSwapIORef)
import GHC.TypeError
import MMAP (mapShared, mkMmapFlags, mmap, munmap, protRead, protWrite)
import Sayland.Internal.Codegen
import Sayland.Internal.Core
import Sayland.Internal.Object
import Sayland.Internal.Prelude
import Sayland.Wire
import System.Posix (setFdSize)

$(loadProtocolFileEnums "xml-protocols/wayland.xml")

-- | Constant representing the t'Wl_display' ID which is always 1 in Wayland.
wlDisplayId :: TObjectID Wl_display
wlDisplayId = TObjectID . FromClient $ mkObjectID @Client @1

-- | Get the t'Wl_display' object which always exists during a connection.
getWlDisplay :: Wayland p Wl_display
getWlDisplay = pure $ Wl_display wlDisplayId

-- | Pattern aliases for convenience. These are deemed "global" errors so they are used often.
pattern Err_invalid_method, Err_implementation, Err_invalid_object, Err_no_memory :: Enum_wl_display_error
pattern Err_invalid_method = Enum_wl_display_error_invalid_method
pattern Err_implementation = Enum_wl_display_error_implementation
pattern Err_invalid_object = Enum_wl_display_error_invalid_object
pattern Err_no_memory = Enum_wl_display_error_no_memory

-- | A rectangle, described in pixels.
data Rectangle = Rectangle
  { position :: (Int32, Int32)
  , size :: (Int32, Int32)
  }
  deriving stock (Eq, Ord)

-- | Nothing or empty list means no change. In order to "reset" values, set them to the defaults - `Just Nothing` for the null object, normal transform, etc.
-- The offset is relative to the previous buffer, so `Nothing` is the same as @(0, 0)@.
data ContentUpdate = ContentUpdate
  { cuSurface :: Maybe (TObjectID Wl_surface)
  , cuBuffer :: Maybe (Maybe (TObjectID Wl_buffer))
  , cuOffset :: Maybe (Int32, Int32)
  , cuDamage :: [Rectangle]
  , cuDamageBuffer :: [Rectangle]
  , cuFrameCallbacks :: [TObjectID Wl_callback]
  , cuOpaqueRegion :: Maybe (Maybe (TObjectID Wl_region))
  , cuInputRegion :: Maybe (Maybe (TObjectID Wl_region))
  , cuBufferScale :: Maybe Int32
  , cuBufferTransform :: Maybe Enum_wl_output_transform
  , cuBufferRelease :: Maybe (TObjectID Wl_callback)
  , cuSubsurfaces :: Maybe SubsurfaceStack
  , cuFifoBarrier :: Bool
  , cuFifoWaitBarrier :: Bool
  , cuSlaveCUs :: [ContentUpdate]
  }
  deriving stock (Eq, Ord)

instance Semigroup ContentUpdate where
  (<>) = mergeContentUpdate

instance Monoid ContentUpdate where
  mempty =
    ContentUpdate
      { cuSurface = Nothing
      , cuBuffer = Nothing
      , cuOffset = Nothing
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

-- | Combine two content updates of one surface, the second committed after the first, like the commits of a
-- synchronized subsurface waiting for its parent's. Offsets add up, damage and frame callbacks accumulate,
-- and any other state the second sets replaces the first's. The Semigroup instance uses this for `<>`.
mergeContentUpdate :: ContentUpdate -> ContentUpdate -> ContentUpdate
mergeContentUpdate old new =
  ContentUpdate
    { cuSurface = new.cuSurface <|> old.cuSurface
    , cuBuffer = new.cuBuffer <|> old.cuBuffer
    , cuOffset = case (old.cuOffset, new.cuOffset) of
        (Just (x, y), Just (x', y')) -> Just (x + x', y + y')
        (a, b) -> b <|> a
    , cuDamage = new.cuDamage <> old.cuDamage
    , cuDamageBuffer = new.cuDamageBuffer <> old.cuDamageBuffer
    , cuFrameCallbacks = new.cuFrameCallbacks <> old.cuFrameCallbacks
    , cuOpaqueRegion = new.cuOpaqueRegion <|> old.cuOpaqueRegion
    , cuInputRegion = new.cuInputRegion <|> old.cuInputRegion
    , cuBufferScale = new.cuBufferScale <|> old.cuBufferScale
    , cuBufferTransform = new.cuBufferTransform <|> old.cuBufferTransform
    , cuBufferRelease = new.cuBufferRelease <|> old.cuBufferRelease
    , cuSubsurfaces = new.cuSubsurfaces <|> old.cuSubsurfaces
    , cuFifoBarrier = old.cuFifoBarrier || new.cuFifoBarrier
    , cuFifoWaitBarrier = old.cuFifoWaitBarrier || new.cuFifoWaitBarrier
    , cuSlaveCUs = foldl' cacheContentUpdate old.cuSlaveCUs new.cuSlaveCUs
    }

-- | Add a content update to a list of cached ones, merging it into the one of the same surface if there is one.
-- So the list holds at most one update per surface, however many times the surfaces commit.
cacheContentUpdate :: [ContentUpdate] -> ContentUpdate -> [ContentUpdate]
cacheContentUpdate cached cu = case break (\c -> c.cuSurface == cu.cuSurface) cached of
  (before, c : after) -> before <> (mergeContentUpdate c cu : after)
  (_, []) -> cached <> [cu]

-- | The role of a t'Wl_surface', as the value that gave it the role, like its @xdg_toplevel@.
-- @SurfaceRole ()@ means the surface has no role.
data SurfaceRole where SurfaceRole :: (Typeable a) => a -> SurfaceRole

-- Interfaces {{{
newtype Wl_display = Wl_display {wlid :: TObjectID Wl_display}

newtype Wl_registry = Wl_registry {wlid :: TObjectID Wl_registry}

data Wl_callback = Wl_callback {wlid :: TObjectID Wl_callback, done :: MVar ()}

newtype Wl_compositor = Wl_compositor {wlid :: TObjectID Wl_compositor}

data Wl_shm_pool = Wl_shm_pool {wlid :: TObjectID Wl_shm_pool, fd :: Fd, size :: IORef Int32, ptr :: IORef (Ptr ()), references :: IORef Int}

data Wl_shm = Wl_shm {wlid :: TObjectID Wl_shm, formats :: IORef [Enum_wl_shm_format]}

-- | Drop one reference to a shm pool, unmapping it once nothing (the pool object or its buffers) holds it.
unmapShmPoolRef :: (MonadIO m) => Wl_shm_pool -> m ()
unmapShmPoolRef pool = do
  refs <- atomicModifyIORef' pool.references $ \x -> (x - 1, x - 1)
  when (refs == 0) $ do
    ptr <- readIORef pool.ptr
    size <- readIORef pool.size
    liftIO $ munmap ptr (fromIntegral size)

-- | What holds the contents of a t'Wl_buffer', like shared memory or a dmabuf.
class BufferBackend a where
  -- | Run on the compositor when it sends @wl_buffer.release@.
  releaseBuffer :: a -> Wayland p ()

-- | A buffer backend of any type.
data Buffer where Buffer :: (Typeable a, BufferBackend a) => a -> Buffer

-- | A buffer in a t'Wl_shm_pool': where it starts in the pool, its stride and its pixel format.
data ShmBuffer = ShmBuffer {offset :: Int32, stride :: Int32, pool :: Wl_shm_pool, format :: Enum_wl_shm_format}

instance BufferBackend ShmBuffer where
  releaseBuffer ShmBuffer{pool} = unmapShmPoolRef pool

instance BufferBackend () where
  releaseBuffer _ = pass

data Wl_buffer = Wl_buffer
  { wlid :: TObjectID Wl_buffer
  , buffer :: Buffer
  , width :: Int32
  , height :: Int32
  }

-- | The surface role of a drag-and-drop icon, with the drag it belongs to.
newtype DndIcon = DndIcon Dnd

-- | A MIME type that data is offered in.
type Mimetype = WlText

data Wl_data_offer = Wl_data_offer
  { wlid :: TObjectID Wl_data_offer
  , acceptedMimetypes :: IORef [WlString]
  , offeredMimetypes :: IORef [Mimetype]
  , offer_data_device :: TObjectID Wl_data_device
  , offerSourceActions :: IORef Enum_wl_data_device_manager_dnd_action
  , offerActions :: IORef Enum_wl_data_device_manager_dnd_action
  , offerPreferredAction :: IORef Enum_wl_data_device_manager_dnd_action
  , selectedAction :: IORef Enum_wl_data_device_manager_dnd_action
  }

data Wl_data_source = Wl_data_source
  { wlid :: TObjectID Wl_data_source
  , sourceOfferedMimetypes :: IORef [Mimetype]
  , sourceActions :: IORef Enum_wl_data_device_manager_dnd_action
  , sourceSelectedAction :: IORef Enum_wl_data_device_manager_dnd_action
  , sourceTargetMimetype :: IORef (Maybe Mimetype)
  }

-- | Builder for an empty t'Enum_wl_data_device_manager_dnd_action'.
noDndAction :: Enum_wl_data_device_manager_dnd_action
noDndAction =
  Enum_wl_data_device_manager_dnd_action
    { dnd_action_none = True
    , dnd_action_ask = False
    , dnd_action_copy = False
    , dnd_action_move = False
    }

-- | A drag-and-drop session, started with @wl_data_device.start_drag@.
data Dnd = Dnd
  { source :: Maybe (TObjectID Wl_data_source)
  -- ^ If source is Nothing, Dnd is performed only within the same client.
  , origin :: TObjectID Wl_surface
  , icon :: Maybe (TObjectID Wl_surface)
  , grabSerial :: Word32
  }

-- | A drag-and-drop session as the client it enters sees it, from @wl_data_device.enter@ until leave or drop.
data DndClient = DndClient
  { target :: TObjectID Wl_surface
  , dndPosition :: (WlFixed, WlFixed)
  , offer :: Maybe (TObjectID Wl_data_offer)
  , enterSerial :: Word32
  }

-- | The selection set with @wl_data_device.set_selection@.
data Selection = Selection
  { source :: Maybe (TObjectID Wl_data_source)
  , eventSerial :: Word32
  }

-- | The selection as a client sees it, from @wl_data_device.selection@.
newtype SelectionClient = SelectionClient
  { offer :: Maybe (TObjectID Wl_data_offer)
  }

data Wl_data_device = Wl_data_device
  { wlid :: TObjectID Wl_data_device
  , sources :: IORef [TObjectID Wl_data_source]
  , offers :: IORef [TObjectID Wl_data_offer]
  , dnd :: MVar Dnd
  -- ^ a Drag and Drop session
  , dndClient :: MVar DndClient
  -- ^ a Drag and Drop client session
  , selection :: MVar Selection
  -- ^ selection session
  , selectionClient :: MVar SelectionClient
  , seat :: TObjectID Wl_seat
  }

newtype Wl_data_device_manager = Wl_data_device_manager {wlid :: TObjectID Wl_data_device_manager}

newtype Wl_shell = Wl_shell {wlid :: TObjectID Wl_shell}

newtype Wl_shell_surface = Wl_shell_surface {wlid :: TObjectID Wl_shell_surface}

data Wl_region = Wl_region {wlid :: TObjectID Wl_region, included :: IORef [Rectangle], excluded :: IORef [Rectangle]}

-- | The stacking order of a surface's subsurfaces: those below the surface and those above it.
data SubsurfaceStack = SubsurfaceStack
  { above :: Seq.Seq (TObjectID Wl_surface)
  , below :: Seq.Seq (TObjectID Wl_surface)
  }
  deriving stock (Eq, Ord)

-- | The committed state of a t'Wl_surface'. Updated by every commit that is applied, which a compositor can
-- react to from an t'EventHandler' for @wl_surface.commit@, as those run after the state is updated.
data SurfaceState = SurfaceState
  { sBuffer :: Maybe (TObjectID Wl_buffer)
  , sBufferOffset :: (Int, Int)
  -- ^ Where the last applied commit put the buffer, relative to the previous buffer.
  , sDamage :: [Rectangle]
  -- ^ The damage of the last applied commit, in surface coordinates.
  , sDamageBuffer :: [Rectangle]
  -- ^ The damage of the last applied commit, in buffer coordinates.
  , sCallbacks :: [TObjectID Wl_callback]
  -- ^ Frame callbacks that were committed and are not done yet.
  , sOpaqueRegion :: Maybe (TObjectID Wl_region)
  , sInputRegion :: Maybe (TObjectID Wl_region)
  , sBufferScale :: Int
  , sBufferTransform :: Enum_wl_output_transform
  , sSubsurfaces :: SubsurfaceStack
  , sBufferRelease :: Maybe (TObjectID Wl_callback)
  -- ^ The release callback of `sBuffer`, from @wl_surface.get_release@.
  , sFifoBarrier :: Bool
  }

data Wl_surface = Wl_surface
  { wlid :: TObjectID Wl_surface
  , pendingState :: IORef ContentUpdate
  , role :: IORef SurfaceRole
  , state :: TVar SurfaceState
  , outputs :: IORef [TObjectID Wl_output]
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

-- | A @wl_output.geometry@ event: the output's position, physical size, subpixel layout, make, model and transform.
data OutputGeometry = OutputGeometry {outputPosition :: (Int32, Int32), outputSize :: (Int32, Int32), subpixel :: Enum_wl_output_subpixel, make :: WlText, model :: WlText, outputTransform :: Enum_wl_output_transform}

-- | A @wl_output.mode@ event: the mode's flags, its size in pixels and its refresh rate in mHz.
data OutputMode = OutputMode {outputFlags :: Enum_wl_output_mode, modeWidth :: Int32, modeHeight :: Int32, modeRefresh :: Int32}

-- | What changed about an output since the last @wl_output.done@, which applies it. 'Nothing' for what did not change.
data OutputUpdate = OutputUpdate
  { updateGeometry :: Maybe OutputGeometry
  , updateMode :: Maybe OutputMode
  , updateScale :: Maybe Int32
  , updateName :: Maybe WlText
  , updateDescription :: Maybe WlText
  }

data Wl_output = Wl_output
  { wlid :: TObjectID Wl_output
  , outputGeometry :: IORef OutputGeometry
  , outputMode :: IORef OutputMode
  , outputScale :: IORef Int32
  , outputName :: IORef WlText
  , outputDescription :: IORef WlText
  , outputUpdate :: IORef OutputUpdate
  }

newtype Wl_subcompositor = Wl_subcompositor {wlid :: TObjectID Wl_subcompositor}

newtype Wl_fixes = Wl_fixes {wlid :: TObjectID Wl_fixes}

--- }}}

$(loadProtocolFile wlFormatter "xml-protocols/wayland.xml")

-- | Enter an object the message creates in this side's object map. An ID that is still in use is a protocol violation,
-- and so is an ID from the peer that skips ahead of the next one it may use, as libwayland enforces.
registerObject :: (Message m, Object i) => m -> i -> Wayland p ()
registerObject msg obj = registerSomeObject msg (toObjectID obj.wlid) (SomeObject obj)

-- | `registerObject` for an object of any interface.
registerSomeObject :: (Message m) => m -> AnyObjectID -> SomeObject -> Wayland p ()
registerSomeObject msg oid obj = do
  env <- getConnectionEnv
  checkPeerIDOrder msg oid
  inserted <- atomically $ do
    m <- readTVar env.objects
    if Map.member oid m then pure False else True <$ writeTVar env.objects (Map.insert oid obj m)
  unless inserted
    . protocolViolation Unrecoverable msg wlDisplayId Err_invalid_object
    . wlTextLossy
    $ "object ID " <> T.show (fromAnyObjectID oid) <> " is still in use"

-- | The peer may give a new object any ID up to one past the highest it has used, never further.
-- libwayland ends the connection of a peer whose new ID skips ahead so we mark the violation `Unrecoverable`
-- though technically we could recover from it.
--
-- IDs this side allocated are not checked as they are in the other range.`withNewObjectID` keeps them in order.
checkPeerIDOrder :: (Message m) => m -> AnyObjectID -> Wayland p ()
checkPeerIDOrder msg oid = do
  env <- getConnectionEnv
  me <- getPerspective
  let n = fromAnyObjectID oid
  unless (allocatedBy oid == me) $ do
    skipsAhead <- atomicModifyIORef' env.nextPeerID $ \next ->
      if n > next
        then (next, True)
        else (max next (n + 1), False) -- n + 1 might be lower than next with id reuse.
    when skipsAhead
      . protocolViolation Unrecoverable msg wlDisplayId Err_invalid_object
      . wlTextLossy
      $ "new object ID " <> T.show n <> " skips ahead of the next one the peer may use"

-- | Remove an object from the objects map. On a server this also sends @delete_id@.
dropObject :: TObjectID a -> Wayland p ()
dropObject (TObjectID i) = do
  env <- getConnectionEnv
  atomically . modifyTVar' env.objects $ Map.delete i
  onServer $ sendMessage (Event_wl_display_delete_id $ fromAnyObjectID i) wlDisplayId

-- | @mmap@ a shm pool's fd, read/write and shared.
mapShmPool :: (MonadIO m) => Fd -> Int32 -> m (Ptr ())
mapShmPool fd size =
  liftIO $ mmap nullPtr (fromIntegral size) (protRead <> protWrite) (mkMmapFlags mapShared mempty) fd 0

-- | The parent of a surface with a synchronized subsurface role, whose commits wait for the parent's.
synchronizedParent :: Wl_surface -> Wayland p (Maybe Wl_surface)
synchronizedParent surface = do
  SurfaceRole role <- readIORef surface.role
  case cast role of
    Nothing -> pure Nothing
    Just (subsurfaceId :: TObjectID Wl_subsurface) ->
      getInterface subsurfaceId >>= \case
        Nothing -> pure Nothing
        Just subsurface -> readIORef subsurface.synchronized >>= bool (pure Nothing) (getInterface subsurface.parent)

-- | Apply a committed content update to the current state of its surface, then the cached updates of the
-- synchronized subsurfaces that waited for it. All in one transaction, so the surface and its subsurfaces change together.
applyContentUpdate :: ConnectionEnv p -> Wl_surface -> ContentUpdate -> STM ()
applyContentUpdate env surface cu = do
  objects <- readTVar env.objects
  -- A frame callback is dropped once it is done, so the ones still in the object map are not done yet.
  let notDone = filter (\cb -> Map.member (toObjectID cb) objects)
  modifyTVar' surface.state $ \s ->
    s
      { sBuffer = fromMaybe s.sBuffer cu.cuBuffer
      , sBufferOffset = maybe (0, 0) (bimap fromIntegral fromIntegral) cu.cuOffset
      , sDamage = cu.cuDamage
      , sDamageBuffer = cu.cuDamageBuffer
      , sCallbacks = cu.cuFrameCallbacks <> notDone s.sCallbacks
      , sOpaqueRegion = fromMaybe s.sOpaqueRegion cu.cuOpaqueRegion
      , sInputRegion = fromMaybe s.sInputRegion cu.cuInputRegion
      , sBufferScale = maybe s.sBufferScale fromIntegral cu.cuBufferScale
      , sBufferTransform = fromMaybe s.sBufferTransform cu.cuBufferTransform
      , sSubsurfaces = fromMaybe s.sSubsurfaces cu.cuSubsurfaces
      , sBufferRelease = if isJust cu.cuBuffer then cu.cuBufferRelease else cu.cuBufferRelease <|> s.sBufferRelease
      , sFifoBarrier = s.sFifoBarrier || cu.cuFifoBarrier
      }
  for_ cu.cuSlaveCUs $ \slave ->
    for_ slave.cuSurface $ getInterfaceSTM env >=> traverse_ (\subsurface -> applyContentUpdate env subsurface slave)

-- Interface Implementations {{{
-- Wl_display {{{
instance Object Wl_display where
  onEvent obj msg@(Event_wl_display_delete_id dId) = do
    env <- getConnectionEnv
    case mkWlObjectID dId of
      Just oid -> atomically . modifyTVar' env.objects $ Map.delete oid
      Nothing -> protocolViolation Recoverable msg obj Err_invalid_object [wl|delete_id for the null object|]
    forwardMessage obj msg
  onEvent obj msg@(Event_wl_display_error objectId code message) = do
    onClient . throwIO $ ProtocolError (Just objectId) code (Just message)
    forwardMessage obj msg
  onRequest obj msg@(Request_wl_display_sync callbackId) = do
    callbackObj <- Wl_callback callbackId <$> newEmptyMVar
    registerObject msg callbackObj
    onServer $ sendMsg callbackObj (Event_wl_callback_done 0)
    forwardMessage obj msg
  onRequest obj msg@(Request_wl_display_get_registry registryId) = do
    let registryObj = Wl_registry registryId
    registerObject msg registryObj
    onServer $ do
      env <- getConnectionEnv
      let entries = zip [0 ..] $ Map.toList env.interfaceTable
      for_ entries $ \(name, (interface, entry)) ->
        sendMsg registryObj (Event_wl_registry_global name interface entry.version)
    forwardMessage obj msg

-- }}}

-- Wl_callback {{{
instance Object Wl_callback where
  onEvent obj msg@(Event_wl_callback_done _) = do
    putMVar obj.done ()
    forwardMessage obj msg
    dropObject obj.wlid
  onRequest _ = \case {}

-- }}}

-- Wl_registry {{{
instance Object Wl_registry where
  onEvent obj msg@(Event_wl_registry_global name interface version) = do
    env <- getConnectionEnv
    atomicModifyIORef' env.globals $ \m -> (Map.insert name (interface, version) m, ())
    forwardMessage obj msg
  onEvent obj msg@(Event_wl_registry_global_remove name) = do
    env <- getConnectionEnv
    atomicModifyIORef' env.globals $ \m -> (Map.delete name m, ())
    forwardMessage obj msg

  onRequest obj msg@(Request_wl_registry_bind name (WlNewId requestedIface requestedVersion newId)) = do
    env <- getConnectionEnv
    globals <- readIORef env.globals

    (iface, advertisedVersion) <- Map.lookup name globals & whenNothing $ do
      protocolViolation Unrecoverable msg wlDisplayId Err_invalid_method [wl|wl_registry: bind to a global that was not advertised|]

    unless (iface == requestedIface) $
      protocolViolation Unrecoverable msg wlDisplayId Err_invalid_method [wl|wl_registry: bind with the wrong interface for this global|]

    unless (requestedVersion >= 1) $
      protocolViolation Unrecoverable msg wlDisplayId Err_invalid_method [wl|wl_registry: bind version must be at least 1|]

    unless (requestedVersion <= advertisedVersion) $
      protocolViolation Unrecoverable msg wlDisplayId Err_invalid_method [wl|wl_registry: bind version is higher than what is advertised|]

    entry <- Map.lookup iface env.interfaceTable & whenNothing $ do
      protocolViolation Unrecoverable msg wlDisplayId Err_invalid_method [wl|wl_registry: bind to an unsupported interface|]

    registerSomeObject msg (FromClient newId) =<< liftIO (entry.construct $ FromClient newId)
    forwardMessage obj msg

-- }}}

-- Wl_compositor {{{
instance Object Wl_compositor where
  onRequest obj msg@(Request_wl_compositor_create_surface wlid) = do
    wl_surface <- do
      role <- newIORef $ SurfaceRole ()
      pendingState <- newIORef mempty
      outputs <- newIORef []
      state <-
        newTVarIO $
          SurfaceState
            { sBuffer = Nothing
            , sBufferOffset = (0, 0)
            , sDamage = []
            , sDamageBuffer = []
            , sCallbacks = []
            , sOpaqueRegion = Nothing
            , sInputRegion = Nothing
            , sBufferScale = 1
            , sBufferTransform = Enum_wl_output_transform_normal
            , sSubsurfaces = SubsurfaceStack{above = Seq.empty, below = Seq.empty}
            , sBufferRelease = Nothing
            , sFifoBarrier = False
            }
      pure Wl_surface{..}
    registerObject msg wl_surface
    forwardMessage obj msg
  onRequest obj msg@(Request_wl_compositor_create_region wlid) = do
    wl_regionObj <- do
      included <- newIORef []
      excluded <- newIORef []
      pure Wl_region{..}
    registerObject msg wl_regionObj
    forwardMessage obj msg
  onRequest obj msg@Request_wl_compositor_release = do
    forwardMessage obj msg
    dropObject obj.wlid

  onEvent _ = \case {}

-- }}}

-- Wl_shm_pool {{{
instance Object Wl_shm_pool where
  onRequest pool msg@(Request_wl_shm_pool_create_buffer bufId offset width height stride format) = do
    atomicModifyIORef' pool.references $ \x -> (x + 1, ())
    registerObject
      msg
      Wl_buffer
        { wlid = bufId
        , width
        , height
        , buffer = Buffer ShmBuffer{pool, offset, stride, format}
        }
    forwardMessage pool msg
  onRequest pool msg@Request_wl_shm_pool_destroy = do
    forwardMessage pool msg
    unmapShmPoolRef pool
    dropObject pool.wlid
  onRequest pool msg@(Request_wl_shm_pool_resize size) = do
    -- Only the client owns the backing file, so only it grows it.
    onClient . liftIO . setFdSize pool.fd $ fromIntegral size
    oldSize <- readIORef pool.size
    oldPtr <- readIORef pool.ptr
    liftIO $ munmap oldPtr (fromIntegral oldSize)
    newPtr <- mapShmPool pool.fd size
    atomicWriteIORef pool.ptr newPtr
    atomicWriteIORef pool.size size
    forwardMessage pool msg

  onEvent _ = \case {}

-- }}}

-- Wl_shm {{{
instance Global Wl_shm where
  global i = Wl_shm i <$> newIORef []

instance Object Wl_shm where
  onRequest shm msg@(Request_wl_shm_create_pool poolId fd size) = do
    sizeRef <- newIORef size
    ptrRef <- newIORef =<< mapShmPool fd size
    references <- newIORef 1
    registerObject msg Wl_shm_pool{wlid = poolId, fd, size = sizeRef, ptr = ptrRef, references}
    forwardMessage shm msg
  onRequest shm msg@Request_wl_shm_release = do
    forwardMessage shm msg
    dropObject shm.wlid

  onEvent shm msg@(Event_wl_shm_format format) = do
    atomicModifyIORef' shm.formats $ \fs -> (format : fs, ())
    forwardMessage shm msg

-- }}}

-- Wl_buffer {{{
instance Object Wl_buffer where
  onRequest buffer msg@Request_wl_buffer_destroy = do
    forwardMessage buffer msg
    dropObject buffer.wlid

  onEvent buffer@Wl_buffer{buffer = Buffer backend} msg@Event_wl_buffer_release = do
    forwardMessage buffer msg
    -- Only the compositor owns the backend resources.
    onServer $ releaseBuffer backend

-- }}}

-- Wl_data_offer {{{
instance Object Wl_data_offer where
  onRequest offer msg@(Request_wl_data_offer_accept _ mimetype) = do
    atomicModifyIORef' offer.acceptedMimetypes $ \ms -> (mimetype : ms, ())
    forwardMessage offer msg
  onRequest offer msg@Request_wl_data_offer_receive{} =
    forwardMessage offer msg
  onRequest offer msg@Request_wl_data_offer_destroy = do
    forwardMessage offer msg
    dropObject offer.wlid
  onRequest offer msg@Request_wl_data_offer_finish =
    forwardMessage offer msg
  onRequest offer msg@(Request_wl_data_offer_set_actions dnd_actions preferred_action) = do
    atomicWriteIORef offer.offerActions dnd_actions
    atomicWriteIORef offer.offerPreferredAction preferred_action
    forwardMessage offer msg

  onEvent offer msg@(Event_wl_data_offer_offer mimetype) = do
    atomicModifyIORef' offer.offeredMimetypes $ \ms -> (mimetype : ms, ())
    forwardMessage offer msg
  onEvent offer msg@(Event_wl_data_offer_source_actions source_actions) = do
    atomicWriteIORef offer.offerSourceActions source_actions
    forwardMessage offer msg
  onEvent offer msg@(Event_wl_data_offer_action dnd_action) = do
    atomicWriteIORef offer.selectedAction dnd_action
    forwardMessage offer msg

-- }}}

-- Wl_data_source {{{
instance Object Wl_data_source where
  onRequest source msg@(Request_wl_data_source_offer mimetype) = do
    atomicModifyIORef' source.sourceOfferedMimetypes $ \ms -> (mimetype : ms, ())
    forwardMessage source msg
  onRequest source msg@Request_wl_data_source_destroy = do
    forwardMessage source msg
    dropObject source.wlid
  onRequest source msg@(Request_wl_data_source_set_actions dnd_actions) = do
    atomicWriteIORef source.sourceActions dnd_actions
    forwardMessage source msg

  onEvent source msg@(Event_wl_data_source_target mimetype) = do
    atomicWriteIORef source.sourceTargetMimetype mimetype
    forwardMessage source msg
  onEvent source msg@Event_wl_data_source_send{} =
    -- Handling the data from this is for user made EventHandlers.
    forwardMessage source msg
  onEvent source msg@Event_wl_data_source_cancelled = do
    forwardMessage source msg
    onClient $ sendMsg source Request_wl_data_source_destroy
  onEvent source msg@Event_wl_data_source_dnd_drop_performed =
    forwardMessage source msg
  onEvent source msg@Event_wl_data_source_dnd_finished = do
    forwardMessage source msg
    onClient $ sendMsg source Request_wl_data_source_destroy
  onEvent source msg@(Event_wl_data_source_action action) = do
    atomicWriteIORef source.sourceSelectedAction action
    forwardMessage source msg

-- }}}

-- Wl_data_device {{{
instance Object Wl_data_device where
  onRequest device msg@(Request_wl_data_device_start_drag source origin icon grabSerial) = do
    let dnd = Dnd{source, origin, icon, grabSerial}
    onClient $ do
      isEmpty <- liftIO $ isEmptyMVar device.dnd
      unless isEmpty $ error "tried starting drag while during one"
      putMVar device.dnd dnd
    onServer $ do
      tryTakeMVar_ device.dnd
      maybe (pure Nothing) getInterface icon >>= \case
        Nothing -> putMVar device.dnd dnd
        Just x -> do
          SurfaceRole role <- readIORef x.role
          case cast role of
            Just () -> do
              atomicWriteIORef x.role $ SurfaceRole $ DndIcon dnd
              putMVar device.dnd dnd
            Nothing -> protocolViolation Unrecoverable msg device Enum_wl_data_device_error_role [wl|start_drag: icon surface already has a role|]
    forwardMessage device msg
  onRequest device msg@(Request_wl_data_device_set_selection source eventSerial) = do
    tryTakeMVar_ device.selection
    putMVar device.selection Selection{source, eventSerial}
    forwardMessage device msg
  onRequest device msg@Request_wl_data_device_release = do
    forwardMessage device msg
    dropObject device.wlid

  onEvent device msg@(Event_wl_data_device_data_offer offerId) = do
    acceptedMimetypes <- newIORef []
    offeredMimetypes <- newIORef []
    offerSourceActions <- newIORef noDndAction
    offerActions <- newIORef noDndAction
    selectedAction <- newIORef noDndAction
    offerPreferredAction <- newIORef noDndAction
    registerObject msg Wl_data_offer{wlid = offerId, offer_data_device = device.wlid, ..}
    forwardMessage device msg
  onEvent device msg@(Event_wl_data_device_enter enterSerial target x y offer) = do
    tryTakeMVar_ device.dndClient
    putMVar device.dndClient DndClient{target, dndPosition = (x, y), offer, enterSerial}
    forwardMessage device msg
  onEvent device msg@Event_wl_data_device_leave = do
    tryTakeMVar_ device.dndClient
    forwardMessage device msg
  onEvent device msg@(Event_wl_data_device_motion _ x y) = do
    liftIO $ modifyMVar_ device.dndClient $ \session -> pure session{dndPosition = (x, y)}
    forwardMessage device msg
  onEvent device msg@Event_wl_data_device_drop = do
    tryTakeMVar_ device.dndClient
    forwardMessage device msg
  onEvent device msg@(Event_wl_data_device_selection offer) = do
    tryTakeMVar_ device.selectionClient
    putMVar device.selectionClient SelectionClient{offer}
    forwardMessage device msg

-- }}}

-- Wl_data_device_manager {{{
instance Object Wl_data_device_manager where
  onRequest mgr msg@(Request_wl_data_device_manager_create_data_source sourceId) = do
    sourceOfferedMimetypes <- newIORef []
    sourceActions <- newIORef noDndAction
    sourceSelectedAction <- newIORef noDndAction
    sourceTargetMimetype <- newIORef Nothing
    registerObject msg Wl_data_source{wlid = sourceId, ..}
    forwardMessage mgr msg
  onRequest mgr msg@(Request_wl_data_device_manager_get_data_device deviceId seat) = do
    sources <- newIORef []
    offers <- newIORef []
    dnd <- newEmptyMVar
    dndClient <- newEmptyMVar
    selection <- newEmptyMVar
    selectionClient <- newEmptyMVar
    registerObject msg Wl_data_device{wlid = deviceId, ..}
    forwardMessage mgr msg
  onRequest mgr msg@Request_wl_data_device_manager_release = do
    forwardMessage mgr msg
    dropObject mgr.wlid

  onEvent _ = \case {}

-- }}}

-- Wl_shell {{{
instance (Unsatisfiable (Text "wl_shell is deprecated and not implemented. Use xdg_wm_base from xdg-shell instead.")) => Object Wl_shell

instance (Unsatisfiable (Text "wl_shell is deprecated and not implemented. Use xdg_wm_base from xdg-shell instead.")) => Global Wl_shell

-- }}}

-- Wl_shell_surface {{{
instance (Unsatisfiable (Text "wl_shell_surface is deprecated and not implemented. Use xdg_surface from xdg-shell instead.")) => Object Wl_shell_surface

-- }}}

-- Wl_surface {{{
instance Object Wl_surface where
  onRequest surface msg@Request_wl_surface_destroy{} = do
    forwardMessage surface msg
    dropObject surface.wlid
  onRequest surface msg@(Request_wl_surface_attach bufferId x y) = do
    atomicModifyIORef' surface.pendingState $ \s -> (s{cuBuffer = Just bufferId, cuOffset = Just (x, y)}, ())
    forwardMessage surface msg
  onRequest surface msg@(Request_wl_surface_damage x y w h) = do
    onClient $ logDebug "New clients should not use this request (wl_surface.damage). Instead damage can be posted with wl_surface.damage_buffer which uses buffer coordinates instead of surface coordinates."
    atomicModifyIORef' surface.pendingState $ \s -> (s{cuDamage = Rectangle{position = (x, y), size = (w, h)} : s.cuDamage}, ())
    forwardMessage surface msg
  onRequest surface msg@(Request_wl_surface_frame cb) = do
    done <- newEmptyMVar
    registerObject msg Wl_callback{wlid = cb, done}
    atomicModifyIORef' surface.pendingState $ \s -> (s{cuFrameCallbacks = cb : s.cuFrameCallbacks}, ())
    forwardMessage surface msg
  onRequest surface msg@(Request_wl_surface_set_opaque_region region) = do
    atomicModifyIORef' surface.pendingState $ \s -> (s{cuOpaqueRegion = Just region}, ())
    forwardMessage surface msg
  onRequest surface msg@(Request_wl_surface_set_input_region region) = do
    atomicModifyIORef' surface.pendingState $ \s -> (s{cuInputRegion = Just region}, ())
    forwardMessage surface msg
  onRequest surface msg@(Request_wl_surface_set_buffer_transform transform) = do
    atomicModifyIORef' surface.pendingState $ \s -> (s{cuBufferTransform = Just transform}, ())
    forwardMessage surface msg
  onRequest surface msg@(Request_wl_surface_set_buffer_scale scale) = do
    atomicModifyIORef' surface.pendingState $ \s -> (s{cuBufferScale = Just scale}, ())
    forwardMessage surface msg
  onRequest surface msg@(Request_wl_surface_damage_buffer x y w h) = do
    atomicModifyIORef' surface.pendingState $ \s -> (s{cuDamageBuffer = Rectangle{position = (x, y), size = (w, h)} : s.cuDamageBuffer}, ())
    forwardMessage surface msg
  onRequest surface msg@(Request_wl_surface_offset x y) = do
    atomicModifyIORef' surface.pendingState $ \s -> (s{cuOffset = Just (x, y)}, ())
    forwardMessage surface msg
  onRequest surface msg@(Request_wl_surface_get_release release) = do
    done <- newEmptyMVar
    registerObject msg Wl_callback{wlid = release, done}
    atomicModifyIORef' surface.pendingState $ \s -> (s{cuBufferRelease = Just release}, ())
    forwardMessage surface msg
  onRequest surface msg@Request_wl_surface_commit{} = do
    cu <- liftIO $ atomicSwapIORef surface.pendingState mempty
    synchronizedParent surface >>= \case
      -- Cached in the parent's pending state, to be applied with the parent's next commit.
      Just parent -> atomicModifyIORef' parent.pendingState $ \s -> (s{cuSlaveCUs = cacheContentUpdate s.cuSlaveCUs cu{cuSurface = Just surface.wlid}}, ())
      Nothing -> getConnectionEnv >>= \env -> atomically $ applyContentUpdate env surface cu
    forwardMessage surface msg

  onEvent surface msg@(Event_wl_surface_enter output) = do
    atomicModifyIORef' surface.outputs $ \os -> (output : os, ())
    forwardMessage surface msg
  onEvent surface msg@(Event_wl_surface_leave output) = do
    atomicModifyIORef' surface.outputs $ \os -> (filter (/= output) os, ())
    forwardMessage surface msg
  onEvent surface msg = do
    stub surface msg
    forwardMessage surface msg

-- }}}

-- Wl_seat {{{
instance Object Wl_seat where
  onRequest seat msg@(Request_wl_seat_get_pointer wlid) = do
    registerObject msg Wl_pointer{wlid}
    forwardMessage seat msg
  onRequest seat msg@(Request_wl_seat_get_keyboard wlid) = do
    registerObject msg Wl_keyboard{wlid}
    forwardMessage seat msg
  onRequest seat msg@(Request_wl_seat_get_touch wlid) = do
    registerObject msg Wl_touch{wlid}
    forwardMessage seat msg
  onRequest seat msg@Request_wl_seat_release{} = do
    forwardMessage seat msg
    dropObject seat.wlid

  onEvent seat msg = do
    stub seat msg
    forwardMessage seat msg

-- }}}

-- Wl_pointer {{{
instance Object Wl_pointer where
  onRequest obj msg@Request_wl_pointer_set_cursor{} = do
    stub obj msg
    forwardMessage obj msg
  onRequest obj msg@Request_wl_pointer_release{} = do
    forwardMessage obj msg
    dropObject obj.wlid

  onEvent obj msg = do
    case msg of
      Event_wl_pointer_enter{} -> pass
      Event_wl_pointer_leave{} -> pass
      Event_wl_pointer_motion{} -> pass
      Event_wl_pointer_button{} -> pass
      Event_wl_pointer_axis{} -> pass
      Event_wl_pointer_frame{} -> pass
      Event_wl_pointer_axis_source{} -> pass
      Event_wl_pointer_axis_stop{} -> pass
      Event_wl_pointer_axis_discrete{} -> pass
      Event_wl_pointer_axis_value120{} -> pass
      Event_wl_pointer_axis_relative_direction{} -> pass
    forwardMessage obj msg

-- }}}

-- Wl_keyboard {{{

instance Object Wl_keyboard where
  onRequest obj msg@Request_wl_keyboard_release{} = do
    forwardMessage obj msg
    dropObject obj.wlid

  onEvent obj msg = do
    case msg of
      Event_wl_keyboard_keymap{} -> pass
      Event_wl_keyboard_enter{} -> pass
      Event_wl_keyboard_leave{} -> pass
      Event_wl_keyboard_key{} -> pass
      Event_wl_keyboard_modifiers{} -> pass
      Event_wl_keyboard_repeat_info{} -> pass
    forwardMessage obj msg

-- }}}

-- Wl_touch {{{
instance Object Wl_touch where
  onRequest obj msg@Request_wl_touch_release{} = do
    forwardMessage obj msg
    dropObject obj.wlid

  onEvent obj msg = do
    case msg of
      Event_wl_touch_down{} -> pass
      Event_wl_touch_up{} -> pass
      Event_wl_touch_motion{} -> pass
      Event_wl_touch_frame{} -> pass
      Event_wl_touch_cancel{} -> pass
      Event_wl_touch_shape{} -> pass
      Event_wl_touch_orientation{} -> pass
    forwardMessage obj msg

-- }}}

-- Wl_output {{{
instance Global Wl_output where
  global wlid = do
    outputGeometry <- newIORef OutputGeometry{outputPosition = (0, 0), outputSize = (0, 0), subpixel = Enum_wl_output_subpixel_horizontal_rgb, make = mempty, model = mempty, outputTransform = Enum_wl_output_transform_normal}
    outputMode <- newIORef OutputMode{outputFlags = Enum_wl_output_mode{mode_current = False, mode_preferred = False}, modeWidth = 0, modeHeight = 0, modeRefresh = 0}
    outputScale <- newIORef 1
    outputName <- newIORef mempty
    outputDescription <- newIORef mempty
    outputUpdate <- newIORef $ OutputUpdate Nothing Nothing Nothing Nothing Nothing
    pure Wl_output{..}

instance Object Wl_output where
  onRequest obj msg@Request_wl_output_release{} = do
    forwardMessage obj msg
    dropObject obj.wlid

  onEvent obj msg@(Event_wl_output_geometry x y width height subpixel make model outputTransform) = do
    atomicModifyIORef' obj.outputUpdate $ \u -> (u{updateGeometry = Just OutputGeometry{outputPosition = (x, y), outputSize = (width, height), subpixel, make, model, outputTransform}}, ())
    forwardMessage obj msg
  onEvent obj msg@(Event_wl_output_mode outputFlags modeWidth modeHeight modeRefresh) = do
    atomicModifyIORef' obj.outputUpdate $ \u -> (u{updateMode = Just OutputMode{outputFlags, modeWidth, modeHeight, modeRefresh}}, ())
    forwardMessage obj msg
  onEvent obj msg@Event_wl_output_done = do
    update <- readIORef obj.outputUpdate
    for_ update.updateGeometry $ atomicWriteIORef obj.outputGeometry
    for_ update.updateMode $ atomicWriteIORef obj.outputMode
    for_ update.updateScale $ atomicWriteIORef obj.outputScale
    for_ update.updateName $ atomicWriteIORef obj.outputName
    for_ update.updateDescription $ atomicWriteIORef obj.outputDescription
    forwardMessage obj msg
  onEvent obj msg@(Event_wl_output_scale factor) = do
    atomicModifyIORef' obj.outputUpdate $ \u -> (u{updateScale = Just factor}, ())
    forwardMessage obj msg
  onEvent obj msg@(Event_wl_output_name name) = do
    atomicModifyIORef' obj.outputUpdate $ \u -> (u{updateName = Just name}, ())
    forwardMessage obj msg
  onEvent obj msg@(Event_wl_output_description description) = do
    atomicModifyIORef' obj.outputUpdate $ \u -> (u{updateDescription = Just description}, ())
    forwardMessage obj msg

-- }}}

-- Wl_region {{{
instance Object Wl_region where
  onRequest obj msg@Request_wl_region_destroy{} = do
    forwardMessage obj msg
    dropObject obj.wlid
  onRequest obj msg@(Request_wl_region_add x y w h) = do
    atomicModifyIORef' obj.included $ \rs -> (Rectangle{position = (x, y), size = (w, h)} : rs, ())
    forwardMessage obj msg
  onRequest obj msg@(Request_wl_region_subtract x y w h) = do
    atomicModifyIORef' obj.excluded $ \rs -> (Rectangle{position = (x, y), size = (w, h)} : rs, ())
    forwardMessage obj msg

  onEvent _ = \case {}

-- }}}

-- Wl_subcompositor {{{
instance Object Wl_subcompositor where
  onRequest subcompositor msg@Request_wl_subcompositor_destroy = do
    forwardMessage subcompositor msg
    dropObject subcompositor.wlid
  onRequest subcompositor msg@(Request_wl_subcompositor_get_subsurface subsurfaceId surfaceId parentId) = do
    surface <-
      getInterface surfaceId >>= \case
        Nothing -> protocolViolation Unrecoverable msg subcompositor Enum_wl_subcompositor_error_bad_surface [wl|could not find surface|]
        Just surface -> pure surface
    SurfaceRole role <- readIORef surface.role
    case cast role of
      Just () -> pass
      Nothing -> protocolViolation Unrecoverable msg subcompositor Enum_wl_subcompositor_error_bad_surface [wl|surface already has a role assigned|]
    position <- newIORef (0, 0)
    synchronized <- newIORef True
    registerObject msg Wl_subsurface{wlid = subsurfaceId, surface = surfaceId, parent = parentId, position, synchronized}
    atomicWriteIORef surface.role $ SurfaceRole subsurfaceId
    forwardMessage subcompositor msg

  onEvent _ = \case {}

-- }}}

-- Wl_subsurface {{{
instance Object Wl_subsurface where
  onRequest subsurface msg@Request_wl_subsurface_destroy = do
    surfaceObj' <- getInterface subsurface.surface
    case surfaceObj' of
      Just surfaceObj -> atomicWriteIORef surfaceObj.role $ SurfaceRole ()
      Nothing -> pass
    parentObj' <- getInterface subsurface.parent
    case parentObj' of
      Just parentObj -> do
        atomicWriteIORef parentObj.role $ SurfaceRole ()
        atomically . modifyTVar' parentObj.state $ \state' ->
          state'
            { sSubsurfaces =
                state'.sSubsurfaces
                  { above = Seq.filter (/= subsurface.surface) state'.sSubsurfaces.above
                  , below = Seq.filter (/= subsurface.surface) state'.sSubsurfaces.below
                  }
            }
      Nothing -> pass
    forwardMessage subsurface msg
    dropObject subsurface.wlid
  onRequest subsurface msg@(Request_wl_subsurface_set_position x y) = do
    atomicWriteIORef subsurface.position (x, y)
    forwardMessage subsurface msg
  onRequest subsurface msg@(Request_wl_subsurface_place_below sibling) = do
    parentSurface' <- getInterface subsurface.parent
    case parentSurface' of
      Nothing -> protocolViolation Unrecoverable msg subsurface.wlid Enum_wl_subsurface_error_bad_surface [wl|invalid parent surface|]
      Just parentSurface -> do
        state' <- readTVarIO parentSurface.state
        b <- atomicModifyIORef' parentSurface.pendingState $ \s ->
          let stack = fromMaybe state'.sSubsurfaces s.cuSubsurfaces
              joint = stack.below <> stack.above
              (before, after) = Seq.breakl (== sibling) joint
              joint2 = (before Seq.|> subsurface.surface) <> after
              (below, above) = Seq.breakl (== subsurface.parent) joint2
           in bool (s{cuSubsurfaces = Just SubsurfaceStack{below, above}}, True) (s, False) (after == Seq.empty)
        unless b $ protocolViolation Unrecoverable msg subsurface.wlid Enum_wl_subsurface_error_bad_surface [wl|wl_surface is not a sibling or the parent|]
    forwardMessage subsurface msg
  onRequest subsurface msg@(Request_wl_subsurface_place_above sibling) = do
    parentSurface' <- getInterface subsurface.parent
    case parentSurface' of
      Nothing -> protocolViolation Unrecoverable msg subsurface.wlid Enum_wl_subsurface_error_bad_surface [wl|invalid parent surface|]
      Just parentSurface -> do
        state' <- readTVarIO parentSurface.state
        b <- atomicModifyIORef' parentSurface.pendingState $ \s ->
          let stack = fromMaybe state'.sSubsurfaces s.cuSubsurfaces
           in case Seq.breakl (== sibling) (stack.below <> stack.above) of
                (before, found Seq.:<| after) ->
                  let (below, above) = Seq.breakl (== subsurface.parent) ((before Seq.|> found Seq.|> subsurface.surface) <> after)
                   in (s{cuSubsurfaces = Just SubsurfaceStack{below, above}}, True)
                (_, Seq.Empty) -> (s, False)
        unless b $ protocolViolation Unrecoverable msg subsurface.wlid Enum_wl_subsurface_error_bad_surface [wl|wl_surface is not a sibling or the parent|]
    forwardMessage subsurface msg
  onRequest subsurface msg@Request_wl_subsurface_set_sync = do
    atomicWriteIORef subsurface.synchronized True
    forwardMessage subsurface msg
  onRequest subsurface msg@Request_wl_subsurface_set_desync = do
    atomicWriteIORef subsurface.synchronized False
    forwardMessage subsurface msg

  onEvent _ = \case {}

-- }}}

-- Wl_fixes {{{
instance Object Wl_fixes where
  onRequest fixes msg@Request_wl_fixes_destroy{} = do
    forwardMessage fixes msg
    dropObject fixes.wlid
  onRequest fixes msg@(Request_wl_fixes_destroy_registry registry) = do
    forwardMessage fixes msg
    dropObject registry

  onEvent _ = \case {}

-- }}}

-- Wrapper Functions, for QoL {{{

-- | Wait until the compositor has handled every request sent before this, like libwayland's @wl_display_roundtrip@.
-- The event loop must be running in another thread, as it delivers the reply.
roundtrip :: Wayland Client ()
roundtrip = newObject (Wl_display wlDisplayId) Request_wl_display_sync >>= takeMVar . (.done)

-- | Bind the first advertised global of an interface. Throws t'MissingGlobal' if the global isn't advertised.
-- **This function does not check if all global announcements have arrived**! You probably want to use `roundtrip` beforehand.
-- That will make the server confirm when its done sending globals.
-- Use `tryBindToInterface` for a version returning Maybe instead of throwing.
bindToInterface :: forall i. (Global i) => Wl_registry -> Wayland Client i
bindToInterface registry =
  tryBindToInterface @i registry
    >>= maybe (throwIO . MissingGlobal $ getInterfaceName (Proxy @i)) pure

-- | Like `bindToInterface`. But returns 'Nothing' if the global isn't advertised.
tryBindToInterface :: forall i. (Global i) => Wl_registry -> Wayland Client (Maybe i)
tryBindToInterface registry = do
  let targetIface = getInterfaceName (Proxy @i)
  logInfo $ "binding to " <> show targetIface
  env <- getConnectionEnv
  globals :: Map GlobalName (WlText, Word32) <- readIORef env.globals
  let matchingIfaces = [(name, ver) | (name, (advertisedIface, ver)) <- Map.toList globals, advertisedIface == targetIface]
  case matchingIfaces of
    [] -> pure Nothing
    (name, advertisedVersion) : _ -> do
      let negotiatedVersion = min advertisedVersion $ getInterfaceVersion (Proxy @i)
      oid <- withNewObjectID $ \oid -> oid <$ sendMsg registry (Request_wl_registry_bind name (WlNewId targetIface negotiatedVersion oid))
      Just <$> (getInterface (TObjectID $ FromClient oid) >>= maybe (error "sayland bug: bind did not register the global") pure)

-- }}}
-- }}}

$(generateTables wlFormatter "xml-protocols/wayland.xml")

-- vim: foldmethod=marker
