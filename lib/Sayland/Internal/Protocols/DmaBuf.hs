{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_HADDOCK not-home #-}

-- | Description: Internals of Sayland.Protocols.DmaBuf
module Sayland.Internal.Protocols.DmaBuf (module Sayland.Internal.Protocols.DmaBuf) where

import Control.Exception (ErrorCall (..), SomeException)
import Control.Monad
import Control.Monad.Catch (try)
import Data.Binary
import Data.Binary.Get (getWord32host, getWord64host, runGet)
import Data.Bits (Bits (shiftL), (.|.))
import Data.ByteString qualified as BS
import Foreign (Ptr, plusPtr)
import Sayland.Internal.Codegen
import Sayland.Internal.Core
import Sayland.Internal.Prelude
import Sayland.Internal.Protocols.Wayland
import Sayland.Kernel (Kernel)
import Sayland.Kernel qualified as Kernel
import Sayland.Wire

$(loadProtocolFileEnums "xml-protocols/linux-dmabuf-v1.xml")

-- Interfaces {{{

newtype Zwp_linux_dmabuf_v1 = Zwp_linux_dmabuf_v1 {wlid :: TObjectID Zwp_linux_dmabuf_v1}

data Dmabuf = Dmabuf
  { dmabufFd :: Fd
  , dmabufPlaneIdx :: Word32
  , dmabufOffset :: Word32
  , dmabufStride :: Word32
  }

data DmabufBuffer = DmabufBuffer
  { dmabufs :: [Dmabuf]
  , format :: Word32
  , modifier :: Word64
  , flags :: Enum_zwp_linux_buffer_params_v1_flags
  }

instance BufferBackend DmabufBuffer where
  releaseBuffer DmabufBuffer{dmabufs} = getKernel >>= \kernel -> liftConc . forM_ dmabufs $ \Dmabuf{dmabufFd = fd} -> Kernel.closeFd kernel fd

data Zwp_linux_buffer_params_v1 = Zwp_linux_buffer_params_v1 {wlid :: TObjectID Zwp_linux_buffer_params_v1, dmabufSet :: IORef [Dmabuf], paramsModifier :: IORef Word64, sampling_device :: IORef (Maybe WlArray)}

data Tranche = Tranche
  { targetDevice :: WlArray
  , trancheFlags :: Enum_zwp_linux_dmabuf_feedback_v1_tranche_flags
  , trancheFormats :: [WlArray]
  }

emptyTranche :: Tranche
emptyTranche = Tranche{targetDevice = WlArray "", trancheFlags = Enum_zwp_linux_dmabuf_feedback_v1_tranche_flags False False, trancheFormats = []}

data Feedback = Feedback
  { formatTable :: [(Word32, Word64)]
  , tranches :: [Tranche]
  , pendingTranche :: Tranche
  }

data Zwp_linux_dmabuf_feedback_v1 = Zwp_linux_dmabuf_feedback_v1
  { wlid :: TObjectID Zwp_linux_dmabuf_feedback_v1
  , feedbackSurface :: Maybe (TObjectID Wl_surface)
  , feedbackState :: IORef Feedback
  , pendingFeedbackState :: IORef Feedback
  }

-- | Builder for a feedback object with no format table or tranches yet.
newFeedback :: TObjectID Zwp_linux_dmabuf_feedback_v1 -> Maybe (TObjectID Wl_surface) -> Wayland p Zwp_linux_dmabuf_feedback_v1
newFeedback wlid feedbackSurface = do
  let empty = Feedback{formatTable = [], tranches = [], pendingTranche = emptyTranche}
  feedbackState <- newIORef empty
  pendingFeedbackState <- newIORef empty
  pure Zwp_linux_dmabuf_feedback_v1{..}

-- }}}

$(loadProtocolFile wlFormatter "xml-protocols/linux-dmabuf-v1.xml")

-- | Read a number from mapped memory, in host byte order, like 'Foreign.Storable.peek'.
peekWord :: Kernel -> Get a -> Int -> Ptr () -> ConcIO a
peekWord kernel getter size ptr = runGet getter . BS.fromStrict <$> Kernel.peekBytes kernel ptr size

-- Implementations {{{
-- Zwp_linux_dmabuf_v1 {{{
instance Object Zwp_linux_dmabuf_v1 where
  onRequest dmabuf msg@Request_zwp_linux_dmabuf_v1_destroy = do
    forwardMessage dmabuf msg
    dropObject dmabuf.wlid
  onRequest dmabuf msg@(Request_zwp_linux_dmabuf_v1_create_params paramsId) = do
    dmabufSet <- newIORef []
    paramsModifier <- newIORef 0
    sampling_device <- newIORef Nothing
    registerObject msg Zwp_linux_buffer_params_v1{wlid = paramsId, ..}
    forwardMessage dmabuf msg
  onRequest dmabuf msg@(Request_zwp_linux_dmabuf_v1_get_surface_feedback feedbackId surfaceId) = do
    registerObject msg =<< newFeedback feedbackId (Just surfaceId)
    forwardMessage dmabuf msg
  onRequest dmabuf msg@(Request_zwp_linux_dmabuf_v1_get_default_feedback feedbackId) = do
    registerObject msg =<< newFeedback feedbackId Nothing
    forwardMessage dmabuf msg

  onEvent dmabuf msg = do
    case msg of
      Event_zwp_linux_dmabuf_v1_format{} -> logDebug "deprecated since v4, superseded by feedback"
      Event_zwp_linux_dmabuf_v1_modifier{} -> logDebug "deprecated since v4, superseded by feedback"
    forwardMessage dmabuf msg

-- }}}

-- Zwp_linux_buffer_params_v1 {{{
instance Object Zwp_linux_buffer_params_v1 where
  onRequest params msg@Request_zwp_linux_buffer_params_v1_destroy = do
    forwardMessage params msg
    dropObject params.wlid
  onRequest params msg@(Request_zwp_linux_buffer_params_v1_add dmabufFd dmabufPlaneIdx dmabufOffset dmabufStride modifier_hi modifier_lo) = do
    let dmabufModifier :: Word64 = shiftL (fromIntegral modifier_hi) 32 .|. fromIntegral modifier_lo
    modifier <- readIORef params.paramsModifier
    unless (dmabufModifier == modifier)
      $ protocolViolation Unrecoverable msg params Enum_zwp_linux_buffer_params_v1_error_invalid_format [wl|zwp_linux_buffer_params_v1.add: invalid format|]
    atomicModifyIORef' params.dmabufSet $ \ds -> (Dmabuf{dmabufFd, dmabufOffset, dmabufStride, dmabufPlaneIdx} : ds, ())
    forwardMessage params msg
  onRequest params msg@Request_zwp_linux_buffer_params_v1_create{} =
    forwardMessage params msg
  onRequest params msg@(Request_zwp_linux_buffer_params_v1_create_immed bufferId width height format flags) = do
    onClient $ do
      modifier <- readIORef params.paramsModifier
      dmabufs <- atomicModifyIORef' params.dmabufSet ([],)
      registerObject msg Wl_buffer{wlid = bufferId, width, height, buffer = Buffer DmabufBuffer{..}}
    forwardMessage params msg
  onRequest params msg@(Request_zwp_linux_buffer_params_v1_set_sampling_device dev) = do
    atomicWriteIORef params.sampling_device $ Just dev
    forwardMessage params msg

  onEvent obj msg = case msg of
    Event_zwp_linux_buffer_params_v1_created _ -> do
      -- The created wl_buffer is not registered yet.
      stub obj msg
      forwardMessage obj msg
    Event_zwp_linux_buffer_params_v1_failed -> forwardMessage obj msg

-- }}}

-- Zwp_linux_dmabuf_feedback_v1 {{{
instance Object Zwp_linux_dmabuf_feedback_v1 where
  onRequest feedback msg@Request_zwp_linux_dmabuf_feedback_v1_destroy = do
    dropObject feedback.wlid
    forwardMessage feedback msg

  onEvent feedback msg@Event_zwp_linux_dmabuf_feedback_v1_done = do
    fstate <- readIORef feedback.pendingFeedbackState
    atomicWriteIORef feedback.feedbackState fstate
    forwardMessage feedback msg
  onEvent feedback (Event_zwp_linux_dmabuf_feedback_v1_format_table fd size) = do
    kernel <- getKernel
    result <- liftConc . try $ Kernel.mmap kernel fd (fromIntegral size)
    ptr <- case result of
      Left (e :: SomeException) -> throwIO . ErrorCall $ "mmap failed: " ++ show e
      Right ptr' -> pure ptr'
    formatTable <- liftConc $ forM [0 .. fromIntegral size `div` 16] $ \n -> do
      let ptr' = ptr `plusPtr` (16 * n)
      format :: Word32 <- peekWord kernel getWord32host 4 ptr'
      _ :: Word32 <- peekWord kernel getWord32host 4 (ptr' `plusPtr` 32)
      modifier :: Word64 <- peekWord kernel getWord64host 8 (ptr' `plusPtr` 64)
      pure (format, modifier)
    atomicModifyIORef' feedback.pendingFeedbackState $ (,()) . (\x -> x{formatTable})
  onEvent feedback msg@(Event_zwp_linux_dmabuf_feedback_v1_main_device _) = do
    stubDeprecated feedback msg -- deprecated since v6
    forwardMessage feedback msg
  onEvent feedback msg@Event_zwp_linux_dmabuf_feedback_v1_tranche_done = do
    atomicModifyIORef' feedback.pendingFeedbackState $ \s -> (s{tranches = s.tranches <> [s.pendingTranche], pendingTranche = emptyTranche}, ())
    forwardMessage feedback msg
  onEvent feedback msg@(Event_zwp_linux_dmabuf_feedback_v1_tranche_flags trancheFlags) = do
    atomicModifyIORef' feedback.pendingFeedbackState $ \s -> (s{pendingTranche = s.pendingTranche{trancheFlags}}, ())
    forwardMessage feedback msg
  onEvent feedback msg@(Event_zwp_linux_dmabuf_feedback_v1_tranche_formats format) = do
    atomicModifyIORef' feedback.pendingFeedbackState $ \s -> (s{pendingTranche = s.pendingTranche{trancheFormats = format : s.pendingTranche.trancheFormats}}, ())
    forwardMessage feedback msg
  onEvent feedback msg@(Event_zwp_linux_dmabuf_feedback_v1_tranche_target_device targetDevice) = do
    atomicModifyIORef' feedback.pendingFeedbackState $ \s -> (s{pendingTranche = s.pendingTranche{targetDevice}}, ())
    forwardMessage feedback msg

-- }}}
-- }}}

$(generateTables wlFormatter "xml-protocols/linux-dmabuf-v1.xml")

-- vim: foldmethod=marker
