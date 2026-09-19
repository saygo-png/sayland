{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

module Sayland.Protocols.DmaBuf (module Sayland.Protocols.DmaBuf) where

import Control.Exception (try)
import Data.Bits (Bits (shiftL), (.|.))
import Foreign (Storable (peek), castPtr, nullPtr, plusPtr)
import MMAP (mapShared, mkMmapFlags, mmap, protRead, protWrite)
import Relude
import Sayland.Codegen
import Sayland.Core
import Sayland.Object
import Sayland.Protocols.Wayland
import Sayland.Wire
import System.Posix (closeFd)

$(loadProtocolFileEnums False "protocols/linux-dmabuf-v1.xml")

newtype Zwp_linux_dmabuf_v1 = Zwp_linux_dmabuf_v1 {wlid :: TObjectID Zwp_linux_dmabuf_v1}

data Dmabuf = Dmabuf
  { dmabufFd :: WlFd
  , dmabufPlaneIdx :: WlUInt
  , dmabufOffset :: WlUInt
  , dmabufStride :: WlUInt
  , dmabufModifier :: Word64
  }

data DmabufBuffer = DmabufBuffer
  { dmabufs :: [Dmabuf]
  , format :: WlUInt
  , flags :: Enum_zwp_linux_buffer_params_v1_flags
  }

instance BufferBackend DmabufBuffer where
  releaseBuffer DmabufBuffer{dmabufs} = liftIO $ forM_ dmabufs $ \Dmabuf{dmabufFd = WlFd fd} -> closeFd fd

data Zwp_linux_buffer_params_v1 = Zwp_linux_buffer_params_v1 {wlid :: TObjectID Zwp_linux_buffer_params_v1, dmabufSet :: IORef [Dmabuf], sampling_device :: IORef (Maybe WlArray)}

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

$(loadProtocolFile wlFormatter False "protocols/linux-dmabuf-v1.xml")

instance NewInterface Zwp_linux_dmabuf_feedback_v1 where
  newInterface wlid = do
    feedbackState <- newIORef Feedback{formatTable = [], tranches = [], pendingTranche = emptyTranche}
    pendingFeedbackState <- newIORef Feedback{formatTable = [], tranches = [], pendingTranche = emptyTranche}
    pure Zwp_linux_dmabuf_feedback_v1{feedbackSurface = Nothing, ..}

instance NewInterface Zwp_linux_buffer_params_v1 where
  newInterface wlid = do
    dmabufSet <- newIORef []
    sampling_device <- newIORef Nothing
    pure Zwp_linux_buffer_params_v1{..}

-- Tables {{{
$(generateTables False wlFormatter "protocols/linux-dmabuf-v1.xml")

-- }}}

instance Interface' Zwp_linux_dmabuf_v1 Client where
  runRequest dmabuf request@Request_zwp_linux_dmabuf_v1_destroy = do
    dropObject dmabuf.wlid
    sendMessage' request dmabuf.wlid
  runRequest dmabuf request@(Request_zwp_linux_dmabuf_v1_create_params paramsId) = do
    void $ newObject paramsId =<< newInterface paramsId
    sendMessage' request dmabuf.wlid
  runRequest dmabuf request@(Request_zwp_linux_dmabuf_v1_get_surface_feedback feedbackId surfaceId) = do
    feedback <- newInterface feedbackId
    void $ newObject feedbackId feedback{feedbackSurface = Just surfaceId}
    sendMessage' request dmabuf.wlid
  runRequest dmabuf request@(Request_zwp_linux_dmabuf_v1_get_default_feedback feedbackId) = do
    void $ newObject feedbackId =<< newInterface feedbackId
    sendMessage' request dmabuf.wlid
  runEvent _ (Event_zwp_linux_dmabuf_v1_format _) = error "deprecated event"
  runEvent _ (Event_zwp_linux_dmabuf_v1_modifier{}) = error "deprecated event"

instance Interface' Zwp_linux_dmabuf_v1 Server where
  runRequest dmabuf Request_zwp_linux_dmabuf_v1_destroy = dropObject dmabuf.wlid
  runRequest _ (Request_zwp_linux_dmabuf_v1_create_params paramsId) = void $ newObject paramsId =<< newInterface paramsId
  runRequest _ (Request_zwp_linux_dmabuf_v1_get_surface_feedback feedbackId surfaceId) = do
    feedback <- newInterface feedbackId
    void $ newObject feedbackId feedback{feedbackSurface = Just surfaceId}
  runRequest _ (Request_zwp_linux_dmabuf_v1_get_default_feedback feedbackId) = void $ newObject feedbackId =<< newInterface feedbackId
  runEvent _ (Event_zwp_linux_dmabuf_v1_format _) = pass
  runEvent _ (Event_zwp_linux_dmabuf_v1_modifier{}) = pass

instance Interface' Zwp_linux_buffer_params_v1 Client where
  runRequest params request@Request_zwp_linux_buffer_params_v1_destroy = do
    dropObject params.wlid
    sendMessage' request params.wlid
  runRequest params request@(Request_zwp_linux_buffer_params_v1_add dmabufFd dmabufPlaneIdx dmabufOffset dmabufStride (WlUInt modifier_hi) (WlUInt modifier_lo)) = do
    let dmabufModifier :: Word64 = shiftL (fromIntegral modifier_hi) 32 .|. fromIntegral modifier_lo
    atomicModifyIORef params.dmabufSet $ (,()) . (Dmabuf{dmabufFd, dmabufOffset, dmabufStride, dmabufModifier, dmabufPlaneIdx} :)
    sendMessage' request params.wlid
  runRequest params request@(Request_zwp_linux_buffer_params_v1_create _width _height _format _flags) = do
    sendMessage' request params.wlid
  runRequest params request@(Request_zwp_linux_buffer_params_v1_create_immed bufferId width height format flags) = do
    buffer <- newInterface bufferId
    dmabufs <- atomicModifyIORef params.dmabufSet ([],)
    let dmabuf = DmabufBuffer{..}
    void $ newObject bufferId buffer{width, height, buffer = Buffer dmabuf}
    sendMessage' request params.wlid
  runRequest params request@(Request_zwp_linux_buffer_params_v1_set_sampling_device dev) = do
    writeIORef params.sampling_device $ Just dev
    sendMessage' request params.wlid

instance Interface' Zwp_linux_buffer_params_v1 Server where
  runRequest params Request_zwp_linux_buffer_params_v1_destroy = dropObject params.wlid
  runRequest params (Request_zwp_linux_buffer_params_v1_add dmabufFd dmabufPlaneIdx dmabufOffset dmabufStride (WlUInt modifier_hi) (WlUInt modifier_lo)) = do
    let dmabufModifier :: Word64 = shiftL (fromIntegral modifier_hi) 32 .|. fromIntegral modifier_lo
    atomicModifyIORef params.dmabufSet $ (,()) . (Dmabuf{dmabufFd, dmabufOffset, dmabufStride, dmabufModifier, dmabufPlaneIdx} :)
  runRequest _params Request_zwp_linux_buffer_params_v1_create{} = pass -- the compositor handles this event
  runRequest _params Request_zwp_linux_buffer_params_v1_create_immed{} = pass -- the compositor handles this event
  runRequest params (Request_zwp_linux_buffer_params_v1_set_sampling_device dev) = writeIORef params.sampling_device $ Just dev

instance Interface' Zwp_linux_dmabuf_feedback_v1 Client where
  runRequest feedback request@Request_zwp_linux_dmabuf_feedback_v1_destroy = do
    dropObject feedback.wlid
    sendMessage' request feedback.wlid
  runEvent feedback Event_zwp_linux_dmabuf_feedback_v1_done = do
    fstate <- readIORef feedback.pendingFeedbackState
    writeIORef feedback.feedbackState fstate
  runEvent feedback (Event_zwp_linux_dmabuf_feedback_v1_format_table (WlFd fd) (WlUInt size)) = do
    result <-
      liftIO
        $ try
        $ mmap
          nullPtr
          (fromIntegral size)
          (protRead <> protWrite)
          (mkMmapFlags mapShared mempty)
          fd
          0
    ptr <- case result of
      Left (e :: SomeException) -> error $ toText $ "mmap failed: " ++ show e
      Right ptr' -> pure ptr'
    formatTable <- liftIO $ forM [0 .. fromIntegral size `div` 16] $ \n -> do
      let ptr' = ptr `plusPtr` (16 * n)
      format :: Word32 <- peek $ castPtr ptr'
      _ :: Word32 <- peek (ptr' `plusPtr` 32)
      modifier :: Word64 <- peek (ptr' `plusPtr` 64)
      pure (format, modifier)
    atomicModifyIORef feedback.pendingFeedbackState $ (,()) . (\x -> x{formatTable})
  runEvent _feedback (Event_zwp_linux_dmabuf_feedback_v1_main_device _) = error "deprecated"
  runEvent feedback Event_zwp_linux_dmabuf_feedback_v1_tranche_done = atomicModifyIORef feedback.pendingFeedbackState $ \s -> (s{tranches = s.tranches <> [s.pendingTranche], pendingTranche = emptyTranche}, ())
  runEvent feedback (Event_zwp_linux_dmabuf_feedback_v1_tranche_flags trancheFlags) = atomicModifyIORef feedback.pendingFeedbackState $ \s -> (s{pendingTranche = s.pendingTranche{trancheFlags}}, ())
  runEvent feedback (Event_zwp_linux_dmabuf_feedback_v1_tranche_formats format) = atomicModifyIORef feedback.pendingFeedbackState $ \s -> (s{pendingTranche = s.pendingTranche{trancheFormats = format : s.pendingTranche.trancheFormats}}, ())
  runEvent feedback (Event_zwp_linux_dmabuf_feedback_v1_tranche_target_device targetDevice) = atomicModifyIORef feedback.pendingFeedbackState $ \s -> (s{pendingTranche = s.pendingTranche{targetDevice}}, ())

instance Interface' Zwp_linux_dmabuf_feedback_v1 Server where
  runRequest feedback Request_zwp_linux_dmabuf_feedback_v1_destroy = dropObject feedback.wlid
  runEvent feedback event@Event_zwp_linux_dmabuf_feedback_v1_done = do
    fstate <- readIORef feedback.pendingFeedbackState
    writeIORef feedback.feedbackState fstate
    sendMessage' event feedback.wlid
  runEvent feedback (Event_zwp_linux_dmabuf_feedback_v1_format_table (WlFd fd) (WlUInt size)) = do
    result <-
      liftIO
        $ try
        $ mmap
          nullPtr
          (fromIntegral size)
          (protRead <> protWrite)
          (mkMmapFlags mapShared mempty)
          fd
          0
    ptr <- case result of
      Left (e :: SomeException) -> error $ toText $ "mmap failed: " ++ show e
      Right ptr' -> pure ptr'
    formatTable <- liftIO $ forM [0 .. fromIntegral size `div` 16] $ \n -> do
      let ptr' = ptr `plusPtr` (16 * n)
      format :: Word32 <- peek $ castPtr ptr'
      _ :: Word32 <- peek (ptr' `plusPtr` 32)
      modifier :: Word64 <- peek (ptr' `plusPtr` 64)
      pure (format, modifier)
    atomicModifyIORef feedback.pendingFeedbackState $ (,()) . (\x -> x{formatTable})
  runEvent _feedback (Event_zwp_linux_dmabuf_feedback_v1_main_device _) = pass
  runEvent feedback event@Event_zwp_linux_dmabuf_feedback_v1_tranche_done = do
    atomicModifyIORef feedback.pendingFeedbackState $ \s -> (s{tranches = s.tranches <> [s.pendingTranche], pendingTranche = emptyTranche}, ())
    sendMessage' event feedback.wlid
  runEvent feedback event@(Event_zwp_linux_dmabuf_feedback_v1_tranche_flags trancheFlags) = do
    atomicModifyIORef feedback.pendingFeedbackState $ \s -> (s{pendingTranche = s.pendingTranche{trancheFlags}}, ())
    sendMessage' event feedback.wlid
  runEvent feedback event@(Event_zwp_linux_dmabuf_feedback_v1_tranche_formats format) = do
    atomicModifyIORef feedback.pendingFeedbackState $ \s -> (s{pendingTranche = s.pendingTranche{trancheFormats = format : s.pendingTranche.trancheFormats}}, ())
    sendMessage' event feedback.wlid
  runEvent feedback event@(Event_zwp_linux_dmabuf_feedback_v1_tranche_target_device targetDevice) = do
    atomicModifyIORef feedback.pendingFeedbackState $ \s -> (s{pendingTranche = s.pendingTranche{targetDevice}}, ())
    sendMessage' event feedback.wlid

-- vim: foldmethod=marker
