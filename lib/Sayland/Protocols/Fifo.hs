-- | Description: Implementation of @wayland-protocols\/staging\/fifo-v1@.
--
-- A content update can raise the surface's fifo barrier when it is applied (@set_barrier@), and wait while the barrier
-- is up (@wait_barrier@). This module adds both through the surface's hooks: the barrier is a surface extension,
-- @set_barrier@ adds to @cuOnApply@ and @wait_barrier@ adds to @cuConstraints@.
--
-- A compositor that advertises fifo, as one using `fifo_v1Table` does, lowers the barrier of the surfaces it presents
-- after each latching deadline with `clearFifoBarrier`. Without that, updates that wait for the barrier are never
-- applied.
module Sayland.Protocols.Fifo (module Sayland.Internal.Protocols.Fifo) where

import Sayland.Internal.Protocols.Fifo
