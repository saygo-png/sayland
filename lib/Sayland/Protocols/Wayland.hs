-- | Description: Implementation of @core\/wayland@.
module Sayland.Protocols.Wayland (module Sayland.Internal.Protocols.Wayland) where

import Sayland.Internal.Protocols.Wayland hiding (applyContentUpdate, cacheContentUpdate, checkPeerIDOrder, flushContentUpdates, lastCommittedStack, mapShmPool, mergeContentUpdate, synchronizedParent, unmapShmPoolRef, wlDisplayId)
