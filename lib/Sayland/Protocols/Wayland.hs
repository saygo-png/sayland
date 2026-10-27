-- | Description: Implementation of @core\/wayland@.
module Sayland.Protocols.Wayland (module Sayland.Internal.Protocols.Wayland) where

import Sayland.Internal.Protocols.Wayland hiding (applyContentUpdate, checkPeerIDOrder, contents, lastCommittedStack, mapShmPool, synchronizedParent, unmapShmPoolRef, updateReady, wlDisplayId)
