-- | Description: Re-exports all public modules of Sayland and Sayland-wire.
module Sayland (
  module Sayland.Wire,
  module Sayland.Connection,
  module Sayland.Core,
  module Sayland.Log,
  module Sayland.Object,
  module Sayland.Protocols.DmaBuf,
  module Sayland.Protocols.ExtWorkspace,
  module Sayland.Protocols.Fifo,
  module Sayland.Protocols.Wayland,
  module Sayland.Protocols.WlrLayerShell,
  module Sayland.Protocols.XdgShell,
  allProtocols,
) where

import Sayland.Connection
import Sayland.Core
import Sayland.Internal.Prelude
import Sayland.Log
import Sayland.Object
import Sayland.Protocols.DmaBuf
import Sayland.Protocols.ExtWorkspace
import Sayland.Protocols.Fifo
import Sayland.Protocols.Wayland
import Sayland.Protocols.WlrLayerShell
import Sayland.Protocols.XdgShell
import Sayland.Wire

allProtocols :: ProtocolTable
allProtocols =
  mconcat
    [ linux_dmabuf_v1Table
    , ext_workspace_v1Table
    , fifo_v1Table
    , waylandTable
    , wlr_layer_shell_unstable_v1Table
    , xdg_shellTable
    ]
