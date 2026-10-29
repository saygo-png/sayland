-- | Description : Internal custom Prelude.
--
-- Not meant for use by users of the library. It is here to avoid a whole Relude dependency.
-- Some of the module is directly copy pasted from Relude.
-- It is an exposed module in case someone does find some use in them.
--
-- On the dejafu branch, its concurrency primitives are dejafu's, from "Sayland.Internal.Conc", under the names main uses.
module Sayland.Internal.Prelude (
  module Prelude,
  module Sayland.Internal.Conc,
) where

import Sayland.Internal.Conc
import Prelude
