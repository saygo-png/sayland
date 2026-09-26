{-# LANGUAGE TypeFamilyDependencies #-}

-- | Description : Internal part of Sayland.Core.
module Sayland.Core.Internal (
  TObjectID (..),
  raw,
) where

import Data.Kind
import Sayland.Wire
import Prelude

-- | Type representing an `objectID` of a certain object.
newtype TObjectID (a :: Type) = TObjectID RawObjectID deriving newtype (Show, Eq, Ord)

type role TObjectID phantom

instance WireFormat (TObjectID a) where
  wireGet = TObjectID <$> wireGet
  wirePut (TObjectID o) = wirePut o

-- | Get the `RawObjectID` out of a `TObjectID`
raw :: TObjectID i -> RawObjectID
raw (TObjectID w) = w
