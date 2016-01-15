-- |
-- Messages containing 'Optional' 'Enumeration' fields fail to encode.
-- This module contains orphan instances required to make these functional.
--
-- For more information reference the associated ticket:
-- <https://github.com/alphaHeavy/protobuf/issues/3>

module Data.ProtocolBuffers.Orphans (Foldable) where

import Data.Orphans ()
