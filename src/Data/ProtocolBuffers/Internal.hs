module Data.ProtocolBuffers.Internal
  ( Tag
  , Field(..)
  , fieldTag
  , getField
  , EncodeWire(..)
  , DecodeWire(..)
  , zzEncode32
  , zzEncode64
  , zzDecode32
  , zzDecode64
  ) where

import Data.ProtocolBuffers.Wire
