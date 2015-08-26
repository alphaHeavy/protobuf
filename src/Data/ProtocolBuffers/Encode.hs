{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}

module Data.ProtocolBuffers.Encode
  ( Encode(..)
  , encodeMessage
  , encodeLengthPrefixedMessage
  , GEncode
  ) where

import Data.Foldable
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HashMap
import Data.Proxy
import Data.Binary.Builder.Sized
import Data.Monoid

import GHC.Generics
import GHC.TypeLits

import Data.ProtocolBuffers.Types
import Data.ProtocolBuffers.Wire

-- |
-- Encode a Protocol Buffers message.
encodeMessage :: Encode a => a -> Builder
encodeMessage = encode

-- |
-- Encode a Protocol Buffers message prefixed with a varint encoded 32-bit integer describing its length.
encodeLengthPrefixedMessage :: Encode a => a -> Builder
{-# INLINE encodeLengthPrefixedMessage #-}
encodeLengthPrefixedMessage msg = (putVarUInt $ size msg') <> msg'
  where
    msg' = encodeMessage msg

class Encode (a :: *) where
  encode :: a -> Builder
  default encode :: (Generic a, GEncode (Rep a)) => a -> Builder
  encode = gencode . from

-- | Untyped message encoding
instance Encode (HashMap Tag [WireField]) where
  encode = foldMap step . HashMap.toList where
    step = uncurry (foldMap . encodeWire)

class GEncode (f :: * -> *) where
  gencode :: f a -> Builder

instance GEncode a => GEncode (M1 i c a) where
  gencode = gencode . unM1

instance (GEncode a, GEncode b) => GEncode (a :*: b) where
  gencode (x :*: y) = gencode x <> gencode y

instance (GEncode a, GEncode b) => GEncode (a :+: b) where
  gencode (L1 x) = gencode x
  gencode (R1 y) = gencode y

instance (EncodeWire a, KnownNat n, Foldable f) => GEncode (K1 i (Field n (f a))) where
  gencode = foldMap (encodeWire tag) . runField . unK1 where
    tag = fromIntegral $ natVal (Proxy :: Proxy n)

instance GEncode U1 where
  gencode _ = empty
