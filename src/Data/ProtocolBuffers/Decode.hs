{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}

module Data.ProtocolBuffers.Decode
  ( Decode(..)
  , decodeMessage
  , decodeLengthPrefixedMessage
  ) where

import Control.Applicative
import Control.Monad
import Control.Monad.Identity
import Data.Foldable
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HashMap
import Data.Int (Int32)
import Data.Monoid
import Data.Serialize.Get
import Data.Tagged
import Data.Traversable
import qualified Data.TypeLevel as Tl

import GHC.Generics

import Data.ProtocolBuffers.Types
import Data.ProtocolBuffers.Wire

-- |
-- Decode a Protocol Buffers message.
decodeMessage :: Decode a => Get a
{-# INLINE decodeMessage #-}
decodeMessage = decode =<< go HashMap.empty where
  go msg = do
    mfield <- Just <$> getField <|> return Nothing
    case mfield of
      Just v  -> go $! HashMap.insertWith (flip (++)) (fieldTag v) [v] msg
      Nothing -> return msg

-- |
-- Decode a Protocol Buffers message prefixed with a 32-bit integer describing it's length.
decodeLengthPrefixedMessage :: Decode a => Get a
{-# INLINE decodeLengthPrefixedMessage #-}
decodeLengthPrefixedMessage = do
  len :: Int32 <- getVarInt
  isolate (fromIntegral len) decodeMessage

class Decode (a :: *) where
  decode :: HashMap Tag [Field] -> Get a
  default decode :: (Generic a, GDecode (Rep a)) => HashMap Tag [Field] -> Get a
  decode = fmap to . gdecode

-- | Untyped message decoding, @ 'decode' = 'id' @
instance Decode (HashMap Tag [Field]) where
  decode = pure

class GDecode (f :: * -> *) where
  gdecode :: HashMap Tag [Field] -> Get (f a)

instance GDecode a => GDecode (M1 i c a) where
  gdecode = fmap M1 . gdecode

instance (GDecode a, GDecode b) => GDecode (a :*: b) where
  gdecode msg = liftA2 (:*:) (gdecode msg) (gdecode msg)

instance (GDecode x, GDecode y) => GDecode (x :+: y) where
  gdecode msg = L1 <$> gdecode msg <|> R1 <$> gdecode msg

instance (DecodeWire a, Monoid a, Tl.Nat n) => GDecode (K1 i (Optional n a)) where
  gdecode msg =
    let tag = fromIntegral $ Tl.toInt (undefined :: n)
    in case HashMap.lookup tag msg of
      Just val -> K1 . Tagged <$> foldMapM decodeWire val
      Nothing  -> pure $ K1 mempty

instance (DecodeWire a, Tl.Nat n) => GDecode (K1 i (Repeated n a)) where
  gdecode msg =
    let tag = fromIntegral $ Tl.toInt (undefined :: n)
    in case HashMap.lookup tag msg of
      Just val -> K1 . Tagged <$> traverse decodeWire val
      Nothing  -> pure $ K1 mempty

instance (DecodeWire a, Monoid a, Tl.Nat n) => GDecode (K1 i (Required n a)) where
  gdecode msg =
    let tag = fromIntegral $ Tl.toInt (undefined :: n)
    in case HashMap.lookup tag msg of
      Just val -> K1 . Tagged . Identity <$> foldMapM decodeWire val
      Nothing  -> empty

{-
instance (Wire a, Monoid a, Tl.Nat n) => GDecode (K1 i (Packed n a)) where
  decode = error "packed fields are not implemented"
-}

foldMapM :: (Monad m, Foldable t, Monoid b) => (a -> m b) -> t a -> m b
foldMapM f = foldlM go mempty where
  go !acc el = mappend acc `liftM` f el
