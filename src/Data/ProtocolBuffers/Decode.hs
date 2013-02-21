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
import qualified Data.ByteString as B
import Data.Foldable
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HashMap
import Data.Int (Int64)
import Data.Monoid
import Data.Serialize.Get
import qualified Data.TypeLevel as Tl

import GHC.Generics

import Data.ProtocolBuffers.Types
import Data.ProtocolBuffers.Wire

-- |
-- Decode a Protocol Buffers message.
decodeMessage :: Decode a => Get a
{-# INLINE decodeMessage #-}
decodeMessage = decode =<< go HashMap.empty where
  go :: HashMap Tag [WireField] -> Get (HashMap Tag [WireField])
  go msg = do
    mfield <- Just <$> getWireField <|> return Nothing
    case mfield of
      Just v  -> go $! HashMap.insertWith (flip (++)) (wireFieldTag v) [v] msg
      Nothing -> return msg

-- |
-- Decode a Protocol Buffers message prefixed with a 32-bit integer describing it's length.
decodeLengthPrefixedMessage :: Decode a => Get a
{-# INLINE decodeLengthPrefixedMessage #-}
decodeLengthPrefixedMessage = do
  len :: Int64 <- getVarInt
  bs <- getBytes $ fromIntegral len
  case runGetState decodeMessage bs 0 of
    Right (val, bs')
      | B.null bs' -> return val
      | otherwise  -> fail $ "Unparsed bytes leftover in decodeLengthPrefixedMessage: " ++ show (B.length bs')
    Left err  -> fail err

class Decode (a :: *) where
  decode :: HashMap Tag [WireField] -> Get a
  default decode :: (Generic a, GDecode (Rep a)) => HashMap Tag [WireField] -> Get a
  decode = fmap to . gdecode

-- | Untyped message decoding, @ 'decode' = 'id' @
instance Decode (HashMap Tag [WireField]) where
  decode = pure

class GDecode (f :: * -> *) where
  gdecode :: HashMap Tag [WireField] -> Get (f a)

instance GDecode a => GDecode (M1 i c a) where
  gdecode = fmap M1 . gdecode

instance (GDecode a, GDecode b) => GDecode (a :*: b) where
  gdecode msg = liftA2 (:*:) (gdecode msg) (gdecode msg)

instance (GDecode x, GDecode y) => GDecode (x :+: y) where
  gdecode msg = L1 <$> gdecode msg <|> R1 <$> gdecode msg

fieldDecode
  :: forall a b i n p . (DecodeWire a, Monoid a, Tl.Nat n)
  => (a -> b)
  -> HashMap Tag [WireField]
  -> Get (K1 i (Field n b) p)
{-# INLINE fieldDecode #-}
fieldDecode c msg =
  let tag = fromIntegral $ Tl.toInt (undefined :: n)
  in case HashMap.lookup tag msg of
    Just val -> K1 . Field . c <$> foldMapM decodeWire val
    Nothing  -> empty

instance (DecodeWire a, Monoid a, Tl.Nat n) => GDecode (K1 i (Field n (OptionalField a))) where
  gdecode msg = fieldDecode Optional msg <|> pure (K1 mempty)

instance (DecodeWire [a], Tl.Nat n) => GDecode (K1 i (Repeated n a)) where
  gdecode msg = fieldDecode Repeated msg <|> pure (K1 mempty)

instance (DecodeWire a, Monoid a, Tl.Nat n) => GDecode (K1 i (Field n (RequiredField a))) where
  gdecode msg = fieldDecode Required msg

instance (DecodeWire (PackedList a), Tl.Nat n) => GDecode (K1 i (Packed n a)) where
  gdecode msg = fieldDecode PackedField msg

foldMapM :: (Monad m, Foldable t, Monoid b) => (a -> m b) -> t a -> m b
foldMapM f = foldlM go mempty where
  go !acc el = mappend acc `liftM` f el
