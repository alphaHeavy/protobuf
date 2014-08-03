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
  , GDecode(..)
  , fieldDecode
  ) where

import Control.Applicative
import Control.Monad
import qualified Data.ByteString as B
import Data.Foldable
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HashMap
import Data.Int (Int32, Int64)
import Data.Maybe (fromMaybe)
import Data.Monoid
import Data.Proxy
import Data.Serialize.Get
import Data.Traversable (traverse)

import GHC.Generics
import GHC.TypeLits

import Data.ProtocolBuffers.Types
import Data.ProtocolBuffers.Wire

-- |
-- Decode a Protocol Buffers message.
decodeMessage :: Decode a => Get a
{-# INLINE decodeMessage #-}
decodeMessage = decode =<< HashMap.map reverse <$> go HashMap.empty where
  go :: HashMap Tag [WireField] -> Get (HashMap Tag [WireField])
  go msg = do
    mfield <- Just <$> getWireField <|> return Nothing
    case mfield of
      Just v  -> go $! HashMap.insertWith (\(x:[]) xs -> x:xs) (wireFieldTag v) [v] msg
      Nothing -> return msg

-- |
-- Decode a Protocol Buffers message prefixed with a varint encoded 32-bit integer describing it's length.
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
  :: forall a b i n p . (DecodeWire a, Monoid a, KnownNat n)
  => (a -> b)
  -> HashMap Tag [WireField]
  -> Get (K1 i (Field n b) p)
{-# INLINE fieldDecode #-}
fieldDecode c msg =
  let tag = fromIntegral $ natVal (Proxy :: Proxy n)
  in case HashMap.lookup tag msg of
    Just val -> K1 . Field . c <$> foldMapM decodeWire val
    Nothing  -> pure . K1 . Field $ c empty

instance (DecodeWire a, KnownNat n) => GDecode (K1 i (Field n (OptionalField (Last (Value a))))) where
  gdecode msg = fieldDecode Optional msg <|> pure (K1 mempty)

instance (Enum a, KnownNat n) => GDecode (K1 i (Field n (RequiredField (Always (Enumeration a))))) where
  gdecode msg = do
    K1 mx <- fieldDecode Required msg
    case mx :: Field n (RequiredField (Always (Value Int32))) of
      Field (Required (Always (Value x))) ->
        return . K1 . Field . Required . Always . Enumeration . toEnum $ fromIntegral x

instance (Enum a, KnownNat n) => GDecode (K1 i (Field n (OptionalField (Last (Enumeration a))))) where
  gdecode msg = do
    K1 mx <- fieldDecode Optional msg
    case mx :: Field n (OptionalField (Last (Value Int32))) of
      Field (Optional (Last (Just (Value x)))) ->
        return . K1 . Field . Optional . Last . Just . Enumeration . toEnum $ fromIntegral x
      _ -> pure (K1 mempty)

instance (DecodeWire a, KnownNat n) => GDecode (K1 i (Repeated n a)) where
  gdecode msg =
    let tag = fromIntegral $ natVal (Proxy :: Proxy n)
    in case HashMap.lookup tag msg of
      Just val -> K1 . Field . Repeated <$> traverse decodeWire val
      Nothing  -> pure $ K1 mempty

instance (DecodeWire a, KnownNat n) => GDecode (K1 i (Field n (RequiredField (Always (Value a))))) where
  gdecode msg = fieldDecode Required msg

instance (DecodeWire (PackedList a), KnownNat n) => GDecode (K1 i (Packed n a)) where
  gdecode msg = fieldDecode PackedField msg

instance GDecode U1 where
  gdecode _ = return U1

-- |
-- foldMapM implemented in a way that defers using (mempty :: b) unless the
-- Foldable is empty, this allows the gross hack of pretending Always is
-- a Monoid while strictly evaluating the accumulator
foldMapM :: (Monad m, Foldable t, Monoid b) => (a -> m b) -> t a -> m b
foldMapM f = liftM (fromMaybe mempty) . foldlM go Nothing where
  go (Just !acc) = liftM (Just . mappend acc) . f
  go Nothing     = liftM Just . f
