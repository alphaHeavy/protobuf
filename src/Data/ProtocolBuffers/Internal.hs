{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverlappingInstances #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Data.ProtocolBuffers.Internal
  ( Encode(..)
  , Decode(..)
  , decodeMessage
  , decodeLengthPrefixedMessage
  , Required
  , Optional
  , Repeated
  , GetValue(..)
  , Enumeration
  , EmbeddedMessage
  , GetEnum(..)
  , Signed(..)
  , Fixed(..)
  , Wire(..)
  ) where

import Control.Applicative
import Control.DeepSeq (NFData)
import Control.Monad
import Control.Monad.Identity
import Data.Bits
import Data.Foldable
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HashMap
import Data.Monoid
import Data.Serialize.Get
import Data.Serialize.Put
import Data.Tagged
import Data.Traversable
import qualified Data.TypeLevel as Tl

import GHC.Generics

import Data.ProtocolBuffers.Wire

decodeMessage :: Decode a => Get a
decodeMessage = decode =<< go HashMap.empty where
  go msg = do
    mfield <- Just `fmap` getField <|> return Nothing
    case mfield of
      Just v  -> go $! HashMap.insertWith (flip (++)) (fieldTag v) [v] msg
      Nothing -> return msg

--encodeMessage :: (GEncode (Rep a), Generic a) => a -> Put
--encodeMessage = gencode . from

newtype EmbeddedMessage m = EmbeddedMessage m
  deriving (Bits, Bounded, Enum, Eq, Floating, Foldable, Fractional, Functor, Integral, Monoid, NFData, Num, Ord, Real, RealFloat, RealFrac, Show, Traversable)

instance (Encode m, Decode m) => Wire (EmbeddedMessage m) where
  decodeWire (DelimitedField _ bs) =
    case runGet decodeMessage bs of
      Right val -> pure $ EmbeddedMessage val
      Left _err -> empty
  decodeWire _ = empty
  encodeWire t (EmbeddedMessage m) =
    encodeWire t . runPut $ encode m

class GetValue a where
  type GetValueType a :: *
  getValue :: a -> GetValueType a
  putValue :: GetValueType a -> a

instance GetValue (Optional n a) where
  type GetValueType (Tagged n (Maybe a)) = Maybe a
  getValue = unTagged
  putValue = Tagged

instance GetValue (Repeated n a) where
  type GetValueType (Tagged n [a]) = [a]
  getValue = unTagged
  putValue = Tagged

instance GetValue (Required n a) where
  type GetValueType (Tagged n (Identity a)) = a
  getValue = runIdentity . unTagged
  putValue = Tagged . Identity

-- field rules
type Optional (n :: *) a = Tagged n (Maybe a)
type Required (n :: *) a = Tagged n (Identity a)
type Repeated (n :: *) a = Tagged n [a]

instance Show a => Show (Required n a) where
  show (Tagged (Identity x)) = show (Tagged x :: Tagged n a)

instance Enum a => GetEnum (Optional n (Enumeration a)) where
  type GetEnumResult (Tagged n (Maybe (Enumeration a))) = Maybe a
  getEnum = fmap getEnum . unTagged
  putEnum = Tagged . fmap putEnum

instance Enum a => GetEnum (Required n (Enumeration a)) where
  type GetEnumResult (Tagged n (Identity (Enumeration a))) = a
  getEnum = getEnum . runIdentity . unTagged
  putEnum = Tagged . Identity . putEnum

instance Enum a => GetEnum (Repeated n (Enumeration a)) where
  type GetEnumResult (Tagged n [Enumeration a]) = [a]
  getEnum = fmap getEnum . unTagged
  putEnum = Tagged . fmap putEnum

class GDecode (f :: * -> *) where
  gdecode :: (Alternative m, Monad m) => HashMap Tag [Field] -> m (f a)

instance GDecode a => GDecode (M1 i c a) where
  gdecode = fmap M1 . gdecode

class Decode (a :: *) where
  decode :: (Alternative m, Monad m) => HashMap Tag [Field] -> m a
  default decode :: (Alternative m, Monad m, Generic a, GDecode (Rep a)) => HashMap Tag [Field] -> m a
  decode = fmap to . gdecode

foldMapM :: (Monad m, Foldable t, Monoid b) => (a -> m b) -> t a -> m b
foldMapM f = foldlM go mempty where
  go !acc el = mappend acc `liftM` f el

instance (Wire a, Monoid a, Tl.Nat n) => GDecode (K1 i (Optional n a)) where
  gdecode msg =
    let tag = fromIntegral $ Tl.toInt (undefined :: n)
    in case HashMap.lookup tag msg of
      Just val -> K1 . Tagged <$> foldMapM decodeWire val
      Nothing  -> pure $ K1 mempty

instance (Wire a, Tl.Nat n) => GDecode (K1 i (Repeated n a)) where
  gdecode msg =
    let tag = fromIntegral $ Tl.toInt (undefined :: n)
    in case HashMap.lookup tag msg of
      Just val -> K1 . Tagged <$> traverse decodeWire val
      Nothing  -> pure $ K1 mempty

instance (Wire a, Monoid a, Tl.Nat n) => GDecode (K1 i (Required n a)) where
  gdecode msg =
    let tag = fromIntegral $ Tl.toInt (undefined :: n)
    in case HashMap.lookup tag msg of
      Just val -> K1 . Tagged . Identity <$> foldMapM decodeWire val
      Nothing  -> empty

{-
instance (Wire a, Monoid a, Tl.Nat n) => GDecode (K1 i (Packed n a)) where
  decode = error "packed fields are not implemented"
-}

instance (GDecode a, GDecode b) => GDecode (a :*: b) where
  gdecode msg = liftA2 (:*:) (gdecode msg) (gdecode msg)

instance (GDecode x, GDecode y) => GDecode (x :+: y) where
  gdecode msg = L1 <$> gdecode msg <|> R1 <$> gdecode msg

class GEncode (f :: * -> *) where
  gencode :: f a -> Put

class Encode (a :: *) where
  encode :: a -> Put
  default encode :: (Generic a, GEncode (Rep a)) => a -> Put
  encode = gencode . from

instance GEncode a => GEncode (M1 i c a) where
  gencode = gencode . unM1

instance (Wire a, Tl.Nat n, Foldable f) => GEncode (K1 i (Tagged n (f a))) where
  gencode (K1 (Tagged opt)) = traverse_ (encodeWire tag) opt where
    tag = fromIntegral $ Tl.toInt (undefined :: n)

instance (GEncode a, GEncode b) => GEncode (a :*: b) where
  gencode (x :*: y) = gencode x >> gencode y

instance (GEncode a, GEncode b) => GEncode (a :+: b) where
  gencode (L1 x) = gencode x
  gencode (R1 y) = gencode y

decodeLengthPrefixedMessage :: Decode a => Get a
decodeLengthPrefixedMessage = do
  len <- getWord32le
  isolate (fromIntegral len) decodeMessage
