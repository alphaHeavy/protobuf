{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverlappingInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Data.ProtocolBuffers
  ( Encode(..)
  , Decode(..)
  , decodeMessage
  , decodeLengthPrefixed
  , Wire(..)
  , Required
  , Optional
  , Repeated
  , GetValue(..)
  , Enumeration
  , EmbeddedMessage
  , GetEnum(..)
  , Signed(..)
  , Fixed(..)
  ) where

import Control.Applicative
import Control.DeepSeq (NFData)
import Control.Monad
import Control.Monad.Identity
import Data.Bits
import Data.ByteString (ByteString)
import qualified Data.ByteString as B
import Data.Foldable
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HashMap
import Data.Int
import Data.Monoid
import Data.Serialize.Get
import Data.Serialize.IEEE754
import Data.Serialize.Put
import Data.Tagged
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import Data.Traversable
import qualified Data.TypeLevel as Tl
import Data.Word
import Data.Binary.IEEE754 (wordToDouble, wordToFloat)

import GHC.Generics

type Tag = Word32

data Field
  = VarintField    {-# UNPACK #-} !Tag {-# UNPACK #-} !Word64
  | Fixed64Field   {-# UNPACK #-} !Tag {-# UNPACK #-} !Word64
  | DelimitedField {-# UNPACK #-} !Tag !ByteString
  | StartField     {-# UNPACK #-} !Tag
  | EndField       {-# UNPACK #-} !Tag
  | Fixed32Field   {-# UNPACK #-} !Tag {-# UNPACK #-} !Word32
    deriving Show

getVarintPrefixedBS :: Get ByteString
getVarintPrefixedBS = getBytes =<< getVarInt

getField :: Get Field
getField = do
  wireTag :: Word64 <- getVarInt
  let tag = fromIntegral $ wireTag `shiftR` 3
  case wireTag .&. 7 of
    0 -> VarintField    tag <$> getVarInt
    1 -> Fixed64Field   tag <$> getWord64le
    2 -> DelimitedField tag <$> getVarintPrefixedBS
    3 -> return $! StartField tag
    4 -> return $! EndField   tag
    5 -> Fixed32Field   tag <$> getWord32le
    _ -> empty

putField :: Tag -> Word32 -> Put
putField tag typ = putVarUInt $ tag `shiftL` 3 .|. (typ .&. 7)

getVarInt :: (Integral a, Bits a) => Get a
getVarInt = go 0 0 where
  go n !val = do
    b <- getWord8
    if testBit b 7
      then go (n+7) (val .|. ((fromIntegral (b .&. 0x7F)) `shiftL` n))
      else return $! val .|. ((fromIntegral b) `shiftL` n)

-- This can be used on any Integral type and is needed for signed types; unsigned can use putVarUInt below.
-- This has been changed to handle only up to 64 bit integral values (to match documentation).
{-# INLINE putVarSInt #-}
putVarSInt :: (Integral a, Bits a) => a -> Put
putVarSInt bIn =
  case compare bIn 0 of
    LT -> let -- upcast to 64 bit to match documentation of 10 bytes for all negative values
              b = fromIntegral bIn
              len = 10                                -- (pred 10)*7 < 64 <= 10*7
              last'Mask = 1                           -- pred (1 `shiftL` 1)
              go :: Int64 -> Int -> Put
              go !i 1 = putWord8 (fromIntegral (i .&. last'Mask))
              go !i n = putWord8 (fromIntegral (i .&. 0x7F) .|. 0x80) >> go (i `shiftR` 7) (pred n)
          in go b len
    EQ -> putWord8 0
    GT -> putVarUInt bIn

-- This should be used on unsigned Integral types only (not checked)
{-# INLINE putVarUInt #-}
putVarUInt :: (Integral a, Bits a) => a -> Put
putVarUInt i
  | i < 0x80  = putWord8 (fromIntegral i)
  | otherwise = putWord8 (fromIntegral (i .&. 0x7F) .|. 0x80) >> putVarUInt (i `shiftR` 7)

fieldTag :: Field -> Tag
fieldTag f = case f of
  VarintField    t _ -> t
  Fixed64Field   t _ -> t
  DelimitedField t _ -> t
  StartField     t   -> t
  EndField       t   -> t
  Fixed32Field   t _ -> t

decodeMessage :: Decode a => Get a
decodeMessage = decode =<< go HashMap.empty where
  go msg = do
    mfield <- Just `fmap` getField <|> return Nothing
    case mfield of
      Just v  -> go $! HashMap.insertWith (flip (++)) (fieldTag v) [v] msg
      Nothing -> return msg

--encodeMessage :: (GEncode (Rep a), Generic a) => a -> Put
--encodeMessage = gencode . from

class Wire a where
  decodeWire :: Alternative m => Field -> m a
  encodeWire :: Tag -> a -> Put

deriving instance Wire a => Wire (First a)
deriving instance Wire a => Wire (Last a)
deriving instance Wire a => Wire (Product a)
deriving instance Wire a => Wire (Sum a)

instance Wire a => Wire (Maybe a) where
  decodeWire = fmap Just . decodeWire
  encodeWire t (Just a) = encodeWire t a
  encodeWire _ Nothing  = return ()

instance Wire Int32 where
  decodeWire (VarintField  _ val) = pure $ fromIntegral val
  decodeWire _ = empty
  encodeWire t val = putField t 0 >> putVarSInt val

instance Wire Int64 where
  decodeWire (VarintField  _ val) = pure $ fromIntegral val
  decodeWire _ = empty
  encodeWire t val = putField t 0 >> putVarSInt val

instance Wire Word32 where
  decodeWire (VarintField  _ val) = pure $ fromIntegral val
  decodeWire _ = empty
  encodeWire t val = putField t 0 >> putVarUInt val

instance Wire Word64 where
  decodeWire (VarintField  _ val) = pure val
  decodeWire _ = empty
  encodeWire t val = putField t 0 >> putVarUInt val

instance Wire (Signed Int32) where
  decodeWire (VarintField  _ val) = pure $ Signed (zzDecode32 (fromIntegral val))
  decodeWire _ = empty
  encodeWire t (Signed val) = putField t 0 >> (putVarSInt $ zzEncode32 val)

instance Wire (Signed Int64) where
  decodeWire (VarintField  _ val) = pure $ Signed (zzDecode64 (fromIntegral val))
  decodeWire _ = empty
  encodeWire t (Signed val) = putField t 0 >> (putVarSInt $ zzEncode64 val)

instance Wire (Fixed Int32) where
  decodeWire (Fixed32Field _ val) = pure $ Fixed (fromIntegral val)
  decodeWire _ = empty
  encodeWire t (Fixed val) = putField t 5 >> (putWord32le $ fromIntegral val)

instance Wire (Fixed Int64) where
  decodeWire (Fixed64Field _ val) = pure $ Fixed (fromIntegral val)
  decodeWire _ = empty
  encodeWire t (Fixed val) = putField t 1 >> (putWord64le $ fromIntegral val)

instance Wire (Fixed Word32) where
  decodeWire (Fixed32Field _ val) = pure $ Fixed val
  decodeWire _ = empty
  encodeWire t (Fixed val) = putField t 5 >> putWord32le val

instance Wire (Fixed Word64) where
  decodeWire (Fixed64Field _ val) = pure $ Fixed val
  decodeWire _ = empty
  encodeWire t (Fixed val) = putField t 1 >> putWord64le val

instance Wire Bool where
  decodeWire (VarintField _ val) = pure $ val /= 0
  decodeWire _ = empty
  encodeWire t val = putField t 0 >> putVarUInt (if val == False then (0 :: Int32) else 1)

instance Wire Float where
  decodeWire (Fixed32Field _ val) = pure $ wordToFloat val
  decodeWire _ = empty
  encodeWire t val = putField t 5 >> putFloat32le val

instance Wire Double where
  decodeWire (Fixed64Field _ val) = pure $ wordToDouble val
  decodeWire _ = empty
  encodeWire t val = putField t 1 >> putFloat64le val

instance Wire ByteString where
  decodeWire (DelimitedField _ bs) = pure bs
  decodeWire _ = empty
  encodeWire t val = putField t 2 >> putVarUInt (B.length val) >> putByteString val

instance Wire T.Text where
  decodeWire (DelimitedField _ bs) =
    case T.decodeUtf8' bs of
      Right val -> pure val
      Left _err -> empty -- fail $ "Decoding failed: " ++ show err
  decodeWire _ = empty
  encodeWire t = encodeWire t . T.encodeUtf8

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
  getValue (Tagged a) = a
  putValue = Tagged

instance GetValue (Repeated n a) where
  type GetValueType (Tagged n [a]) = [a]
  getValue (Tagged a) = a
  putValue = Tagged

instance GetValue (Required n a) where
  type GetValueType (Tagged n (Identity a)) = a
  getValue (Tagged (Identity a)) = a
  putValue = Tagged . Identity

-- field rules
type Optional (n :: *) a = Tagged n (Maybe a)
type Required (n :: *) a = Tagged n (Identity a)
type Repeated (n :: *) a = Tagged n [a]

instance Show a => Show (Required n a) where
  show (Tagged (Identity x)) = show (Tagged x :: Tagged n a)

newtype Enumeration (a :: *) = Enumeration Int deriving (Eq, NFData, Ord, Show)

instance Wire (Enumeration a) where
  decodeWire f = Enumeration . c <$> decodeWire f where
    c :: Int32 -> Int
    c = fromIntegral
  encodeWire t (Enumeration a) = encodeWire t . c $ fromEnum a where
    c :: Int -> Int32
    c = fromIntegral

instance Monoid (Enumeration a) where
  -- error case is handled by getEnum but we're exposing the instance :-(
  -- really should be a Semigroup instance... if we want a semigroup dependency
  mempty = Enumeration (error "Empty Enumeration")
  _ `mappend` x = x

class GetEnum a where
  type GetEnumResult a :: *
  getEnum :: a -> GetEnumResult a

instance Enum a => GetEnum (Optional n (Enumeration a)) where
  type GetEnumResult (Tagged n (Maybe (Enumeration a))) = Maybe a
  getEnum (Tagged (Just (Enumeration x))) = Just $ toEnum x
  getEnum (Tagged Nothing) = Nothing

instance Enum a => GetEnum (Required n (Enumeration a)) where
  type GetEnumResult (Tagged n (Identity (Enumeration a))) = a
  getEnum (Tagged (Identity (Enumeration x))) = toEnum x

instance Enum a => GetEnum (Repeated n (Enumeration a)) where
  type GetEnumResult (Tagged n [Enumeration a]) = [a]
  getEnum (Tagged xs) = fmap f xs where
    f (Enumeration x) = toEnum x

-- Integer encoding annotations
newtype Signed a = Signed a
  deriving (Bits, Bounded, Enum, Eq, Floating, Foldable, Fractional, Functor, Integral, Monoid, NFData, Num, Ord, Real, RealFloat, RealFrac, Show, Traversable)

newtype Fixed a = Fixed a
  deriving (Bits, Bounded, Enum, Eq, Floating, Foldable, Fractional, Functor, Integral, Monoid, NFData, Num, Ord, Real, RealFloat, RealFrac, Show, Traversable)


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

-- Taken from google's code, but I had to explcitly add fromIntegral in the right places:
zzEncode32 :: Int32 -> Word32
zzEncode32 x = fromIntegral ((x `shiftL` 1) `xor` (x `shiftR` 31))
zzEncode64 :: Int64 -> Word64
zzEncode64 x = fromIntegral ((x `shiftL` 1) `xor` (x `shiftR` 63))
zzDecode32 :: Word32 -> Int32
zzDecode32 w = (fromIntegral (w `shiftR` 1)) `xor` (negate (fromIntegral (w .&. 1)))
zzDecode64 :: Word64 -> Int64
zzDecode64 w = (fromIntegral (w `shiftR` 1)) `xor` (negate (fromIntegral (w .&. 1)))

decodeLengthPrefixed :: Decode a => Get a
decodeLengthPrefixed = do
  len <- getWord32le
  isolate (fromIntegral len) decodeMessage
