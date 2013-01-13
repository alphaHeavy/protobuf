{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE OverlappingInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module Data.ProtocolBuffers
  ( Encode(..)
  , Decode(..)
  , Wire(..)
  , Value
  , Required
  , Optional
  , Repeated
  , GetValue(..)
  , Enumeration
  , GetEnum(..)
  , Signed(..)
  , Fixed(..)
  ) where

import Control.Applicative
import Control.Monad
import Control.Monad.Identity
import Data.Bits
import Data.ByteString
import qualified Data.ByteString as B
import Data.Foldable
import Data.HashMap.Strict as HashMap
import Data.Int
import Data.Monoid
import Data.Serialize.Get
import Data.Serialize.IEEE754
import Data.Serialize.Put
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import Data.Traversable
import qualified Data.TypeLevel as Tl
import Data.Word
import Data.Binary.IEEE754 (wordToDouble, wordToFloat)

import GHC.Generics

type Tag = Word32

data Field where
  VarintField    :: Tag -> Word64 -> Field
  Fixed64Field   :: Tag -> Word64 -> Field
  DelimitedField :: Tag -> ByteString -> Field
  StartField     :: Tag -> Field
  EndField       :: Tag -> Field
  Fixed32Field   :: Tag -> Word32 -> Field

  deriving Show

getVarintPrefixedBS :: Get ByteString
getVarintPrefixedBS = do
  length <- getVarInt
  getBytes length

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

decodeMessage :: (GDecode (Rep a), Generic a) => Get a
decodeMessage = fmap to (gdecode =<< go HashMap.empty) where
  go msg = do
    mfield <- Just `fmap` getField <|> return Nothing
    case mfield of
      Just v  -> go $! HashMap.insertWith (flip (++)) (fieldTag v) [v] msg
      Nothing -> return msg

--encodeMessage :: (GEncode (Rep a), Generic a) => a -> Put
--encodeMessage = gencode . from

class Wire a where
  decodeWire :: Field -> a
  encodeWire :: Tag -> a -> Put
  sizeWire   :: a -> Int

deriving instance Wire a => Wire (First a)
deriving instance Wire a => Wire (Last a)
deriving instance Wire a => Wire (Product a)
deriving instance Wire a => Wire (Sum a)

instance Wire a => Wire (Maybe a) where
  decodeWire = Just . decodeWire
  encodeWire t (Just a) = encodeWire t a
  encodeWire t Nothing  = return ()

instance Wire Int32 where
  decodeWire (VarintField  _ val) = fromIntegral val
  encodeWire t val = putField t 0 >> putVarSInt val

instance Wire Int64 where
  decodeWire (VarintField  _ val) = fromIntegral val
  encodeWire t val = putField t 0 >> putVarSInt val

instance Wire Word32 where
  decodeWire (VarintField  _ val) = fromIntegral val
  encodeWire t val = putField t 0 >> putVarUInt val

instance Wire Word64 where
  decodeWire (VarintField  _ val) = val
  encodeWire t val = putField t 0 >> putVarUInt val

instance Wire (Signed Int32) where
  decodeWire (VarintField  _ val) = Signed (zzDecode32 (fromIntegral val))
  encodeWire t (Signed val) = putField t 0 >> (putVarSInt $ zzEncode32 val)

instance Wire (Signed Int64) where
  decodeWire (VarintField  _ val) = Signed (zzDecode64 (fromIntegral val))
  encodeWire t (Signed val) = putField t 0 >> (putVarSInt $ zzEncode64 val)

instance Wire (Fixed Int32) where
  decodeWire (Fixed32Field _ val) = Fixed (fromIntegral val)
  encodeWire t (Fixed val) = putField t 5 >> (putWord32le $ fromIntegral val)

instance Wire (Fixed Int64) where
  decodeWire (Fixed64Field _ val) = Fixed (fromIntegral val)
  encodeWire t (Fixed val) = putField t 5 >> (putWord64le $ fromIntegral val)

instance Wire (Fixed Word32) where
  decodeWire (Fixed32Field _ val) = Fixed val
  encodeWire t (Fixed val) = putField t 5 >> putWord32le val

instance Wire (Fixed Word64) where
  decodeWire (Fixed64Field _ val) = Fixed val
  encodeWire t (Fixed val) = putField t 1 >> putWord64le val

instance Wire Bool where
  decodeWire (VarintField _ val) = val /= 0
  encodeWire t val = putField t 0 >> (putVarUInt $ if val == False then (0 :: Int32) else 1)

instance Wire Float where
  decodeWire (Fixed32Field _ val) = wordToFloat val
  encodeWire t val = putField t 5 >> putFloat32le val

instance Wire Double where
  decodeWire (Fixed64Field _ val) = wordToDouble val
  encodeWire t val = putField t 1 >> putFloat64le val

instance Wire ByteString where
  decodeWire (DelimitedField _ bs) = bs
  encodeWire t val = putField t 2 >> (putVarUInt $ B.length val) >> (putByteString val)

instance Wire T.Text where
  decodeWire (DelimitedField _ bs) = T.decodeUtf8 bs
  encodeWire t val = putField t 2 >> (putVarUInt $ T.length val) >> (putByteString $ T.encodeUtf8 val)

instance (GDecode (Rep a), Generic a) => Wire a where
  decodeWire (DelimitedField _ bs) = val where
    Right val = runGet decodeMessage bs

newtype Value n f a = Value (f a)
  deriving (Bits, Bounded, Enum, Eq, Floating, Foldable, Fractional, Functor, Integral, Monoid, Num, Ord, Real, RealFloat, RealFrac, Traversable)

instance (Show a, Tl.Nat n) => Show (Value n Maybe a) where
  show (Value x) = "Optional " ++ show (Tl.toInt (undefined :: n)) ++ " " ++ show x

instance (Show a, Tl.Nat n) => Show (Value n Identity a) where
  show (Value (Identity x)) = "Required " ++ show (Tl.toInt (undefined :: n)) ++ " " ++ show x

instance (Show a, Tl.Nat n) => Show (Value n [] a) where
  show (Value x) = "Repeated " ++ show (Tl.toInt (undefined :: n)) ++ " " ++ show x

class GetValue a where
  type GetValueType a :: *
  getValue :: a -> GetValueType a

instance GetValue (Value n Maybe a) where
  type GetValueType (Value n Maybe a) = Maybe a
  getValue (Value a) = a

instance GetValue (Value n [] a) where
  type GetValueType (Value n [] a) = [a]
  getValue (Value a) = a

instance GetValue (Value n Identity a) where
  type GetValueType (Value n Identity a) = a
  getValue (Value (Identity a)) = a

-- field rules
type Optional n a = Value n Maybe a
type Required n a = Value n Identity a
type Repeated n a = Value n [] a

newtype Enumeration a = Enumeration Int deriving (Eq, Ord, Show, Foldable, Functor, Traversable)

instance Monoid (Enumeration a) where
  mempty = Enumeration 0
  _ `mappend` x = x

class GetEnum a where
  type GetEnumResult a :: *
  getEnum :: a -> GetEnumResult a

instance Enum a => GetEnum (Value n Maybe (Enumeration a)) where
  type GetEnumResult (Value n Maybe (Enumeration a)) = Maybe a
  getEnum (Value (Just (Enumeration x))) = Just $ toEnum x
  getEnum (Value Nothing) = Nothing

instance Enum a => GetEnum (Value n Identity (Enumeration a)) where
  type GetEnumResult (Value n Identity (Enumeration a)) = a
  getEnum (Value (Identity (Enumeration x))) = toEnum x

instance Enum a => GetEnum (Value n [] (Enumeration a)) where
  type GetEnumResult (Value n [] (Enumeration a)) = [a]
  getEnum (Value xs) = fmap f xs where
    f (Enumeration x) = toEnum x

-- Integer encoding annotations
newtype Signed a = Signed a
  deriving (Bits, Bounded, Enum, Eq, Floating, Foldable, Fractional, Functor, Integral, Monoid, Num, Ord, Real, RealFloat, RealFrac, Show, Traversable)

newtype Fixed a = Fixed a
  deriving (Bits, Bounded, Enum, Eq, Floating, Foldable, Fractional, Functor, Integral, Monoid, Num, Ord, Real, RealFloat, RealFrac, Show, Traversable)


class GDecode (f :: * -> *) where
  gdecode :: (Applicative m, Monad m, MonadPlus m) => HashMap Tag [Field] -> m (f a)

instance GDecode a => GDecode (M1 i c a) where
  gdecode = fmap M1 . gdecode

class Decode (a :: *) where
  decode :: Get a
  default decode :: (Generic a, GDecode (Rep a)) => Get a
  decode = decodeMessage

-- instance (GDecode x, GDecode y) => GDecode (x :+: y) where
  -- decode msg = do

instance (Wire a, Monoid a, Tl.Nat n) => GDecode (K1 i (Value n Maybe a)) where
  gdecode msg =
    pure $! case HashMap.lookup (fromIntegral (Tl.toInt (undefined :: n))) msg of
      Just val -> K1 . Value $ foldMap decodeWire val
      Nothing  -> K1 mempty

instance (Wire a, Tl.Nat n) => GDecode (K1 i (Value n [] a)) where
  gdecode msg = case HashMap.lookup (fromIntegral (Tl.toInt (undefined :: n))) msg of
    Just val -> pure . K1 . Value . fmap decodeWire $ val
    Nothing  -> pure $ K1 mempty

instance (Wire a, Monoid a, Tl.Nat n) => GDecode (K1 i (Value n Identity a)) where
  gdecode msg =
    case HashMap.lookup (fromIntegral (Tl.toInt (undefined :: n))) msg of
      Just val -> pure . K1 . Value . Identity $ foldMap decodeWire val
      Nothing  -> mzero

{-
instance (Wire a, Monoid a, Tl.Nat n) => GDecode (K1 i (Packed n a)) where
  decode = error "packed fields are not implemented"
-}

instance (GDecode a, GDecode b) => GDecode (a :*: b) where
  gdecode msg = liftA2 (:*:) (gdecode msg) (gdecode msg)

class GEncode (f :: * -> *) where
  gencode :: f a -> Put

class Encode (a :: *) where
  encode :: a -> Put
  default encode :: (Generic a, GEncode (Rep a)) => a -> Put
  encode = gencode . from

instance GEncode a => GEncode (M1 i c a) where
  gencode = gencode . unM1


instance (Wire a, Monoid a, Tl.Nat n) => GEncode (K1 i (Value n Maybe a)) where
  gencode (K1 (Value opt)) = encodeWire (fromIntegral $ Tl.toInt (undefined :: n)) opt

instance (Wire a, Tl.Nat n) => GEncode (K1 i (Value n [] a)) where
  gencode (K1 (Value opt)) = traverse_ (encodeWire (fromIntegral $ Tl.toInt (undefined :: n))) opt

instance (Wire a, Monoid a, Tl.Nat n) => GEncode (K1 i (Value n Identity a)) where
  gencode (K1 (Value (Identity val))) = encodeWire (fromIntegral $ Tl.toInt (undefined :: n)) val

instance (GEncode a, GEncode b) => GEncode (a :*: b) where
  gencode (x :*: y) = gencode x >> gencode y

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
  isolate (fromIntegral len) decode
