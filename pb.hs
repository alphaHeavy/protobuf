{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeOperators #-}

module Pb where

import Control.Applicative
import Data.Bits
import Data.ByteString
import Data.HashMap.Strict as HashMap
import Data.Hex
import Data.Int
import Data.Monoid
import Data.Serialize.Get
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.TypeLevel as Tl
import Data.Word
import Unsafe.Coerce

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

getVarInt :: (Integral a, Bits a) => Get a
getVarInt = go 0 0 where
  go n !val = do
    b <- getWord8
    if testBit b 7
      then go (n+7) (val .|. ((fromIntegral (b .&. 0x7F)) `shiftL` n))
      else return $! val .|. ((fromIntegral b) `shiftL` n)

fieldTag :: Field -> Tag
fieldTag f = case f of
  VarintField    t _ -> t
  Fixed64Field   t _ -> t
  DelimitedField t _ -> t
  StartField     t   -> t
  EndField       t   -> t
  Fixed32Field   t _ -> t

decodeMessage :: (Decode (Rep a), Generic a) => Get a
decodeMessage = fmap (to . decode) $ go HashMap.empty where
  go msg = do
    mfield <- Just `fmap` getField <|> return Nothing
    case mfield of
      Just v  -> go $! HashMap.insertWith (++) (fieldTag v) [v] msg
      Nothing -> return msg

class Wire a where
  decodeWire :: Field -> a
  -- encodeWire :: Tag -> a -> Put
  -- sizeWire   :: Tag -> a -> Int

deriving instance Wire a => Wire (First a)
deriving instance Wire a => Wire (Last a)
deriving instance Wire a => Wire (Product a)
deriving instance Wire a => Wire (Sum a)

instance Wire a => Wire (Maybe a) where
  decodeWire = Just . decodeWire

instance Wire Int32 where
  decodeWire (VarintField  _ val) = fromIntegral val

instance Wire Int64 where
  decodeWire (VarintField  _ val) = fromIntegral val

instance Wire Word32 where
  decodeWire (VarintField  _ val) = fromIntegral val

instance Wire Word64 where
  decodeWire (VarintField  _ val) = val

instance Wire (Signed Int32) where
  decodeWire (VarintField  _ val) = Signed (zzDecode32 (fromIntegral val))

instance Wire (Signed Int64) where
  decodeWire (VarintField  _ val) = Signed (zzDecode64 (fromIntegral val))

instance Wire (Fixed Int32) where
  decodeWire (Fixed32Field _ val) = Fixed (fromIntegral val)

instance Wire (Fixed Int64) where
  decodeWire (Fixed64Field _ val) = Fixed (fromIntegral val)

instance Wire (Fixed Word32) where
  decodeWire (Fixed32Field _ val) = Fixed val

instance Wire (Fixed Word64) where
  decodeWire (Fixed64Field _ val) = Fixed val

instance Wire Bool where
  decodeWire (VarintField _ val) = val /= 0

instance Wire Float where
  decodeWire (Fixed32Field _ val) = unsafeCoerce val

instance Wire Double where
  decodeWire (Fixed64Field _ val) = unsafeCoerce val

instance Wire ByteString where
  decodeWire (DelimitedField _ bs) = bs

instance Wire T.Text where
  decodeWire (DelimitedField _ bs) = T.decodeUtf8 bs

-- field rules
newtype Required n a = Required a

deriving instance Bits a => Bits (Required n a)
deriving instance Bounded a => Bounded (Required n a)
deriving instance Enum a => Enum (Required n a)
deriving instance Eq a => Eq (Required n a)
deriving instance Floating a => Floating (Required n a)
deriving instance Fractional a => Fractional (Required n a)
deriving instance Integral a => Integral (Required n a)
deriving instance Monoid a => Monoid (Required n a)
deriving instance Num a => Num (Required n a)
deriving instance Ord a => Ord (Required n a)
deriving instance Real a => Real (Required n a)
deriving instance RealFloat a => RealFloat (Required n a)
deriving instance RealFrac a => RealFrac (Required n a)
deriving instance Show a => Show (Required n a)

newtype Optional n a = Optional a

deriving instance Bits a => Bits (Optional n a)
deriving instance Bounded a => Bounded (Optional n a)
deriving instance Enum a => Enum (Optional n a)
deriving instance Eq a => Eq (Optional n a)
deriving instance Floating a => Floating (Optional n a)
deriving instance Fractional a => Fractional (Optional n a)
deriving instance Integral a => Integral (Optional n a)
deriving instance Monoid a => Monoid (Optional n a)
deriving instance Num a => Num (Optional n a)
deriving instance Ord a => Ord (Optional n a)
deriving instance Real a => Real (Optional n a)
deriving instance RealFloat a => RealFloat (Optional n a)
deriving instance RealFrac a => RealFrac (Optional n a)
deriving instance Show a => Show (Optional n a)

newtype Packed n a = Packed a

deriving instance Bits a => Bits (Packed n a)
deriving instance Bounded a => Bounded (Packed n a)
deriving instance Enum a => Enum (Packed n a)
deriving instance Eq a => Eq (Packed n a)
deriving instance Floating a => Floating (Packed n a)
deriving instance Fractional a => Fractional (Packed n a)
deriving instance Integral a => Integral (Packed n a)
deriving instance Monoid a => Monoid (Packed n a)
deriving instance Num a => Num (Packed n a)
deriving instance Ord a => Ord (Packed n a)
deriving instance Real a => Real (Packed n a)
deriving instance RealFloat a => RealFloat (Packed n a)
deriving instance RealFrac a => RealFrac (Packed n a)
deriving instance Show a => Show (Packed n a)

-- Integer encoding annotations
newtype Signed a = Signed a

deriving instance Bits a => Bits (Signed a)
deriving instance Bounded a => Bounded (Signed a)
deriving instance Enum a => Enum (Signed a)
deriving instance Eq a => Eq (Signed a)
deriving instance Floating a => Floating (Signed a)
deriving instance Fractional a => Fractional (Signed a)
deriving instance Integral a => Integral (Signed a)
deriving instance Monoid a => Monoid (Signed a)
deriving instance Num a => Num (Signed a)
deriving instance Ord a => Ord (Signed a)
deriving instance Real a => Real (Signed a)
deriving instance RealFloat a => RealFloat (Signed a)
deriving instance RealFrac a => RealFrac (Signed a)
deriving instance Show a => Show (Signed a)

newtype Fixed a = Fixed a

deriving instance Bits a => Bits (Fixed a)
deriving instance Bounded a => Bounded (Fixed a)
deriving instance Enum a => Enum (Fixed a)
deriving instance Eq a => Eq (Fixed a)
deriving instance Floating a => Floating (Fixed a)
deriving instance Fractional a => Fractional (Fixed a)
deriving instance Integral a => Integral (Fixed a)
deriving instance Monoid a => Monoid (Fixed a)
deriving instance Num a => Num (Fixed a)
deriving instance Ord a => Ord (Fixed a)
deriving instance Real a => Real (Fixed a)
deriving instance RealFloat a => RealFloat (Fixed a)
deriving instance RealFrac a => RealFrac (Fixed a)
deriving instance Show a => Show (Fixed a)

class Decode (f :: * -> *) where
  decode :: HashMap Tag [Field] -> f a

instance Decode a => Decode (M1 i c a) where
  decode = M1 . decode

instance (Wire a, Monoid a, Tl.Nat n) => Decode (K1 i (Optional n a)) where
  decode = K1 . Optional . mconcat . fmap decodeWire . HashMap.lookupDefault [] (fromIntegral (Tl.toInt (undefined :: n)))

instance (Wire a, Monoid a, Tl.Nat n) => Decode (K1 i (Required n a)) where
  decode = K1 . Required . mconcat . fmap decodeWire . HashMap.lookupDefault (error "required field not found") (fromIntegral (Tl.toInt (undefined :: n)))

{-
instance (Wire a, Monoid a, Tl.Nat n) => Decode (K1 i (Packed n a)) where
  decode = error "packed fields are not implemented"
-}

instance (Decode a, Decode b) => Decode (a :*: b) where
  decode msg = decode msg :*: decode msg

-- Taken from google's code, but I had to explcitly add fromIntegral in the right places:
zzEncode32 :: Int32 -> Word32
zzEncode32 x = fromIntegral ((x `shiftL` 1) `xor` (x `shiftR` 31))
zzEncode64 :: Int64 -> Word64
zzEncode64 x = fromIntegral ((x `shiftL` 1) `xor` (x `shiftR` 63))
zzDecode32 :: Word32 -> Int32
zzDecode32 w = (fromIntegral (w `shiftR` 1)) `xor` (negate (fromIntegral (w .&. 1)))
zzDecode64 :: Word64 -> Int64
zzDecode64 w = (fromIntegral (w `shiftR` 1)) `xor` (negate (fromIntegral (w .&. 1)))

data TestRec = TestRec
  { field1 :: Required Tl.D1 (Last Int64)
  , field2 :: Optional Tl.D2 (Last T.Text)
  , field3 :: Optional Tl.D3 (Last Int64)
  -- , field3 :: Optional Tl.D3 (Signed Int)
  -- , field4 :: Optional Tl.D4 Double
  } deriving (Generic, Show)

main :: IO ()
main = do
  let val1, val2, val3 :: TestRec
      Right val1 = runGet decodeMessage =<< unhex "089601120774657374696e67"
      Right val2 = runGet decodeMessage =<< unhex "089601189701"
      Right val3 = runGet decodeMessage =<< unhex "089601"
  print val1
  print val2
  print val3