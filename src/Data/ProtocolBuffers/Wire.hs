{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}

module Data.ProtocolBuffers.Wire
  ( Enumeration(..)
  , Field(..)
  , Fixed(..)
  , GetEnum(..)
  , Signed(..)
  , Tag
  , Wire(..)
  , fieldTag
  , getField
  , getVarInt
  , getVarintPrefixedBS
  ) where

import Control.Applicative
import Control.DeepSeq (NFData)
import Data.Bits
import Data.ByteString (ByteString)
import qualified Data.ByteString as B
import Data.Foldable
import Data.Int
import Data.Monoid
import Data.Serialize.Get
import Data.Serialize.IEEE754
import Data.Serialize.Put
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import Data.Traversable
import Data.Word
import Data.Binary.IEEE754 (wordToDouble, wordToFloat)

import Data.ProtocolBuffers.Types

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

putField :: Field -> Put
putField (VarintField    t val) = putWireTag t 0 >> putVarUInt val
putField (Fixed64Field   t val) = putWireTag t 1 >> putVarUInt val
putField (DelimitedField t val) = putWireTag t 2 >> putVarUInt (B.length val) >> putByteString val
putField (StartField     t    ) = putWireTag t 3
putField (EndField       t    ) = putWireTag t 4
putField (Fixed32Field   t val) = putWireTag t 5 >> putVarUInt val

putWireTag :: Tag -> Word32 -> Put
putWireTag tag typ = putVarUInt $ tag `shiftL` 3 .|. (typ .&. 7)

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

class Wire a where
  decodeWire :: Alternative m => Field -> m a
  encodeWire :: Tag -> a -> Put

deriving instance Wire a => Wire (First a)
deriving instance Wire a => Wire (Last a)
deriving instance Wire a => Wire (Product a)
deriving instance Wire a => Wire (Sum a)

instance Wire Field where
  decodeWire = pure
  encodeWire _ = putField

instance Wire a => Wire (Maybe a) where
  decodeWire = fmap Just . decodeWire
  encodeWire t (Just a) = encodeWire t a
  encodeWire _ Nothing  = return ()

instance Wire Int32 where
  decodeWire (VarintField  _ val) = pure $ fromIntegral val
  decodeWire _ = empty
  encodeWire t val = putWireTag t 0 >> putVarSInt val

instance Wire Int64 where
  decodeWire (VarintField  _ val) = pure $ fromIntegral val
  decodeWire _ = empty
  encodeWire t val = putWireTag t 0 >> putVarSInt val

instance Wire Word32 where
  decodeWire (VarintField  _ val) = pure $ fromIntegral val
  decodeWire _ = empty
  encodeWire t val = putWireTag t 0 >> putVarUInt val

instance Wire Word64 where
  decodeWire (VarintField  _ val) = pure val
  decodeWire _ = empty
  encodeWire t val = putWireTag t 0 >> putVarUInt val

instance Wire (Signed Int32) where
  decodeWire (VarintField  _ val) = pure $ Signed (zzDecode32 (fromIntegral val))
  decodeWire _ = empty
  encodeWire t (Signed val) = putWireTag t 0 >> (putVarSInt $ zzEncode32 val)

instance Wire (Signed Int64) where
  decodeWire (VarintField  _ val) = pure $ Signed (zzDecode64 (fromIntegral val))
  decodeWire _ = empty
  encodeWire t (Signed val) = putWireTag t 0 >> (putVarSInt $ zzEncode64 val)

instance Wire (Fixed Int32) where
  decodeWire (Fixed32Field _ val) = pure $ Fixed (fromIntegral val)
  decodeWire _ = empty
  encodeWire t (Fixed val) = putWireTag t 5 >> (putWord32le $ fromIntegral val)

instance Wire (Fixed Int64) where
  decodeWire (Fixed64Field _ val) = pure $ Fixed (fromIntegral val)
  decodeWire _ = empty
  encodeWire t (Fixed val) = putWireTag t 1 >> (putWord64le $ fromIntegral val)

instance Wire (Fixed Word32) where
  decodeWire (Fixed32Field _ val) = pure $ Fixed val
  decodeWire _ = empty
  encodeWire t (Fixed val) = putWireTag t 5 >> putWord32le val

instance Wire (Fixed Word64) where
  decodeWire (Fixed64Field _ val) = pure $ Fixed val
  decodeWire _ = empty
  encodeWire t (Fixed val) = putWireTag t 1 >> putWord64le val

instance Wire Bool where
  decodeWire (VarintField _ val) = pure $ val /= 0
  decodeWire _ = empty
  encodeWire t val = putWireTag t 0 >> putVarUInt (if val == False then (0 :: Int32) else 1)

instance Wire Float where
  decodeWire (Fixed32Field _ val) = pure $ wordToFloat val
  decodeWire _ = empty
  encodeWire t val = putWireTag t 5 >> putFloat32le val

instance Wire Double where
  decodeWire (Fixed64Field _ val) = pure $ wordToDouble val
  decodeWire _ = empty
  encodeWire t val = putWireTag t 1 >> putFloat64le val

instance Wire ByteString where
  decodeWire (DelimitedField _ bs) = pure bs
  decodeWire _ = empty
  encodeWire t val = putWireTag t 2 >> putVarUInt (B.length val) >> putByteString val

instance Wire T.Text where
  decodeWire (DelimitedField _ bs) =
    case T.decodeUtf8' bs of
      Right val -> pure val
      Left _err -> empty -- fail $ "Decoding failed: " ++ show err
  decodeWire _ = empty
  encodeWire t = encodeWire t . T.encodeUtf8

instance Wire (Enumeration a) where
  decodeWire f = Enumeration . c <$> decodeWire f where
    c :: Int32 -> Int
    c = fromIntegral
  encodeWire t (Enumeration a) = encodeWire t . c $ fromEnum a where
    c :: Int -> Int32
    c = fromIntegral

-- Integer encoding annotations
newtype Signed a = Signed a
  deriving (Bits, Bounded, Enum, Eq, Floating, Foldable, Fractional, Functor, Integral, Monoid, NFData, Num, Ord, Real, RealFloat, RealFrac, Show, Traversable)

newtype Fixed a = Fixed a
  deriving (Bits, Bounded, Enum, Eq, Floating, Foldable, Fractional, Functor, Integral, Monoid, NFData, Num, Ord, Real, RealFloat, RealFrac, Show, Traversable)

-- Taken from google's code, but I had to explcitly add fromIntegral in the right places:
zzEncode32 :: Int32 -> Word32
zzEncode32 x = fromIntegral ((x `shiftL` 1) `xor` (x `shiftR` 31))
zzEncode64 :: Int64 -> Word64
zzEncode64 x = fromIntegral ((x `shiftL` 1) `xor` (x `shiftR` 63))
zzDecode32 :: Word32 -> Int32
zzDecode32 w = (fromIntegral (w `shiftR` 1)) `xor` (negate (fromIntegral (w .&. 1)))
zzDecode64 :: Word64 -> Int64
zzDecode64 w = (fromIntegral (w `shiftR` 1)) `xor` (negate (fromIntegral (w .&. 1)))
