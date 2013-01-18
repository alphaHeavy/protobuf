{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}

module Data.ProtocolBuffers.Wire
  ( Enumeration(..)
  , Field(..)
  , GetEnum(..)
  , Tag
  , EncodeWire(..)
  , DecodeWire(..)
  , fieldTag
  , getField
  , getVarInt
  , getVarintPrefixedBS
  , putVarSInt
  , putVarUInt
  ) where

import Control.Applicative
import Control.DeepSeq (NFData)
import Control.Monad.Identity
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

-- |
-- Field identifiers
type Tag = Word32

-- |
-- A representation of the wire format as described in
-- <https://developers.google.com/protocol-buffers/docs/encoding#structure>
data Field
  = VarintField    {-# UNPACK #-} !Tag {-# UNPACK #-} !Word64 -- ^ For: int32, int64, uint32, uint64, sint32, sint64, bool, enum
  | Fixed64Field   {-# UNPACK #-} !Tag {-# UNPACK #-} !Word64 -- ^ For: fixed64, sfixed64, double
  | DelimitedField {-# UNPACK #-} !Tag !ByteString -- ^ For: string, bytes, embedded messages, packed repeated fields
  | StartField     {-# UNPACK #-} !Tag -- ^ For: groups (deprecated)
  | EndField       {-# UNPACK #-} !Tag -- ^ For: groups (deprecated)
  | Fixed32Field   {-# UNPACK #-} !Tag {-# UNPACK #-} !Word32 -- ^ For: fixed32, sfixed32, float
    deriving (Eq, Ord, Show)

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
putWireTag tag typ
  | tag <= 0x1FFFFFFF, typ <= 7 = putVarUInt $ tag `shiftL` 3 .|. (typ .&. 7)
  | tag  > 0x1FFFFFFF = fail $ "Wire tag out of range: "  ++ show tag
  | otherwise         = fail $ "Wire type out of range: " ++ show typ

getVarInt :: (Integral a, Bits a) => Get a
getVarInt = go 0 0 where
  go n !val = do
    b <- getWord8
    if testBit b 7
      then go (n+7) (val .|. (fromIntegral (b .&. 0x7F) `shiftL` n))
      else return $! val .|. (fromIntegral b `shiftL` n)

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

class EncodeWire a where
  encodeWire :: Tag -> a -> Put

class DecodeWire a where
  decodeWire :: Field -> Get a

deriving instance EncodeWire a => EncodeWire (First a)
deriving instance EncodeWire a => EncodeWire (Last a)
deriving instance EncodeWire a => EncodeWire (Product a)
deriving instance EncodeWire a => EncodeWire (Sum a)
deriving instance EncodeWire a => EncodeWire (Optionally a)

deriving instance DecodeWire a => DecodeWire (First a)
deriving instance DecodeWire a => DecodeWire (Last a)
deriving instance DecodeWire a => DecodeWire (Product a)
deriving instance DecodeWire a => DecodeWire (Sum a)
deriving instance DecodeWire a => DecodeWire (Optionally a)

instance EncodeWire Field where
  encodeWire _ = putField

instance DecodeWire Field where
  decodeWire = pure

instance EncodeWire a => EncodeWire (Identity a) where
  encodeWire t = traverse_ (encodeWire t)

instance DecodeWire a => DecodeWire (Identity a) where
  decodeWire = fmap Identity . decodeWire

instance EncodeWire a => EncodeWire (Maybe a) where
  encodeWire t = traverse_ (encodeWire t)

instance DecodeWire a => DecodeWire (Maybe a) where
  decodeWire = fmap Just . decodeWire

instance EncodeWire Int32 where
  encodeWire t val = putWireTag t 0 >> putVarSInt val

instance DecodeWire Int32 where
  decodeWire (VarintField  _ val) = pure $ fromIntegral val
  decodeWire _ = empty

instance EncodeWire Int64 where
  encodeWire t val = putWireTag t 0 >> putVarSInt val

instance DecodeWire Int64 where
  decodeWire (VarintField  _ val) = pure $ fromIntegral val
  decodeWire _ = empty

instance EncodeWire Word32 where
  encodeWire t val = putWireTag t 0 >> putVarUInt val

instance DecodeWire Word32 where
  decodeWire (VarintField  _ val) = pure $ fromIntegral val
  decodeWire _ = empty

instance EncodeWire Word64 where
  encodeWire t val = putWireTag t 0 >> putVarUInt val

instance DecodeWire Word64 where
  decodeWire (VarintField  _ val) = pure val
  decodeWire _ = empty

instance EncodeWire (Signed Int32) where
  encodeWire t (Signed val) = putWireTag t 0 >> putVarSInt (zzEncode32 val)

instance DecodeWire (Signed Int32) where
  decodeWire (VarintField  _ val) = pure . Signed . zzDecode32 $ fromIntegral val
  decodeWire _ = empty

instance EncodeWire (Signed Int64) where
  encodeWire t (Signed val) = putWireTag t 0 >> putVarSInt (zzEncode64 val)

instance DecodeWire (Signed Int64) where
  decodeWire (VarintField  _ val) = pure . Signed . zzDecode64 $ fromIntegral val
  decodeWire _ = empty

instance EncodeWire (Fixed Int32) where
  encodeWire t (Fixed val) = putWireTag t 5 >> putWord32le (fromIntegral val)

instance DecodeWire (Fixed Int32) where
  decodeWire (Fixed32Field _ val) = pure . Fixed $ fromIntegral val
  decodeWire _ = empty

instance EncodeWire (Fixed Int64) where
  encodeWire t (Fixed val) = putWireTag t 1 >> putWord64le (fromIntegral val)

instance DecodeWire (Fixed Int64) where
  decodeWire (Fixed64Field _ val) = pure . Fixed $ fromIntegral val
  decodeWire _ = empty

instance EncodeWire (Fixed Word32) where
  encodeWire t (Fixed val) = putWireTag t 5 >> putWord32le val

instance DecodeWire (Fixed Word32) where
  decodeWire (Fixed32Field _ val) = pure $ Fixed val
  decodeWire _ = empty

instance EncodeWire (Fixed Word64) where
  encodeWire t (Fixed val) = putWireTag t 1 >> putWord64le val

instance DecodeWire (Fixed Word64) where
  decodeWire (Fixed64Field _ val) = pure $ Fixed val
  decodeWire _ = empty

instance EncodeWire Bool where
  encodeWire t val = putWireTag t 0 >> putVarUInt (if val then 1 else (0 :: Int32))

instance DecodeWire Bool where
  decodeWire (VarintField _ val) = pure $ val /= 0
  decodeWire _ = empty

instance EncodeWire Float where
  encodeWire t val = putWireTag t 5 >> putFloat32le val

instance DecodeWire Float where
  decodeWire (Fixed32Field _ val) = pure $ wordToFloat val
  decodeWire _ = empty

instance EncodeWire Double where
  encodeWire t val = putWireTag t 1 >> putFloat64le val

instance DecodeWire Double where
  decodeWire (Fixed64Field _ val) = pure $ wordToDouble val
  decodeWire _ = empty

instance EncodeWire ByteString where
  encodeWire t val = putWireTag t 2 >> putVarUInt (B.length val) >> putByteString val

instance DecodeWire ByteString where
  decodeWire (DelimitedField _ bs) = pure bs
  decodeWire _ = empty

instance EncodeWire T.Text where
  encodeWire t = encodeWire t . T.encodeUtf8

instance DecodeWire T.Text where
  decodeWire (DelimitedField _ bs) =
    case T.decodeUtf8' bs of
      Right val -> pure val
      Left err  -> fail $ "Decoding failed: " ++ show err
  decodeWire _ = empty

instance EncodeWire (PackedList Int32) where
  encodeWire t (PackedList xs) = do
    encodeWire t . runPut $ traverse_ putVarSInt xs

instance DecodeWire (PackedList Int32) where
  decodeWire (DelimitedField _ bs) =
    case runGet (some getVarInt) bs of
      Right val -> return $ PackedList val
      Left err  -> fail err
  decodeWire _ = empty

instance EncodeWire (PackedList Int64) where
  encodeWire t (PackedList xs) = do
    encodeWire t . runPut $ traverse_ putVarSInt xs

instance DecodeWire (PackedList Int64) where
  decodeWire (DelimitedField _ bs) =
    case runGet (some getVarInt) bs of
      Right val -> return $ PackedList val
      Left err  -> fail err
  decodeWire _ = empty

instance EncodeWire (PackedList Word32) where
  encodeWire t (PackedList xs) = do
    encodeWire t . runPut $ traverse_ putVarUInt xs

instance DecodeWire (PackedList Word32) where
  decodeWire (DelimitedField _ bs) =
    case runGet (some getVarInt) bs of
      Right val -> return $ PackedList val
      Left err  -> fail err
  decodeWire _ = empty

instance EncodeWire (PackedList Word64) where
  encodeWire t (PackedList xs) = do
    encodeWire t . runPut $ traverse_ putVarUInt xs

instance DecodeWire (PackedList Word64) where
  decodeWire (DelimitedField _ bs) =
    case runGet (some getVarInt) bs of
      Right val -> return $ PackedList val
      Left err  -> fail err
  decodeWire _ = empty

instance EncodeWire (PackedList (Signed Int32)) where
  encodeWire t (PackedList xs) = do
    let c (Signed x) = putVarSInt $ zzEncode32 x
    encodeWire t . runPut $ traverse_ c xs

instance DecodeWire (PackedList (Signed Int32)) where
  decodeWire (DelimitedField _ bs) =
    case runGet (some getVarInt) bs of
      Right val -> return . PackedList $ fmap (Signed . zzDecode32) val
      Left err  -> fail err
  decodeWire _ = empty

instance EncodeWire (PackedList (Signed Int64)) where
  encodeWire t (PackedList xs) = do
    let c (Signed x) = putVarSInt $ zzEncode64 x
    encodeWire t . runPut $ traverse_ c xs

instance DecodeWire (PackedList (Signed Int64)) where
  decodeWire (DelimitedField _ bs) =
    case runGet (some getVarInt) bs of
      Right val -> return . PackedList $ fmap (Signed . zzDecode64) val
      Left err  -> fail err
  decodeWire _ = empty

instance (Foldable f, Enum a) => EncodeWire (Enumeration (f a)) where
  encodeWire t = traverse_ (encodeWire t . c) . getEnum where
    c :: a -> Int32
    c = fromIntegral . fromEnum

instance Enum a => DecodeWire (Enumeration (Identity a)) where
  decodeWire f = c <$> decodeWire f where
    c :: Int32 -> Enumeration (Identity a)
    c = putEnum . Identity . toEnum . fromIntegral

instance Enum a => DecodeWire (Enumeration (Maybe a)) where
  decodeWire f = c <$> decodeWire f where
    c :: Int32 -> Enumeration (Maybe a)
    c = putEnum . Just . toEnum . fromIntegral

-- Taken from google's code, but I had to explcitly add fromIntegral in the right places:
zzEncode32 :: Int32 -> Word32
zzEncode32 x = fromIntegral ((x `shiftL` 1) `xor` x `shiftR` 31)
zzEncode64 :: Int64 -> Word64
zzEncode64 x = fromIntegral ((x `shiftL` 1) `xor` x `shiftR` 63)
zzDecode32 :: Word32 -> Int32
zzDecode32 w = fromIntegral (w `shiftR` 1) `xor` negate (fromIntegral (w .&. 1))
zzDecode64 :: Word64 -> Int64
zzDecode64 w = fromIntegral (w `shiftR` 1) `xor` negate (fromIntegral (w .&. 1))
