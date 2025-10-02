{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}

module Data.ProtocolBuffers.Wire
  ( Enumeration(..)
  , WireField(..)
  , Tag
  , EncodeWire(..)
  , DecodeWire(..)
  , wireFieldTag
  , getWireField
  , getVarInt
  , getVarintPrefixedBS
  , putVarSInt
  , putVarUInt
  , putVarintPrefixedBS
  , zzEncode32
  , zzEncode64
  , zzDecode32
  , zzDecode64
  ) where

import Control.Applicative
import Data.Bits
import Data.ByteString (ByteString)
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as LBS
import Data.Foldable
import Data.Int
import Data.Monoid
import Data.Binary.Get
import GHC.Float (castFloatToWord32, castWord32ToFloat, castDoubleToWord64, castWord64ToDouble)
import Data.Binary.Builder.Sized hiding (empty)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import Data.Typeable
import Data.Word

import Data.ProtocolBuffers.Types

-- |
-- Field identifiers
type Tag = Word32

-- |
-- A representation of the wire format as described in
-- <https://developers.google.com/protocol-buffers/docs/encoding#structure>
data WireField
  = VarintField    {-# UNPACK #-} !Tag {-# UNPACK #-} !Word64 -- ^ For: int32, int64, uint32, uint64, sint32, sint64, bool, enum
  | Fixed64Field   {-# UNPACK #-} !Tag {-# UNPACK #-} !Word64 -- ^ For: fixed64, sfixed64, double
  | DelimitedField {-# UNPACK #-} !Tag !ByteString -- ^ For: string, bytes, embedded messages, packed repeated fields
  | StartField     {-# UNPACK #-} !Tag -- ^ For: groups (deprecated)
  | EndField       {-# UNPACK #-} !Tag -- ^ For: groups (deprecated)
  | Fixed32Field   {-# UNPACK #-} !Tag {-# UNPACK #-} !Word32 -- ^ For: fixed32, sfixed32, float
    deriving (Eq, Ord, Show, Typeable)

putFloat32le :: Float -> Builder
putFloat32le = putWord32le . castFloatToWord32

putFloat64le :: Double -> Builder
putFloat64le = putWord64le . castDoubleToWord64

getFloat32le :: Get Float
getFloat32le = castWord32ToFloat <$> getWord32le

getFloat64le :: Get Double
getFloat64le = castWord64ToDouble <$> getWord64le

getVarintPrefixedBS :: Get ByteString
getVarintPrefixedBS = getByteString =<< getVarInt

putVarintPrefixedBS :: ByteString -> Builder
putVarintPrefixedBS bs = putVarUInt (B.length bs) <> fromByteString bs

getWireField :: Get WireField
getWireField = do
  wireTag <- getVarInt
  let tag = wireTag `shiftR` 3
  case wireTag .&. 7 of
    0 -> VarintField    tag <$> getVarInt
    1 -> Fixed64Field   tag <$> getWord64le
    2 -> DelimitedField tag <$> getVarintPrefixedBS
    3 -> return $! StartField tag
    4 -> return $! EndField   tag
    5 -> Fixed32Field   tag <$> getWord32le
    x -> fail $ "Wire type out of range: " ++ show x

putWireField :: WireField -> Builder
putWireField (VarintField    t val) = putWireTag t 0 <> putVarUInt val
putWireField (Fixed64Field   t val) = putWireTag t 1 <> putWord64le val
putWireField (DelimitedField t val) = putWireTag t 2 <> putVarintPrefixedBS val
putWireField (StartField     t    ) = putWireTag t 3
putWireField (EndField       t    ) = putWireTag t 4
putWireField (Fixed32Field   t val) = putWireTag t 5 <> putWord32le val

putWireTag :: Tag -> Word32 -> Builder
putWireTag tag typ
  | tag <= 0x1FFFFFFF, typ <= 7 = putVarUInt $ tag `shiftL` 3 .|. (typ .&. 7)
  | tag  > 0x1FFFFFFF = error $ "Wire tag out of range: "  ++ show tag
  | otherwise         = error $ "Wire type out of range: " ++ show typ

getVarInt :: (Integral a, Bits a) => Get a
getVarInt = go 0 0 where
  go n !val = do
    b <- getWord8
    if testBit b 7
      then go (n+7) (val .|. (fromIntegral (b .&. 0x7F) `shiftL` n))
      else return $! val .|. (fromIntegral b `shiftL` n)

-- | This can be used on any Integral type and is needed for signed types; unsigned can use putVarUInt below.
-- This has been changed to handle only up to 64 bit integral values (to match documentation).
{-# INLINE putVarSInt #-}
putVarSInt :: (Integral a, Bits a) => a -> Builder
putVarSInt bIn =
  case compare bIn 0 of
    LT -> let -- upcast to 64 bit to match documentation of 10 bytes for all negative values
              b = fromIntegral bIn
              len = 10                                -- (pred 10)*7 < 64 <= 10*7
              last'Mask = 1                           -- pred (1 `shiftL` 1)
              go :: Int64 -> Int -> Builder
              go !i 1 = singleton (fromIntegral (i .&. last'Mask))
              go !i n = singleton (fromIntegral (i .&. 0x7F) .|. 0x80) <> go (i `shiftR` 7) (pred n)
          in go b len
    EQ -> singleton 0
    GT -> putVarUInt bIn

-- | This should be used on unsigned Integral types only (not checked)
{-# INLINE putVarUInt #-}
putVarUInt :: (Integral a, Bits a) => a -> Builder
putVarUInt i
  | i < 0x80  = singleton (fromIntegral i)
  | otherwise = singleton (fromIntegral (i .&. 0x7F) .|. 0x80) <> putVarUInt (i `shiftR` 7)

wireFieldTag :: WireField -> Tag
wireFieldTag f = case f of
  VarintField    t _ -> t
  Fixed64Field   t _ -> t
  DelimitedField t _ -> t
  StartField     t   -> t
  EndField       t   -> t
  Fixed32Field   t _ -> t

class EncodeWire a where
  encodeWire :: Tag -> a -> Builder

class DecodeWire a where
  decodeWire :: WireField -> Get a

deriving instance EncodeWire a => EncodeWire (Always (Value a))
deriving instance EncodeWire a => EncodeWire (Last (Value a))

deriving instance DecodeWire a => DecodeWire (Always (Value a))
deriving instance DecodeWire a => DecodeWire (Last (Value a))

instance EncodeWire a => EncodeWire [Value a] where
  encodeWire t = foldMap (encodeWire t)

instance EncodeWire WireField where
  encodeWire t f
    | t == wireFieldTag f = putWireField f
    | otherwise           = error "Specified tag and field tag do not match"

instance DecodeWire WireField where
  decodeWire = pure

instance EncodeWire a => EncodeWire (Value a) where
  encodeWire t = foldMap (encodeWire t)

instance DecodeWire a => DecodeWire (Value a) where
  decodeWire = fmap Value . decodeWire

instance EncodeWire a => EncodeWire (Maybe (Value a)) where
  encodeWire t = foldMap (encodeWire t)

instance DecodeWire a => DecodeWire (Maybe (Value a)) where
  decodeWire = fmap (Just . Value) . decodeWire

instance EncodeWire Int32 where
  encodeWire t val = putWireTag t 0 <> putVarSInt val

instance DecodeWire Int32 where
  decodeWire (VarintField  _ val) = pure $ fromIntegral val
  decodeWire _ = empty

instance EncodeWire Int64 where
  encodeWire t val = putWireTag t 0 <> putVarSInt val

instance DecodeWire Int64 where
  decodeWire (VarintField  _ val) = pure $ fromIntegral val
  decodeWire _ = empty

instance EncodeWire Word32 where
  encodeWire t val = putWireTag t 0 <> putVarUInt val

instance DecodeWire Word32 where
  decodeWire (VarintField  _ val) = pure $ fromIntegral val
  decodeWire _ = empty

instance EncodeWire Word64 where
  encodeWire t val = putWireTag t 0 <> putVarUInt val

instance DecodeWire Word64 where
  decodeWire (VarintField  _ val) = pure val
  decodeWire _ = empty

instance EncodeWire (Signed Int32) where
  encodeWire t (Signed val) = putWireTag t 0 <> putVarSInt (zzEncode32 val)

instance DecodeWire (Signed Int32) where
  decodeWire (VarintField  _ val) = pure . Signed . zzDecode32 $ fromIntegral val
  decodeWire _ = empty

instance EncodeWire (Signed Int64) where
  encodeWire t (Signed val) = putWireTag t 0 <> putVarSInt (zzEncode64 val)

instance DecodeWire (Signed Int64) where
  decodeWire (VarintField  _ val) = pure . Signed . zzDecode64 $ fromIntegral val
  decodeWire _ = empty

instance EncodeWire (Fixed Int32) where
  encodeWire t (Fixed val) = putWireTag t 5 <> putWord32le (fromIntegral val)

instance DecodeWire (Fixed Int32) where
  decodeWire (Fixed32Field _ val) = pure . Fixed $ fromIntegral val
  decodeWire _ = empty

instance EncodeWire (Fixed Int64) where
  encodeWire t (Fixed val) = putWireTag t 1 <> putWord64le (fromIntegral val)

instance DecodeWire (Fixed Int64) where
  decodeWire (Fixed64Field _ val) = pure . Fixed $ fromIntegral val
  decodeWire _ = empty

instance EncodeWire (Fixed Word32) where
  encodeWire t (Fixed val) = putWireTag t 5 <> putWord32le val

instance DecodeWire (Fixed Word32) where
  decodeWire (Fixed32Field _ val) = pure $ Fixed val
  decodeWire _ = empty

instance EncodeWire (Fixed Word64) where
  encodeWire t (Fixed val) = putWireTag t 1 <> putWord64le val

instance DecodeWire (Fixed Word64) where
  decodeWire (Fixed64Field _ val) = pure $ Fixed val
  decodeWire _ = empty

instance EncodeWire Bool where
  encodeWire t val = putWireTag t 0 <> putVarUInt (if val then 1 else (0 :: Int32))

instance DecodeWire Bool where
  decodeWire (VarintField _ val) = pure $ val /= 0
  decodeWire _ = empty

instance EncodeWire Float where
  encodeWire t val = putWireTag t 5 <> putFloat32le val

instance DecodeWire Float where
  decodeWire (Fixed32Field _ val) = pure $ castWord32ToFloat val
  decodeWire _ = empty

instance EncodeWire Double where
  encodeWire t val = putWireTag t 1 <> putFloat64le val

instance DecodeWire Double where
  decodeWire (Fixed64Field _ val) = pure $ castWord64ToDouble val
  decodeWire _ = empty

instance EncodeWire Builder where
  encodeWire t val = putWireTag t 2 <> putVarUInt (size val) <> val

instance EncodeWire LBS.ByteString where
  encodeWire t val = encodeWire t $ fromLazyByteString val

instance EncodeWire ByteString where
  encodeWire t val = encodeWire t $ fromByteString val

instance DecodeWire ByteString where
  decodeWire (DelimitedField _ bs) = pure bs
  decodeWire _ = empty

instance EncodeWire String where
  encodeWire t = encodeWire t . T.pack

instance DecodeWire String where
  decodeWire = fmap T.unpack . decodeWire

instance EncodeWire T.Text where
  encodeWire t = encodeWire t . T.encodeUtf8

instance DecodeWire T.Text where
  decodeWire (DelimitedField _ bs) =
    case T.decodeUtf8' bs of
      Right val -> pure val
      Left err  -> fail $ "Decoding failed: " ++ show err
  decodeWire _ = empty

decodePackedList :: Get a -> WireField -> Get [a]
{-# INLINE decodePackedList #-}
decodePackedList g (DelimitedField _ bs) = return $ runGet (many g) (LBS.fromStrict bs)
decodePackedList _ _ = empty

-- |
-- Empty lists are not written out
encodePackedList :: Tag -> Builder -> Builder
{-# INLINE encodePackedList #-}
encodePackedList t p = case size p of
  0 -> mempty
  _ -> encodeWire t p

instance EncodeWire (PackedList (Value Int32)) where
  encodeWire t (PackedList xs) =
    encodePackedList t $ foldMap (putVarSInt . runValue) xs

instance DecodeWire (PackedList (Value Int32)) where
  decodeWire x = do
    xs <- decodePackedList getVarInt x
    return . PackedList $ Value <$> xs

instance EncodeWire (PackedList (Value Int64)) where
  encodeWire t (PackedList xs) =
    encodePackedList t $ foldMap (putVarSInt . runValue) xs

instance DecodeWire (PackedList (Value Int64)) where
  decodeWire x = do
    xs <- decodePackedList getVarInt x
    return . PackedList $ Value <$> xs

instance EncodeWire (PackedList (Value Word32)) where
  encodeWire t (PackedList xs) =
    encodePackedList t $ foldMap (putVarUInt . runValue) xs

instance DecodeWire (PackedList (Value Word32)) where
  decodeWire x = do
    xs <- decodePackedList getVarInt x
    return . PackedList $ Value <$> xs

instance EncodeWire (PackedList (Value Word64)) where
  encodeWire t (PackedList xs) =
    encodePackedList t $ foldMap (putVarUInt . runValue) xs

instance DecodeWire (PackedList (Value Word64)) where
  decodeWire x = do
    xs <- decodePackedList getVarInt x
    return . PackedList $ Value <$> xs

instance EncodeWire (PackedList (Value (Signed Int32))) where
  encodeWire t (PackedList xs) = do
    let c (Signed x) = putVarSInt $ zzEncode32 x
    encodePackedList t $ foldMap (c . runValue) xs

instance DecodeWire (PackedList (Value (Signed Int32))) where
  decodeWire x = do
    xs <- decodePackedList getVarInt x
    return . PackedList $ Value . Signed . zzDecode32 <$> xs

instance EncodeWire (PackedList (Value (Signed Int64))) where
  encodeWire t (PackedList xs) = do
    let c (Signed x) = putVarSInt $ zzEncode64 x
    encodePackedList t $ foldMap (c . runValue) xs

instance DecodeWire (PackedList (Value (Signed Int64))) where
  decodeWire x = do
    xs <- decodePackedList getVarInt x
    return . PackedList $ Value . Signed . zzDecode64 <$> xs

instance EncodeWire (PackedList (Value (Fixed Word32))) where
  encodeWire t (PackedList xs) = do
    let c (Fixed x) = putWord32le x
    encodePackedList t $ foldMap (c . runValue) xs

instance DecodeWire (PackedList (Value (Fixed Word32))) where
  decodeWire x = do
    xs <- decodePackedList getWord32le x
    return . PackedList $ Value . Fixed <$> xs

instance EncodeWire (PackedList (Value (Fixed Word64))) where
  encodeWire t (PackedList xs) = do
    let c (Fixed x) = putWord64le x
    encodePackedList t $ foldMap (c . runValue) xs

instance DecodeWire (PackedList (Value (Fixed Word64))) where
  decodeWire x = do
    xs <- decodePackedList getWord64le x
    return . PackedList $ Value . Fixed <$> xs

instance EncodeWire (PackedList (Value (Fixed Int32))) where
  encodeWire t (PackedList xs) = do
    let c (Fixed x) = putWord32le $ fromIntegral x
    encodePackedList t $ foldMap (c . runValue) xs

instance DecodeWire (PackedList (Value (Fixed Int32))) where
  decodeWire x = do
    xs <- decodePackedList getWord32le x
    return . PackedList $ Value . Fixed . fromIntegral <$> xs

instance EncodeWire (PackedList (Value (Fixed Int64))) where
  encodeWire t (PackedList xs) = do
    let c (Fixed x) = putWord64le $ fromIntegral x
    encodePackedList t $ foldMap (c . runValue) xs

instance DecodeWire (PackedList (Value (Fixed Int64))) where
  decodeWire x = do
    xs <- decodePackedList getWord64le x
    return . PackedList $ Value . Fixed . fromIntegral <$> xs

instance EncodeWire (PackedList (Value Float)) where
  encodeWire t (PackedList xs) =
    encodePackedList t $ foldMap (putFloat32le . runValue) xs

instance DecodeWire (PackedList (Value Float)) where
  decodeWire x = do
    xs <- decodePackedList getFloat32le x
    return . PackedList $ Value <$> xs

instance EncodeWire (PackedList (Value Double)) where
  encodeWire t (PackedList xs) =
    encodePackedList t $ foldMap (putFloat64le . runValue) xs

instance DecodeWire (PackedList (Value Double)) where
  decodeWire x = do
    xs <- decodePackedList getFloat64le x
    return . PackedList $ Value <$> xs

instance EncodeWire (PackedList (Value Bool)) where
  encodeWire t (PackedList xs) =
    encodePackedList t $ foldMap (putVarUInt . fromEnum) xs

instance DecodeWire (PackedList (Value Bool)) where
  decodeWire x = do
    xs <- decodePackedList getVarInt x
    return . PackedList $ toEnum <$> xs

instance Enum a => EncodeWire (PackedList (Enumeration a)) where
  encodeWire t (PackedList xs) =
    encodePackedList t $ foldMap (putVarUInt . fromEnum) xs

instance Enum a => DecodeWire (PackedList (Enumeration a)) where
  decodeWire x = do
    xs <- decodePackedList getVarInt x
    return . PackedList $ toEnum <$> xs

instance (Foldable f, Enum a) => EncodeWire (f (Enumeration a)) where
  encodeWire t = foldMap (encodeWire t . c . runEnumeration) where
    c :: a -> Int32
    c = fromIntegral . fromEnum

instance Enum a => DecodeWire (Enumeration a) where
  decodeWire f = c <$> decodeWire f where
    c :: Int32 -> Enumeration a
    c = Enumeration . toEnum . fromIntegral

instance Enum a => DecodeWire (Maybe (Enumeration a)) where
  decodeWire f = c <$> decodeWire f where
    c :: Int32 -> Maybe (Enumeration a)
    c = Just . Enumeration . toEnum . fromIntegral

instance Enum a => DecodeWire (Always (Enumeration a)) where
  decodeWire f = c <$> decodeWire f where
    c :: Int32 -> Always (Enumeration a)
    c = Always . Enumeration . toEnum . fromIntegral

-- Taken from google's code, but I had to explcitly add fromIntegral in the right places:
zzEncode32 :: Int32 -> Word32
zzEncode32 x = fromIntegral ((x `shiftL` 1) `xor` x `shiftR` 31)
zzEncode64 :: Int64 -> Word64
zzEncode64 x = fromIntegral ((x `shiftL` 1) `xor` x `shiftR` 63)
zzDecode32 :: Word32 -> Int32
zzDecode32 w = fromIntegral (w `shiftR` 1) `xor` negate (fromIntegral (w .&. 1))
zzDecode64 :: Word64 -> Int64
zzDecode64 w = fromIntegral (w `shiftR` 1) `xor` negate (fromIntegral (w .&. 1))
