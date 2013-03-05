{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE UndecidableInstances #-}

import Test.Framework (Test, defaultMain, testGroup)
import Test.Framework.Providers.HUnit
import Test.Framework.Providers.QuickCheck2 (testProperty)
import Test.HUnit (Assertion, assert, assertEqual, assertFailure)
import Test.QuickCheck
import Test.QuickCheck.Modifiers
import Test.QuickCheck.Property

import GHC.Generics (Generic)

import Control.Applicative
import Control.Exception (SomeException, evaluate, try)
import Control.Monad
import Control.Monad.Identity
import qualified Data.ByteString as B
import Data.ByteString.Char8 ()
import Data.ProtocolBuffers as Pb
import Data.ProtocolBuffers.Internal as Pb
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HashMap
import Data.Hex
import Data.Int
import Data.List
import Data.Monoid
import Data.Serialize (runGet, runPut)
import Data.Proxy
import Data.Text (Text)
import qualified Data.Text as T
import Data.Typeable
import Data.Word
import Data.TypeLevel (D1, D2, D3, D4, Nat, reifyIntegral)

main :: IO ()
main = defaultMain tests

data EnumFoo
  = EnumFoo1
  | EnumFoo2
  | EnumFoo3
    deriving (Bounded, Enum, Eq, Typeable)

tests =
  [ testGroup "Primitive Wire" primitiveWireTests
  , testGroup "Packed Wire" packedWireTests
  , testGroup "Required Single Values" requiredSingleValueTests
  , testGroup "Optional Single Values" optionalSingleValueTests
  , testGroup "Tags Out of Range" tagsOutOfRangeTests
  , testProperty "Generic message coding" prop_generic
  , testProperty "Generic length prefixed message coding" prop_generic_length_prefixed
  , testCase "Google Reference Test1" test1
  , testCase "Google Reference Test2" test2
  , testCase "Google Reference Test3" test3
  , testCase "Google Reference Test4" test4
  , testCase "Google WireFormatTest ZigZag" wireFormatZZ
  ]

primitiveTests :: (forall a . (Eq a, Typeable a, Arbitrary a, EncodeWire a, DecodeWire a) => Proxy a -> Property) -> [Test]
primitiveTests f =
  [ testProperty "int32"    (f (Proxy :: Proxy Int32))
  , testProperty "int64"    (f (Proxy :: Proxy Int64))
  , testProperty "word32"   (f (Proxy :: Proxy Word32))
  , testProperty "word64"   (f (Proxy :: Proxy Word64))
  , testProperty "sint32"   (f (Proxy :: Proxy (Signed Int32)))
  , testProperty "sint64"   (f (Proxy :: Proxy (Signed Int64)))
  , testProperty "fixed32"  (f (Proxy :: Proxy (Pb.Fixed Word32)))
  , testProperty "fixed64"  (f (Proxy :: Proxy (Pb.Fixed Word64)))
  , testProperty "sfixed32" (f (Proxy :: Proxy (Pb.Fixed Int32)))
  , testProperty "sfixed64" (f (Proxy :: Proxy (Pb.Fixed Int64)))
  , testProperty "float"    (f (Proxy :: Proxy Float))
  , testProperty "double"   (f (Proxy :: Proxy Double))
  , testProperty "bool"     (f (Proxy :: Proxy Bool))
  , testProperty "enum"     (f (Proxy :: Proxy (Always (Enumeration EnumFoo))))
  ]

primitiveWireTests :: [Test]
primitiveWireTests = primitiveTests prop_wire

packedWireTests :: [Test]
packedWireTests =
  [ testProperty "int32"    (prop_wire (Proxy :: Proxy (PackedList (Value Int32))))
  , testProperty "int64"    (prop_wire (Proxy :: Proxy (PackedList (Value Int64))))
  , testProperty "word32"   (prop_wire (Proxy :: Proxy (PackedList (Value Word32))))
  , testProperty "word64"   (prop_wire (Proxy :: Proxy (PackedList (Value Word64))))
  , testProperty "sint32"   (prop_wire (Proxy :: Proxy (PackedList (Value (Signed Int32)))))
  , testProperty "sint64"   (prop_wire (Proxy :: Proxy (PackedList (Value (Signed Int64)))))
  , testProperty "fixed32"  (prop_wire (Proxy :: Proxy (PackedList (Value (Pb.Fixed Word32)))))
  , testProperty "fixed64"  (prop_wire (Proxy :: Proxy (PackedList (Value (Pb.Fixed Word64)))))
  , testProperty "sfixed32" (prop_wire (Proxy :: Proxy (PackedList (Value (Pb.Fixed Int32)))))
  , testProperty "sfixed64" (prop_wire (Proxy :: Proxy (PackedList (Value (Pb.Fixed Int64)))))
  , testProperty "float"    (prop_wire (Proxy :: Proxy (PackedList (Value Float))))
  , testProperty "double"   (prop_wire (Proxy :: Proxy (PackedList (Value Double))))
  , testProperty "bool"     (prop_wire (Proxy :: Proxy (PackedList (Value Bool))))
  ]

requiredSingleValueTests :: [Test]
requiredSingleValueTests = primitiveTests prop_req

optionalSingleValueTests :: [Test]
optionalSingleValueTests = primitiveTests prop_opt

tagsOutOfRangeTests :: [Test]
tagsOutOfRangeTests = primitiveTests prop_req_out_of_range

instance Arbitrary a => Arbitrary (Field n (RequiredField (Always (Value a)))) where
  arbitrary = putField <$> arbitrary
  shrink = fmap putField . shrink . getField

instance Arbitrary a => Arbitrary (Field n (OptionalField (Last (Value a)))) where
  arbitrary = putField <$> arbitrary
  shrink = fmap putField . shrink . getField

instance Arbitrary a => Arbitrary (Field n (RepeatedField [Value a])) where
  arbitrary = putField <$> listOf1 arbitrary
  shrink = fmap putField . shrink . getField

instance Arbitrary a => Arbitrary (PackedList a) where
  arbitrary = PackedList <$> listOf1 arbitrary
  shrink = fmap PackedList . shrink . unPackedList

instance Arbitrary a => Arbitrary (Signed a) where
  arbitrary = Signed <$> arbitrary
  shrink (Signed x) = fmap Signed $ shrink x

instance Arbitrary a => Arbitrary (Value a) where
  arbitrary = Value <$> arbitrary
  shrink (Value x) = fmap Value $ shrink x

instance (Bounded a, Enum a) => Arbitrary (Enumeration a) where
  arbitrary = Enumeration <$> elements [minBound..maxBound]
  shrink (Enumeration x) = Enumeration . toEnum <$> shrink (fromEnum x)

instance Arbitrary a => Arbitrary (Pb.Fixed a) where
  arbitrary = Pb.Fixed <$> arbitrary
  shrink (Pb.Fixed x) = fmap Pb.Fixed $ shrink x

instance Arbitrary a => Arbitrary (Always a) where
  arbitrary = Always <$> arbitrary
  shrink (Always x) = Always <$> shrink x

instance Arbitrary WireField where
  arbitrary = do
    tag <- choose (0, 536870912)
    oneof
      [ VarintField tag             <$> arbitrary
      , Fixed64Field tag            <$> arbitrary
      , DelimitedField tag . B.pack <$> arbitrary
      , Fixed32Field tag            <$> arbitrary
      ]

  shrink (VarintField t v)    = VarintField    <$> shrink t <*> shrink v
  shrink (Fixed64Field t v)   = Fixed64Field   <$> shrink t <*> shrink v
  shrink (DelimitedField t v) = DelimitedField <$> shrink t <*> fmap B.pack (shrink (B.unpack v))
  shrink (Fixed32Field t v)   = Fixed32Field   <$> shrink t <*> shrink v

newtype RequiredValue n a = RequiredValue (Required n (Value a))
  deriving (Eq, Generic)

instance (EncodeWire a, Nat n) => Encode (RequiredValue n a)
instance (DecodeWire a, Nat n) => Decode (RequiredValue n a)

newtype OptionalValue n a = OptionalValue (Optional n (Value a))
  deriving (Eq, Generic)

instance (EncodeWire a, Nat n) => Encode (OptionalValue n a)
instance (DecodeWire a, Nat n) => Decode (OptionalValue n a)

prop_wire :: forall a . (Eq a, Arbitrary a, EncodeWire a, DecodeWire a, Typeable a) => Proxy a -> Property
prop_wire _ = label ("prop_wire :: " ++ show (typeOf (undefined :: a))) $ do
  tag <- choose (0, 536870912)
  val <- arbitrary
  let bs = runPut (encodeWire tag (val :: a))
      dec = do
        field <- getWireField
        guard $ tag == wireFieldTag field
        decodeWire field
  case runGet dec bs of
    Right val' -> return $ val == val'
    Left err   -> fail err

prop_generic :: Property
prop_generic = do
  msg <- HashMap.fromListWith (++) . fmap (\ c -> (wireFieldTag c, [c])) <$> listOf1 arbitrary
  prop_roundtrip msg

prop_generic_length_prefixed :: Property
prop_generic_length_prefixed = do
  msg <- HashMap.fromListWith (++) . fmap (\ c -> (wireFieldTag c, [c])) <$> listOf1 arbitrary
  let bs = runPut $ encodeLengthPrefixedMessage (msg :: HashMap Tag [WireField])
  case runGet decodeLengthPrefixedMessage bs of
    Right msg' -> printTestCase "foo" $ msg == msg'
    Left err   -> fail err

prop_roundtrip :: (Eq a, Encode a, Decode a) => a -> Property
prop_roundtrip msg = do
  let bs = runPut $ encodeMessage msg
  case runGet decodeMessage bs of
    Right msg' -> property $ msg == msg'
    Left err   -> fail err

prop_encode_fail :: Encode a => a -> Property
prop_encode_fail msg = morallyDubiousIOProperty $ do
  res <- try . evaluate . runPut $ encodeMessage msg
  return $ case res :: Either SomeException B.ByteString of
    Left  _ -> True
    Right _ -> False

prop_req_reify_out_of_range :: forall a r . a -> (forall n . Nat n => RequiredValue n a -> Gen r) -> Gen r
prop_req_reify_out_of_range a f = do
  let g :: forall n . Nat n => n -> Gen r
      g _ = f (RequiredValue (putField a) :: RequiredValue n a)
  -- according to https://developers.google.com/protocol-buffers/docs/proto
  -- the max is 2^^29 - 1, or 536,870,911.
  --
  -- the min is set to 0 since reifyIntegral only supports naturals, which
  -- is also recommended since these are encoded as varints which have
  -- fairly high overhead for negative tags
  n <- choose (536870912, maxBound)
  reifyIntegral (n :: Int32) g

prop_req_reify :: forall a r . a -> (forall n . Nat n => RequiredValue n a -> Gen r) -> Gen r
prop_req_reify a f = do
  let g :: forall n . Nat n => n -> Gen r
      g _ = f (RequiredValue (putField a) :: RequiredValue n a)
  -- according to https://developers.google.com/protocol-buffers/docs/proto
  -- the max is 2^^29 - 1, or 536,870,911.
  --
  -- the min is set to 0 since reifyIntegral only supports naturals, which
  -- is also recommended since these are encoded as varints which have
  -- fairly high overhead for negative tags
  n <- choose (0, 536870911)
  reifyIntegral (n :: Int32) g

prop_req_out_of_range :: forall a . (Arbitrary a, EncodeWire a) => Proxy a -> Property
prop_req_out_of_range _ = do
  val <- Just <$> arbitrary
  prop_req_reify_out_of_range (val :: Maybe (Value a)) prop_encode_fail

prop_req :: forall a . (Arbitrary a, Eq a, EncodeWire a, DecodeWire a, Typeable a) => Proxy a -> Property
prop_req _ = label ("prop_req :: " ++ show (typeOf (undefined :: a))) $ do
  val <- Just <$> arbitrary
  prop_req_reify (val :: Maybe (Value a)) prop_roundtrip

prop_opt_reify :: forall a r . Maybe a -> (forall n . Nat n => OptionalValue n a -> Gen r) -> Gen r
prop_opt_reify a f = do
  let g :: forall n . Nat n => n -> Gen r
      g _ = f (OptionalValue (putField a) :: OptionalValue n a)
  -- according to https://developers.google.com/protocol-buffers/docs/proto
  -- the max is 2^^29 - 1, or 536,870,911.
  --
  -- the min is set to 0 since reifyIntegral only supports naturals, which
  -- is also recommended since these are encoded as varints which have
  -- fairly high overhead for negative tags
  n <- choose (0, 536870911)
  reifyIntegral (n :: Int32) g

prop_opt :: forall a . (Arbitrary a, Eq a, EncodeWire a, DecodeWire a, Typeable a) => Proxy a -> Property
prop_opt _ = label ("prop_opt :: " ++ show (typeOf (undefined :: a))) $ do
  val <- arbitrary
  prop_opt_reify (val :: Maybe a) prop_roundtrip

-- implement the examples from https://developers.google.com/protocol-buffers/docs/encoding
testSpecific msg ref = do
  let bs = runPut $ encodeMessage msg
  assertEqual "Encoded message mismatch" bs ref

  case runGet decodeMessage bs of
    Right msg' -> assertEqual "Decoded message mismatch" msg msg'
    Left err   -> assertFailure err

data Test1 = Test1{test1_a :: Required D1 (Value Int32)} deriving (Generic)
deriving instance Eq Test1
deriving instance Show Test1
instance Encode Test1
instance Decode Test1

test1 :: Assertion
test1 = testSpecific msg =<< unhex "089601" where
  msg = Test1{test1_a = putField 150}

data Test2 = Test2{test2_b :: Required D2 (Value Text)} deriving (Generic)
deriving instance Eq Test2
deriving instance Show Test2
instance Encode Test2
instance Decode Test2

test2 :: Assertion
test2 = testSpecific msg =<< unhex "120774657374696e67" where
  msg = Test2{test2_b = putField "testing"}

data Test3 = Test3{test3_c :: Required D3 (Message Test1)} deriving (Generic, Eq, Show)
instance Encode Test3
instance Decode Test3

test3 :: Assertion
test3 = testSpecific msg =<< unhex "1a03089601" where
  msg = Test3{test3_c = putField Test1{test1_a = putField 150}}

data Test4 = Test4{test4_d :: Packed D4 (Value Word32)} deriving (Generic, Eq, Show)
instance Encode Test4
instance Decode Test4

test4 :: Assertion
test4 = testSpecific msg =<< unhex "2206038e029ea705" where
  msg = Test4{test4_d = putField [3,270,86942]}

-- some from http://code.google.com/p/protobuf/source/browse/trunk/src/google/protobuf/wire_format_unittest.cc
wireFormatZZ :: Assertion
wireFormatZZ = do
  assert $ 0 == zzEncode32   0
  assert $ 1 == zzEncode32 (-1)
  assert $ 2 == zzEncode32   1
  assert $ 3 == zzEncode32 (-2)
  assert $ 0x7FFFFFFE == zzEncode32 0x3FFFFFFF
  assert $ 0x7FFFFFFF == zzEncode32 0xC0000000
  assert $ 0xFFFFFFFE == zzEncode32 0x7FFFFFFF
  assert $ 0xFFFFFFFF == zzEncode32 0x80000000

  assert $   0  == zzDecode32 0
  assert $ (-1) == zzDecode32 1
  assert $   1  == zzDecode32 2
  assert $ (-2) == zzDecode32 3
  assert $ 0x3FFFFFFF == zzDecode32 0x7FFFFFFE
  assert $ 0xC0000000 == zzDecode32 0x7FFFFFFF
  assert $ 0x7FFFFFFF == zzDecode32 0xFFFFFFFE
  assert $ 0x80000000 == zzDecode32 0xFFFFFFFF

  assert $ 0 == zzEncode64   0
  assert $ 1 == zzEncode64 (-1)
  assert $ 2 == zzEncode64   1
  assert $ 3 == zzEncode64 (-2)
  assert $ 0x000000007FFFFFFE == zzEncode64 0x000000003FFFFFFF
  assert $ 0x000000007FFFFFFF == zzEncode64 0xFFFFFFFFC0000000
  assert $ 0x00000000FFFFFFFE == zzEncode64 0x000000007FFFFFFF
  assert $ 0x00000000FFFFFFFF == zzEncode64 0xFFFFFFFF80000000
  assert $ 0xFFFFFFFFFFFFFFFE == zzEncode64 0x7FFFFFFFFFFFFFFF
  assert $ 0xFFFFFFFFFFFFFFFF == zzEncode64 0x8000000000000000

  assert $   0  == zzDecode64 0
  assert $ (-1) == zzDecode64 1
  assert $   1  == zzDecode64 2
  assert $ (-2) == zzDecode64 3
  assert $ 0x000000003FFFFFFF == zzDecode64 0x000000007FFFFFFE
  assert $ 0xFFFFFFFFC0000000 == zzDecode64 0x000000007FFFFFFF
  assert $ 0x000000007FFFFFFF == zzDecode64 0x00000000FFFFFFFE
  assert $ 0xFFFFFFFF80000000 == zzDecode64 0x00000000FFFFFFFF
  assert $ 0x7FFFFFFFFFFFFFFF == zzDecode64 0xFFFFFFFFFFFFFFFE
  assert $ 0x8000000000000000 == zzDecode64 0xFFFFFFFFFFFFFFFF

  -- these tests are already covered by QuickCheck properties:
  -- Some easier-to-verify round-trip tests.  The inputs (other than 0, 1, -1)
  -- were chosen semi-randomly via keyboard bashing.
  let rt32 = zzDecode32 . zzEncode32
      rt64 = zzDecode64 . zzEncode64

  assert $      0  == rt32      0
  assert $      1  == rt32      1
  assert $ (   -1) == rt32 (   -1)
  assert $  14927  == rt32  14927
  assert $ (-3612) == rt32 (-3612)

  assert $      0  == rt64      0
  assert $      1  == rt64      1
  assert $ (   -1) == rt64 (   -1)
  assert $  14927  == rt64  14927
  assert $ (-3612) == rt64 (-3612)

  assert $     856912304801416  == rt64     856912304801416
  assert $ (-75123905439571256) == rt64 (-75123905439571256)
