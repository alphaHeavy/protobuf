{-# LANGUAGE DataKinds #-}
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
{-# OPTIONS_GHC -fno-warn-orphans #-}

import Test.QuickCheck
import Test.QuickCheck.Property
import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck

import GHC.Generics
import GHC.TypeLits

import Control.Applicative
import Control.Exception (SomeException, evaluate, try)
import Control.Monad
import qualified Data.ByteString as B
import Data.ByteString.Char8 ()
import Data.ProtocolBuffers as Pb
import Data.ProtocolBuffers.Internal as Pb
import Data.ProtocolBuffers.Orphans ()
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HashMap
import Data.Hex
import Data.Int
import Data.IntSet (IntSet)
import qualified Data.IntSet as IntSet
import Data.Monoid
import Data.Binary.Get (Get, runGet)
import Data.Binary.Builder.Sized (Builder, toLazyByteString)
import Data.Proxy
import Data.Text (Text)
import Data.Typeable
import Data.Word
import qualified Data.ByteString.Lazy as LBS
import Data.Binary.Get (runGetOrFail)

main :: IO ()
main = defaultMain tests

data EnumFoo
  = EnumFoo1
  | EnumFoo2
  | EnumFoo3
    deriving (Bounded, Enum, Eq, Typeable)

tests :: TestTree
tests = testGroup "Root"
  [ testGroup "Primitive Wire" primitiveWireTests
  , testGroup "Packed Wire" packedWireTests
  , testGroup "Required Single Values" requiredSingleValueTests
  , testGroup "Optional Single Values" optionalSingleValueTests
  , testGroup "Repeated Single Values" repeatedSingleValueTests
  -- TODO Fix and re-enable
  --, testGroup "Tags Out of Range" tagsOutOfRangeTests
  , testProperty "Generic message coding" prop_generic
  , testProperty "Generic length prefixed message coding" prop_generic_length_prefixed
  , testProperty "Varint prefixed bytestring" prop_varint_prefixed_bytestring
  , testProperty "Random message" prop_message
  , testCase "Google Reference Test1" test1
  , testCase "Google Reference Test2" test2
  , testCase "Google Reference Test3" test3
  , testCase "Google Reference Test4" test4
  , testCase "Optional Enum Test5: Nothing" test5
  , testCase "Optional Enum Test5: Just Test5A" test6
  , testCase "Optional Enum Test5: Just Test5B" test7
  , testCase "Repeated Enum Test6: []" test8
  , testCase "Repeated Enum Test6: [Test6A]" test9
  , testCase "Repeated Enum Test6: [Test6A, Test6B]" test10
  , testCase "Repeated Enum Test6: [Test6A, Test6A]" test11
  , testCase "Repeated Enum Test6: [Test6A, Test6B, Test6A]" test12
  , testCase "Repeated Enum Test7: []" test13
  , testCase "Repeated Enum Test7: [Test7]" test14
  , testCase "Repeated Enum Test7: [Test7, Test7]" test15
  , testCase "Google WireFormatTest ZigZag" wireFormatZZ
  ]

primitiveTests :: (forall a . (Eq a, Typeable a, Arbitrary a, EncodeWire a, DecodeWire a) => Proxy a -> Property) -> [TestTree]
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

primitiveWireTests :: [TestTree]
primitiveWireTests = primitiveTests prop_wire

packedWireTests :: [TestTree]
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

requiredSingleValueTests :: [TestTree]
requiredSingleValueTests = primitiveTests prop_req

optionalSingleValueTests :: [TestTree]
optionalSingleValueTests = primitiveTests prop_opt

repeatedSingleValueTests :: [TestTree]
repeatedSingleValueTests = primitiveTests prop_repeated

tagsOutOfRangeTests :: [TestTree]
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

instance (EncodeWire a, KnownNat n) => Encode (RequiredValue n a)
instance (DecodeWire a, KnownNat n) => Decode (RequiredValue n a)

newtype OptionalValue n a = OptionalValue (Optional n (Value a))
  deriving (Eq, Generic)

instance (EncodeWire a, KnownNat n) => Encode (OptionalValue n a)
instance (DecodeWire a, KnownNat n) => Decode (OptionalValue n a)

newtype RepeatedValue n a = RepeatedValue (Repeated n (Value a))
  deriving (Eq, Generic)

instance (EncodeWire a, KnownNat n) => Encode (RepeatedValue n a)
instance (DecodeWire a, KnownNat n) => Decode (RepeatedValue n a)

arbitraryField :: forall r . Int -> (forall a . (Monoid a, GEncode (K1 R a), GDecode (K1 R a), Eq a, Show a) => a -> Gen r) -> Gen r
arbitraryField i f =
  case someNatVal (fromIntegral i) of
    Nothing -> fail $ "someNatVal failed for " ++ show i
    Just (SomeNat (n :: Proxy n)) -> do
      flavor <- choose (1, 3)
      case flavor :: Int of
        0 -> do -- Packed
          which <- choose (0, 5)
          case which :: Int of
            0 -> arbitrary >>= \ x -> f (putField x :: Packed n (Value Float))
            1 -> arbitrary >>= \ x -> f (putField x :: Packed n (Value Double))
            2 -> arbitrary >>= \ x -> f (putField x :: Packed n (Value Int32))
            3 -> arbitrary >>= \ x -> f (putField x :: Packed n (Value Int64))
            4 -> arbitrary >>= \ x -> f (putField x :: Packed n (Value Word32))
            5 -> arbitrary >>= \ x -> f (putField x :: Packed n (Value Word64))
            -- 6 -> arbitraryMessage (\ (msg :: msg) -> oneof [return (Just msg), return Nothing] >>= \ msg' -> f (putField msg' :: Optional n (Message msg)))
            -- 7 -> arbitrary >>= \ x -> f (putField x :: Packed n (Value Text))
            -- 8 -> arbitrary >>= \ x -> f (putField x :: Packed n (Value B.ByteString))
        1 -> do -- Repeated
          which <- choose (0, 5)
          case which :: Int of
            0 -> arbitrary >>= \ x -> f (putField x :: Repeated n (Value Float))
            1 -> arbitrary >>= \ x -> f (putField x :: Repeated n (Value Double))
            2 -> arbitrary >>= \ x -> f (putField x :: Repeated n (Value Int32))
            3 -> arbitrary >>= \ x -> f (putField x :: Repeated n (Value Int64))
            4 -> arbitrary >>= \ x -> f (putField x :: Repeated n (Value Word32))
            5 -> arbitrary >>= \ x -> f (putField x :: Repeated n (Value Word64))
            -- 6 -> arbitraryMessage (\ (msg :: msg) -> oneof [return (Just msg), return Nothing] >>= \ msg' -> f (putField msg' :: Optional n (Message msg)))
            -- 7 -> arbitrary >>= \ x -> f (putField x :: Repeated n (Value Text))
            -- 8 -> arbitrary >>= \ x -> f (putField x :: Repeated n (Value B.ByteString))

        2 -> do -- Optional
          which <- choose (0, 6)
          case which :: Int of
            0 -> arbitrary >>= \ x -> f (putField x :: Optional n (Value Float))
            1 -> arbitrary >>= \ x -> f (putField x :: Optional n (Value Double))
            2 -> arbitrary >>= \ x -> f (putField x :: Optional n (Value Int32))
            3 -> arbitrary >>= \ x -> f (putField x :: Optional n (Value Int64))
            4 -> arbitrary >>= \ x -> f (putField x :: Optional n (Value Word32))
            5 -> arbitrary >>= \ x -> f (putField x :: Optional n (Value Word64))
            6 -> arbitraryMessage (\ (msg :: msg) -> oneof [return (Just msg), return Nothing] >>= \ msg' -> f (putField msg' :: Optional n (Message msg)))
            -- 7 -> arbitrary >>= \ x -> f (putField x :: Optional n (Value Text))
            -- 8 -> arbitrary >>= \ x -> f (putField x :: Optional n (Value B.ByteString))
        3 -> do -- Required
          which <- choose (0, 6)
          case which :: Int of
            0 -> arbitrary >>= \ x -> f (putField x :: Required n (Value Float))
            1 -> arbitrary >>= \ x -> f (putField x :: Required n (Value Double))
            2 -> arbitrary >>= \ x -> f (putField x :: Required n (Value Int32))
            3 -> arbitrary >>= \ x -> f (putField x :: Required n (Value Int64))
            4 -> arbitrary >>= \ x -> f (putField x :: Required n (Value Word32))
            5 -> arbitrary >>= \ x -> f (putField x :: Required n (Value Word64))
            6 -> arbitraryMessage (\ (msg :: msg) -> f (putField msg :: Required n (Message msg)))
            -- 7 -> arbitrary >>= \ x -> f (putField x :: Required n (Value Text))
            -- 8 -> arbitrary >>= \ x -> f (putField x :: Required n (Value B.ByteString))

data T1 a = T1 a deriving (Show, Eq,Generic)
instance GEncode (K1 R a) => Encode (T1 a)
instance GDecode (K1 R a) => Decode (T1 a)

data T2 a b = T2 a b deriving (Show, Eq,Generic)
instance (GEncode (K1 R a), GEncode (K1 R b)) => Encode (T2 a b)
instance (GDecode (K1 R a), GDecode (K1 R b)) => Decode (T2 a b)

data T3 a b c = T3 a b c deriving (Show, Eq,Generic)
instance (GEncode (K1 R a), GEncode (K1 R b), GEncode (K1 R c)) => Encode (T3 a b c)
instance (GDecode (K1 R a), GDecode (K1 R b), GDecode (K1 R c)) => Decode (T3 a b c)

arbitraryMessage :: forall r . (forall a . (Encode a, Decode a, Generic a, GMessageMonoid (Rep a), Eq a, Show a) => a -> Gen r) -> Gen r
arbitraryMessage f = do
  fieldCount <- choose (1, 3)
  xs <- fieldTags fieldCount
  case fieldCount of
    1 -> arbitraryField (xs !! 0) (\ f1 -> f (T1 f1))
    2 -> arbitraryField (xs !! 0) (\ f1 -> arbitraryField (xs !! 1) (\ f2 -> f (T2 f1 f2)))
    3 -> arbitraryField (xs !! 0) (\ f1 -> arbitraryField (xs !! 1) (\ f2 -> arbitraryField (xs !! 2) (\ f3 -> f (T3 f1 f2 f3))))

fieldTags :: Int -> Gen [Int]
fieldTags i = go IntSet.empty [] where
  go xs ys
    | IntSet.size xs >= i = return ys
    | otherwise = do
        next <- choose (0, 536870912)
        if next `IntSet.member` xs
          then go xs ys
          else go (IntSet.insert next xs) (next:ys)

prop_message :: Gen Property
prop_message = arbitraryMessage prop_roundtrip_msg

prop_wire :: forall a . (Eq a, Arbitrary a, EncodeWire a, DecodeWire a, Typeable a) => Proxy a -> Property
prop_wire _ = label ("prop_wire :: " ++ show (typeOf (undefined :: a))) $ do
  tag <- choose (0, 536870912)
  val <- arbitrary
  let bs = toLazyByteString (encodeWire tag (val :: a))
      dec = do
        field <- getWireField
        guard $ tag == wireFieldTag field
        decodeWire field
  case runGetOrFail dec bs of
    Right (_, _, val') -> return $ val == val'
    Left (_, _, err)   -> fail err

prop_generic :: Gen Property
prop_generic = do
  msg <- HashMap.fromListWith (++) . fmap (\ c -> (wireFieldTag c, [c])) <$> listOf1 arbitrary
  prop_roundtrip_msg msg

prop_generic_length_prefixed :: Gen Property
prop_generic_length_prefixed = do
  msg <- HashMap.fromListWith (++) . fmap (\ c -> (wireFieldTag c, [c])) <$> listOf1 arbitrary
  let bs = toLazyByteString $ encodeLengthPrefixedMessage (msg :: HashMap Tag [WireField])
  case runGetOrFail decodeLengthPrefixedMessage bs of
    Right (_, _, msg') -> return $ counterexample "foo" $ msg == msg'
    Left (_, _, err)   -> fail err

prop_roundtrip_msg :: (Eq a, Encode a, Decode a) => a -> Gen Property
prop_roundtrip_msg msg = do
  let bs = toLazyByteString $ encodeMessage msg
  case runGet decodeMessage bs of
    msg' -> return . property $ msg == msg'

prop_varint_prefixed_bytestring :: Gen Property
prop_varint_prefixed_bytestring = do
  bs <- B.pack <$> arbitrary
  prop_roundtrip_value getVarintPrefixedBS putVarintPrefixedBS bs

prop_roundtrip_value :: (Eq a, Show a) => Get a -> (a -> Builder) -> a -> Gen Property
prop_roundtrip_value get put val = do
  let bs = toLazyByteString (put val)
  case runGet get bs of
    val' -> return $ val === val'

prop_encode_fail :: Encode a => a -> Gen Prop
prop_encode_fail msg = unProperty $ ioProperty $ do
  res <- try . evaluate . toLazyByteString $ encodeMessage msg
  return $ case res :: Either SomeException LBS.ByteString of
    Left  _ -> True
    Right _ -> False

prop_req_reify_out_of_range :: forall a r . a -> (forall n . KnownNat n => RequiredValue n a -> Gen r) -> Gen r
prop_req_reify_out_of_range a f = do
  let g :: forall n . KnownNat n => Proxy n -> Gen r
      g _ = f (RequiredValue (putField a) :: RequiredValue n a)
  -- according to https://developers.google.com/protocol-buffers/docs/proto
  -- the max is 2^^29 - 1, or 536,870,911.
  --
  -- the min is set to 0 since reifyIntegral only supports naturals, which
  -- is also recommended since these are encoded as varints which have
  -- fairly high overhead for negative tags
  n <- choose (536870912, toInteger $ (maxBound :: Int))
  case someNatVal n of
    Just (SomeNat x) -> g x

prop_reify_valid_tag :: forall r . (forall n . KnownNat n => Proxy n -> Gen r) -> Gen r
prop_reify_valid_tag f = do
  -- according to https://developers.google.com/protocol-buffers/docs/proto
  -- the max is 2^^29 - 1, or 536,870,911.
  --
  -- the min is set to 0 since reifyIntegral only supports naturals, which
  -- is also recommended since these are encoded as varints which have
  -- fairly high overhead for negative tags
  n <- choose (0, 536870911)
  case someNatVal n of
    Just (SomeNat x) -> f x

prop_req_reify :: forall a r . a -> (forall n . KnownNat n => RequiredValue n a -> Gen r) -> Gen r
prop_req_reify a f = prop_reify_valid_tag g where
  g :: forall n . KnownNat n => Proxy n -> Gen r
  g _ = f (RequiredValue (putField a) :: RequiredValue n a)

prop_req_out_of_range :: forall a . (Arbitrary (Value a), EncodeWire a) => Proxy a -> Property
prop_req_out_of_range _ = MkProperty $ do
  val <- Just <$> arbitrary
  prop_req_reify_out_of_range (val :: Maybe (Value a)) prop_encode_fail

prop_req :: forall a . (Arbitrary (Value a), Eq a, EncodeWire a, DecodeWire a, Typeable a) => Proxy a -> Property
prop_req _ = label ("prop_req :: " ++ show (typeOf (undefined :: a))) $ do
  val <- Just <$> arbitrary
  prop_req_reify (val :: Maybe (Value a)) prop_roundtrip_msg

prop_repeated_reify :: forall a r . [a] -> (forall n . KnownNat n => RepeatedValue n a -> Gen r) -> Gen r
prop_repeated_reify a f = prop_reify_valid_tag g where
  g :: forall n . KnownNat n => Proxy n -> Gen r
  g _ = f (RepeatedValue (putField a) :: RepeatedValue n a)

prop_repeated :: forall a . (Arbitrary a, Eq a, EncodeWire a, DecodeWire a, Typeable a) => Proxy a -> Property
prop_repeated _ = label ("prop_repeated :: " ++ show (typeOf (undefined :: a))) $ do
  val <- arbitrary
  prop_repeated_reify (val :: [a]) prop_roundtrip_msg

prop_opt_reify :: forall a r . Maybe a -> (forall n . KnownNat n => OptionalValue n a -> Gen r) -> Gen r
prop_opt_reify a f = prop_reify_valid_tag g where
  g :: forall n . KnownNat n => Proxy n -> Gen r
  g _ = f (OptionalValue (putField a) :: OptionalValue n a)

prop_opt :: forall a . (Arbitrary a, Eq a, EncodeWire a, DecodeWire a, Typeable a) => Proxy a -> Property
prop_opt _ = label ("prop_opt :: " ++ show (typeOf (undefined :: a))) $ do
  val <- arbitrary
  prop_opt_reify (val :: Maybe a) prop_roundtrip_msg

-- implement the examples from https://developers.google.com/protocol-buffers/docs/encoding
testSpecific :: (Eq a, Show a, Encode a, Decode a) => a -> B.ByteString -> IO ()
testSpecific msg ref = do
  let bs = toLazyByteString $ encodeMessage msg
  assertEqual "Encoded message mismatch" (LBS.toStrict bs) ref

  case runGetOrFail decodeMessage bs of
    Right (_, _, msg') -> assertEqual "Decoded message mismatch" msg msg'
    Left (_, _, err)   -> assertFailure err

data Test1 = Test1{test1_a :: Required 1 (Value Int32)} deriving (Generic)
deriving instance Eq Test1
deriving instance Show Test1
instance Encode Test1
instance Decode Test1

test1 :: Assertion
test1 = testSpecific msg =<< unhex "089601" where
  msg = Test1{test1_a = putField 150}

data Test2 = Test2{test2_b :: Required 2 (Value Text)} deriving (Generic)
deriving instance Eq Test2
deriving instance Show Test2
instance Encode Test2
instance Decode Test2

test2 :: Assertion
test2 = testSpecific msg =<< unhex "120774657374696e67" where
  msg = Test2{test2_b = putField "testing"}

data Test3 = Test3{test3_c :: Required 3 (Message Test1)} deriving (Generic, Eq, Show)
instance Encode Test3
instance Decode Test3

test3 :: Assertion
test3 = testSpecific msg =<< unhex "1a03089601" where
  msg = Test3{test3_c = putField Test1{test1_a = putField 150}}

data Test4 = Test4{test4_d :: Packed 4 (Value Word32)} deriving (Generic, Eq, Show)
instance Encode Test4
instance Decode Test4

test4 :: Assertion
test4 = testSpecific msg =<< unhex "2206038e029ea705" where
  msg = Test4{test4_d = putField [3,270,86942]}

data Test5Enum = Test5A | Test5B deriving (Eq, Show, Enum)
data Test5 = Test5{test5_e :: Optional 5 (Enumeration Test5Enum)} deriving (Generic, Eq, Show)
instance Encode Test5
instance Decode Test5

data Test6Enum = Test6A | Test6B deriving (Eq, Show, Enum)
data Test6 = Test6{test6_e :: Repeated 6 (Enumeration Test6Enum)} deriving (Generic, Eq, Show)
instance Encode Test6
instance Decode Test6

data Test7Enum = Test7A deriving (Eq, Show, Enum)
data Test7 = Test7{test7_e :: Repeated 7 (Enumeration Test7Enum)} deriving (Generic, Eq, Show)
instance Encode Test7
instance Decode Test7

test5 :: Assertion
test5 = testSpecific msg =<< unhex "" where
  msg = Test5{test5_e = putField Nothing}

test6 :: Assertion
test6 = testSpecific msg =<< unhex "2800" where
  msg = Test5{test5_e = putField $ Just Test5A }

test7 :: Assertion
test7 = testSpecific msg =<< unhex "2801" where
  msg = Test5{test5_e = putField $ Just Test5B }

test8 :: Assertion
test8 = testSpecific msg =<< unhex "" where
  msg = Test6{test6_e = putField $ [] }

test9 :: Assertion
test9 = testSpecific msg =<< unhex "3000" where
  msg = Test6{test6_e = putField $ [Test6A] }

test10 :: Assertion
test10 = testSpecific msg =<< unhex "30003001" where
  msg = Test6{test6_e = putField $ [Test6A, Test6B]}

test11 :: Assertion
test11 = testSpecific msg =<< unhex "30003000" where
  msg = Test6{test6_e = putField $ [Test6A, Test6A]}

test12 :: Assertion
test12 = testSpecific msg =<< unhex "300030013000" where
  msg = Test6{test6_e = putField $ [Test6A, Test6B, Test6A]}

test13 :: Assertion
test13 = testSpecific msg =<< unhex "" where
  msg = Test7{test7_e = putField $ [] }

test14 :: Assertion
test14 = testSpecific msg =<< unhex "3800" where
  msg = Test7{test7_e = putField $ [Test7A] }

test15 :: Assertion
test15 = testSpecific msg =<< unhex "38003800" where
  msg = Test7{test7_e = putField $ [Test7A, Test7A] }

-- some from http://code.google.com/p/protobuf/source/browse/trunk/src/google/protobuf/wire_format_unittest.cc
wireFormatZZ :: Assertion
wireFormatZZ = do
  assert $ 0 == zzEncode32   0
  assert $ 1 == zzEncode32 (-1)
  assert $ 2 == zzEncode32   1
  assert $ 3 == zzEncode32 (-2)
  assert $ 0x7FFFFFFE == zzEncode32 (fromIntegral 0x3FFFFFFF)
  assert $ 0x7FFFFFFF == zzEncode32 (fromIntegral 0xC0000000)
  assert $ 0xFFFFFFFE == zzEncode32 (fromIntegral 0x7FFFFFFF)
  assert $ 0xFFFFFFFF == zzEncode32 (fromIntegral 0x80000000)

  assert $   0  == zzDecode32 0
  assert $ (-1) == zzDecode32 1
  assert $   1  == zzDecode32 2
  assert $ (-2) == zzDecode32 3
  assert $ fromIntegral 0x3FFFFFFF == zzDecode32 0x7FFFFFFE
  assert $ fromIntegral 0xC0000000 == zzDecode32 0x7FFFFFFF
  assert $ fromIntegral 0x7FFFFFFF == zzDecode32 0xFFFFFFFE
  assert $ fromIntegral 0x80000000 == zzDecode32 0xFFFFFFFF

  assert $ 0 == zzEncode64   0
  assert $ 1 == zzEncode64 (-1)
  assert $ 2 == zzEncode64   1
  assert $ 3 == zzEncode64 (-2)
  assert $ 0x000000007FFFFFFE == zzEncode64 (fromIntegral 0x000000003FFFFFFF)
  assert $ 0x000000007FFFFFFF == zzEncode64 (fromIntegral 0xFFFFFFFFC0000000)
  assert $ 0x00000000FFFFFFFE == zzEncode64 (fromIntegral 0x000000007FFFFFFF)
  assert $ 0x00000000FFFFFFFF == zzEncode64 (fromIntegral 0xFFFFFFFF80000000)
  assert $ 0xFFFFFFFFFFFFFFFE == zzEncode64 (fromIntegral 0x7FFFFFFFFFFFFFFF)
  assert $ 0xFFFFFFFFFFFFFFFF == zzEncode64 (fromIntegral 0x8000000000000000)

  assert $   0  == zzDecode64 0
  assert $ (-1) == zzDecode64 1
  assert $   1  == zzDecode64 2
  assert $ (-2) == zzDecode64 3
  assert $ fromIntegral 0x000000003FFFFFFF == zzDecode64 0x000000007FFFFFFE
  assert $ fromIntegral 0xFFFFFFFFC0000000 == zzDecode64 0x000000007FFFFFFF
  assert $ fromIntegral 0x000000007FFFFFFF == zzDecode64 0x00000000FFFFFFFE
  assert $ fromIntegral 0xFFFFFFFF80000000 == zzDecode64 0x00000000FFFFFFFF
  assert $ fromIntegral 0x7FFFFFFFFFFFFFFF == zzDecode64 0xFFFFFFFFFFFFFFFE
  assert $ fromIntegral 0x8000000000000000 == zzDecode64 0xFFFFFFFFFFFFFFFF

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
