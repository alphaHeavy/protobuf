{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeSynonymInstances #-}

import Test.Framework (defaultMain, testGroup)
import Test.Framework.Providers.HUnit
import Test.Framework.Providers.QuickCheck2 (testProperty)
import Test.QuickCheck
import Test.QuickCheck.Modifiers
import Test.QuickCheck.Property

import GHC.Generics (Generic)

import Control.Applicative
import Control.Monad
import Control.Monad.Identity
import qualified Data.ByteString as B
import Data.ProtocolBuffers as Pb
import Data.ProtocolBuffers.Internal as Pb
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HashMap
import Data.Int
import Data.List
import Data.Monoid
import Data.Serialize (runGet, runPut)
import Data.Proxy
import Data.Tagged
import Data.Typeable
import Data.Word
import Data.TypeLevel.Num (Nat, reifyIntegral)

main :: IO ()
main = defaultMain tests

tests =
  [ testGroup "Primitive Wire" primitiveWireTests
  , testGroup "Packed Wire" packedWireTests
  , testGroup "Required Single Values" requiredSingleValueTests
  , testGroup "Optional Single Values" optionalSingleValueTests
  , testGroup "Tags Out of Range" tagsOutOfRangeTests
  ]

primitiveWireTests =
  [ testProperty "int32"    (prop_wire (Proxy :: Proxy Int32))
  , testProperty "int64"    (prop_wire (Proxy :: Proxy Int64))
  , testProperty "word32"   (prop_wire (Proxy :: Proxy Word32))
  , testProperty "word64"   (prop_wire (Proxy :: Proxy Word64))
  , testProperty "sint32"   (prop_wire (Proxy :: Proxy (Signed Int32)))
  , testProperty "sint64"   (prop_wire (Proxy :: Proxy (Signed Int64)))
  , testProperty "fixed32"  (prop_wire (Proxy :: Proxy (Pb.Fixed Word32)))
  , testProperty "fixed64"  (prop_wire (Proxy :: Proxy (Pb.Fixed Word64)))
  , testProperty "sfixed32" (prop_wire (Proxy :: Proxy (Pb.Fixed Int32)))
  , testProperty "sfixed64" (prop_wire (Proxy :: Proxy (Pb.Fixed Int64)))
  , testProperty "float"    (prop_wire (Proxy :: Proxy Float))
  , testProperty "double"   (prop_wire (Proxy :: Proxy Double))
  , testProperty "bool"     (prop_wire (Proxy :: Proxy Bool))
  ]

packedWireTests =
  [ testProperty "int32"    (prop_wire (Proxy :: Proxy (PackedList Int32)))
  , testProperty "int64"    (prop_wire (Proxy :: Proxy (PackedList Int64)))
  , testProperty "word32"   (prop_wire (Proxy :: Proxy (PackedList Word32)))
  , testProperty "word64"   (prop_wire (Proxy :: Proxy (PackedList Word64)))
  , testProperty "sint32"   (prop_wire (Proxy :: Proxy (PackedList (Signed Int32))))
  , testProperty "sint64"   (prop_wire (Proxy :: Proxy (PackedList (Signed Int64))))
  ]

requiredSingleValueTests =
  [ testProperty "int32"    (prop_req (Proxy :: Proxy Int32))
  , testProperty "int64"    (prop_req (Proxy :: Proxy Int64))
  , testProperty "word32"   (prop_req (Proxy :: Proxy Word32))
  , testProperty "word64"   (prop_req (Proxy :: Proxy Word64))
  , testProperty "sint32"   (prop_req (Proxy :: Proxy (Signed Int32)))
  , testProperty "sint64"   (prop_req (Proxy :: Proxy (Signed Int64)))
  , testProperty "fixed32"  (prop_req (Proxy :: Proxy (Pb.Fixed Word32)))
  , testProperty "fixed64"  (prop_req (Proxy :: Proxy (Pb.Fixed Word64)))
  , testProperty "sfixed32" (prop_req (Proxy :: Proxy (Pb.Fixed Int32)))
  , testProperty "sfixed64" (prop_req (Proxy :: Proxy (Pb.Fixed Int64)))
  , testProperty "float"    (prop_req (Proxy :: Proxy Float))
  , testProperty "double"   (prop_req (Proxy :: Proxy Double))
  , testProperty "bool"     (prop_req (Proxy :: Proxy Bool))
  ]

optionalSingleValueTests =
  [ testProperty "int32"    (prop_opt (Proxy :: Proxy Int32))
  , testProperty "int64"    (prop_opt (Proxy :: Proxy Int64))
  , testProperty "word32"   (prop_opt (Proxy :: Proxy Word32))
  , testProperty "word64"   (prop_opt (Proxy :: Proxy Word64))
  , testProperty "sint32"   (prop_opt (Proxy :: Proxy (Signed Int32)))
  , testProperty "sint64"   (prop_opt (Proxy :: Proxy (Signed Int64)))
  , testProperty "fixed32"  (prop_opt (Proxy :: Proxy (Pb.Fixed Word32)))
  , testProperty "fixed64"  (prop_opt (Proxy :: Proxy (Pb.Fixed Word64)))
  , testProperty "sfixed32" (prop_opt (Proxy :: Proxy (Pb.Fixed Int32)))
  , testProperty "sfixed64" (prop_opt (Proxy :: Proxy (Pb.Fixed Int64)))
  , testProperty "float"    (prop_opt (Proxy :: Proxy Float))
  , testProperty "double"   (prop_opt (Proxy :: Proxy Double))
  , testProperty "bool"     (prop_opt (Proxy :: Proxy Bool))
  ]

tagsOutOfRangeTests =
  [ testProperty "word32" prop_req_word32_out_of_range
  ]

instance Arbitrary a => Arbitrary (Required n a) where
  arbitrary = putValue <$> arbitrary
  -- shrink = fmap shrink

instance Arbitrary a => Arbitrary (Optional n a) where
  arbitrary = putValue <$> arbitrary
  -- shrink = fmap shrink

instance Arbitrary a => Arbitrary (Repeated n a) where
  arbitrary = putValue <$> listOf1 arbitrary
  -- shrink = fmap shrink

instance Arbitrary a => Arbitrary (Packed n a) where
  arbitrary = putValue <$> listOf1 arbitrary
  -- shrink = fmap shrink

instance Arbitrary a => Arbitrary (PackedList a) where
  arbitrary = PackedList <$> listOf1 arbitrary
  -- shrink = fmap shrink

instance Arbitrary a => Arbitrary (Signed a) where
  arbitrary = Signed <$> arbitrary

instance Arbitrary a => Arbitrary (Pb.Fixed a) where
  arbitrary = Pb.Fixed <$> arbitrary

instance Arbitrary Field where
  arbitrary = do
    tag <- choose (0, 536870912)
    oneof
      [ VarintField tag             <$> arbitrary
      , Fixed64Field tag            <$> arbitrary
      , DelimitedField tag . B.pack <$> arbitrary
      , Fixed32Field tag            <$> arbitrary
      ]

data RequiredValue n a = RequiredValue (Required n (Last a))
  deriving (Eq, Generic)

instance (EncodeWire a, Nat n) => Encode (RequiredValue n a)
instance (DecodeWire a, Nat n) => Decode (RequiredValue n a)

data OptionalValue n a = OptionalValue (Optional n (Last a))
  deriving (Eq, Generic)

instance (EncodeWire a, Nat n) => Encode (OptionalValue n a)
instance (DecodeWire a, Nat n) => Decode (OptionalValue n a)

newtype One a = One a deriving (Eq, Generic, Encode, Decode)

prop_wire :: forall a . (Eq a, Arbitrary a, EncodeWire a, DecodeWire a, Typeable a) => Proxy a -> Property
prop_wire _ = label ("prop_wire :: " ++ show (typeOf (undefined :: a))) $ do
  tag <- choose (0, 536870912)
  val <- arbitrary
  let bs = runPut (encodeWire tag (val :: a))
      dec = do
        field <- getField
        guard $ tag == fieldTag field
        decodeWire field
  case runGet dec bs of
    Right val' -> return $ val == val'
    Left err   -> fail err

prop_req_roundtrip :: (Eq a, Nat n, Encode (RequiredValue n a), Decode (RequiredValue n a)) => RequiredValue n a -> Gen Bool
prop_req_roundtrip msg = do
  let bs = runPut $ encodeMessage msg
  case runGet decodeMessage bs of
    Right msg' -> return $ msg == msg'
    Left err   -> fail err

prop_req_reify_out_of_range :: forall a r . Last a -> (forall n . Nat n => RequiredValue n a -> Gen r) -> Gen r
prop_req_reify_out_of_range a f = do
  let g :: forall n . Nat n => n -> Gen r
      g _ = f (RequiredValue (putValue a) :: RequiredValue n a)
  -- according to https://developers.google.com/protocol-buffers/docs/proto
  -- the max is 2^^29 - 1, or 536,870,911.
  --
  -- the min is set to 0 since reifyIntegral only supports naturals, which
  -- is also recommended since these are encoded as varints which have
  -- fairly high overhead for negative tags
  n <- choose (536870912, maxBound)
  reifyIntegral (n :: Int32) g

prop_req_reify :: forall a r . Last a -> (forall n . Nat n => RequiredValue n a -> Gen r) -> Gen r
prop_req_reify a f = do
  let g :: forall n . Nat n => n -> Gen r
      g _ = f (RequiredValue (putValue a) :: RequiredValue n a)
  -- according to https://developers.google.com/protocol-buffers/docs/proto
  -- the max is 2^^29 - 1, or 536,870,911.
  --
  -- the min is set to 0 since reifyIntegral only supports naturals, which
  -- is also recommended since these are encoded as varints which have
  -- fairly high overhead for negative tags
  n <- choose (0, 536870911)
  reifyIntegral (n :: Int32) g

prop_req_word32_out_of_range :: Property
prop_req_word32_out_of_range = expectFailure $ do
  val <- Last . Just <$> arbitrary
  prop_req_reify_out_of_range (val :: Last Word32) prop_req_roundtrip

prop_req :: forall a . (Arbitrary a, Eq a, EncodeWire a, DecodeWire a, Typeable a) => Proxy a -> Property
prop_req _ = label ("prop_req :: " ++ show (typeOf (undefined :: a))) $ do
  val <- Last . Just <$> arbitrary
  prop_req_reify (val :: Last a) prop_req_roundtrip

prop_opt_roundtrip :: (Eq a, Nat n, Encode (OptionalValue n a), Decode (OptionalValue n a)) => OptionalValue n a -> Gen Bool
prop_opt_roundtrip msg = do
  let bs = runPut $ encodeMessage msg
  case runGet decodeMessage bs of
    Right msg' -> return $ msg == msg'
    Left err   -> fail err

prop_opt_reify :: forall a r . Last a -> (forall n . Nat n => OptionalValue n a -> Gen r) -> Gen r
prop_opt_reify a f = do
  let g :: forall n . Nat n => n -> Gen r
      g _ = f (OptionalValue (putValue a) :: OptionalValue n a)
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
  val <- Last <$> arbitrary
  prop_opt_reify (val :: Last a) prop_opt_roundtrip
