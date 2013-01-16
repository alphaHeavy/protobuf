{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

import Test.Framework (defaultMain, testGroup)
import Test.Framework.Providers.HUnit
import Test.Framework.Providers.QuickCheck2 (testProperty)
import Test.QuickCheck
import Test.QuickCheck.Modifiers

import GHC.Generics (Generic)

import Control.Applicative
import qualified Data.ByteString as B
import Data.ProtocolBuffers as Pb
import Data.ProtocolBuffers.Internal as Pb
import Data.Int
import Data.List
import Data.Monoid
import Data.Serialize
import Data.Word
import Data.TypeLevel.Num (Nat, reifyIntegral)

main :: IO ()
main = defaultMain tests

tests =
  [ testGroup "Single Values" singleValueTests
  ]

singleValueTests =
  [ testProperty "int32"    prop_int32
  , testProperty "int64"    prop_int64
  , testProperty "word32"   prop_word32
  , testProperty "word64"   prop_word64
  , testProperty "sint32"   prop_sint32
  , testProperty "sint64"   prop_sint64
  , testProperty "fixed32"  prop_fixed32
  , testProperty "fixed64"  prop_fixed64
  , testProperty "sfixed32" prop_sfixed32
  , testProperty "sfixed64" prop_sfixed64
  , testProperty "float"    prop_float
  , testProperty "double"   prop_double
  , testProperty "bool"     prop_bool
  ]

data OneValue n a = OneValue (Required n (Last a))
  deriving (Eq, Generic)

instance (EncodeWire a, Nat n) => Encode (OneValue n a)
instance (DecodeWire a, Nat n) => Decode (OneValue n a)

prop_roundtrip :: (Eq a, Nat n, Encode (OneValue n a), Decode (OneValue n a)) => OneValue n a -> Gen Bool
prop_roundtrip msg = do
  let bs = runPut $ encodeMessage msg
  case runGet decodeMessage bs of
    Right msg' -> return $ msg == msg'
    Left err   -> fail err

prop_reify :: forall a r . Last a -> (forall n . Nat n => OneValue n a -> Gen r) -> Gen r
prop_reify a f = do
  let g :: forall n . Nat n => n -> Gen r
      g _ = f (OneValue (putValue a) :: OneValue n a)
  -- according to https://developers.google.com/protocol-buffers/docs/proto
  -- the max is 2^^29 - 1, or 536,870,911.
  --
  -- the min is set to 0 since reifyIntegral only supports naturals, which
  -- is also recommended since these are encoded as varints which have
  -- fairly high overhead for negative tags
  n <- choose (0, 536870911)
  reifyIntegral (n :: Int32) g

prop_word32 :: Gen Bool
prop_word32 = do
  val <- Last . Just <$> arbitrary
  prop_reify (val :: Last Word32) prop_roundtrip

prop_word64 :: Gen Bool
prop_word64 = do
  val <- Last . Just <$> arbitrary
  prop_reify (val :: Last Word64) prop_roundtrip

prop_int32 :: Gen Bool
prop_int32 = do
  val <- Last . Just <$> arbitrary
  prop_reify (val :: Last Int32) prop_roundtrip

prop_int64 :: Gen Bool
prop_int64 = do
  val <- Last . Just <$> arbitrary
  prop_reify (val :: Last Int64) prop_roundtrip

prop_sint32 :: Gen Bool
prop_sint32 = do
  val <- Last . Just . Signed <$> arbitrary
  prop_reify (val :: Last (Signed Int32)) prop_roundtrip

prop_sint64 :: Gen Bool
prop_sint64 = do
  val <- Last . Just . Signed <$> arbitrary
  prop_reify (val :: Last (Signed Int64)) prop_roundtrip

prop_fixed32 :: Gen Bool
prop_fixed32 = do
  val <- Last . Just . Pb.Fixed <$> arbitrary
  prop_reify (val :: Last (Pb.Fixed Int32)) prop_roundtrip

prop_fixed64 :: Gen Bool
prop_fixed64 = do
  val <- Last . Just . Pb.Fixed <$> arbitrary
  prop_reify (val :: Last (Pb.Fixed Int64)) prop_roundtrip

prop_sfixed32 :: Gen Bool
prop_sfixed32 = do
  val <- Last . Just . Pb.Fixed <$> arbitrary
  prop_reify (val :: Last (Pb.Fixed Word32)) prop_roundtrip

prop_sfixed64 :: Gen Bool
prop_sfixed64 = do
  val <- Last . Just . Pb.Fixed <$> arbitrary
  prop_reify (val :: Last (Pb.Fixed Word64)) prop_roundtrip

prop_float :: Gen Bool
prop_float = do
  val <- Last . Just <$> arbitrary
  prop_reify (val :: Last Float) prop_roundtrip

prop_double :: Gen Bool
prop_double = do
  val <- Last . Just <$> arbitrary
  prop_reify (val :: Last Double) prop_roundtrip

prop_bool :: Gen Bool
prop_bool = do
  val <- Last . Just <$> arbitrary
  prop_reify (val :: Last Bool) prop_roundtrip
