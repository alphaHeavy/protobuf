{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

import Test.Framework (defaultMain, testGroup)
import Test.Framework.Providers.HUnit
import Test.Framework.Providers.QuickCheck2 (testProperty)
import Test.QuickCheck
import Test.QuickCheck.Modifiers
import Test.QuickCheck.Property

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
  [ testGroup "Required Single Values" requiredSingleValueTests
  , testGroup "Optional Single Values" optionalSingleValueTests
  ]

requiredSingleValueTests =
  [ testProperty "int32"    prop_req_int32
  , testProperty "int64"    prop_req_int64
  , testProperty "word32"   prop_req_word32
  , testProperty "word64"   prop_req_word64
  , testProperty "sint32"   prop_req_sint32
  , testProperty "sint64"   prop_req_sint64
  , testProperty "fixed32"  prop_req_fixed32
  , testProperty "fixed64"  prop_req_fixed64
  , testProperty "sfixed32" prop_req_sfixed32
  , testProperty "sfixed64" prop_req_sfixed64
  , testProperty "float"    prop_req_float
  , testProperty "double"   prop_req_double
  , testProperty "bool"     prop_req_bool
  ]

optionalSingleValueTests =
  [ testProperty "int32"    prop_opt_int32
  , testProperty "int64"    prop_opt_int64
  , testProperty "word32"   prop_opt_word32
  , testProperty "word64"   prop_opt_word64
  , testProperty "sint32"   prop_opt_sint32
  , testProperty "sint64"   prop_opt_sint64
  , testProperty "fixed32"  prop_opt_fixed32
  , testProperty "fixed64"  prop_opt_fixed64
  , testProperty "sfixed32" prop_opt_sfixed32
  , testProperty "sfixed64" prop_opt_sfixed64
  , testProperty "float"    prop_opt_float
  , testProperty "double"   prop_opt_double
  , testProperty "bool"     prop_opt_bool
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

prop_req_roundtrip :: (Eq a, Nat n, Encode (RequiredValue n a), Decode (RequiredValue n a)) => RequiredValue n a -> Gen Bool
prop_req_roundtrip msg = do
  let bs = runPut $ encodeMessage msg
  case runGet decodeMessage bs of
    Right msg' -> return $ msg == msg'
    Left err   -> fail err

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

prop_req_word32 :: Gen Bool
prop_req_word32 = do
  val <- Last . Just <$> arbitrary
  prop_req_reify (val :: Last Word32) prop_req_roundtrip

prop_req_word64 :: Gen Bool
prop_req_word64 = do
  val <- Last . Just <$> arbitrary
  prop_req_reify (val :: Last Word64) prop_req_roundtrip

prop_req_int32 :: Gen Bool
prop_req_int32 = do
  val <- Last . Just <$> arbitrary
  prop_req_reify (val :: Last Int32) prop_req_roundtrip

prop_req_int64 :: Gen Bool
prop_req_int64 = do
  val <- Last . Just <$> arbitrary
  prop_req_reify (val :: Last Int64) prop_req_roundtrip

prop_req_sint32 :: Gen Bool
prop_req_sint32 = do
  val <- Last . Just . Signed <$> arbitrary
  prop_req_reify (val :: Last (Signed Int32)) prop_req_roundtrip

prop_req_sint64 :: Gen Bool
prop_req_sint64 = do
  val <- Last . Just . Signed <$> arbitrary
  prop_req_reify (val :: Last (Signed Int64)) prop_req_roundtrip

prop_req_fixed32 :: Gen Bool
prop_req_fixed32 = do
  val <- Last . Just . Pb.Fixed <$> arbitrary
  prop_req_reify (val :: Last (Pb.Fixed Int32)) prop_req_roundtrip

prop_req_fixed64 :: Gen Bool
prop_req_fixed64 = do
  val <- Last . Just . Pb.Fixed <$> arbitrary
  prop_req_reify (val :: Last (Pb.Fixed Int64)) prop_req_roundtrip

prop_req_sfixed32 :: Gen Bool
prop_req_sfixed32 = do
  val <- Last . Just . Pb.Fixed <$> arbitrary
  prop_req_reify (val :: Last (Pb.Fixed Word32)) prop_req_roundtrip

prop_req_sfixed64 :: Gen Bool
prop_req_sfixed64 = do
  val <- Last . Just . Pb.Fixed <$> arbitrary
  prop_req_reify (val :: Last (Pb.Fixed Word64)) prop_req_roundtrip

prop_req_float :: Gen Bool
prop_req_float = do
  val <- Last . Just <$> arbitrary
  prop_req_reify (val :: Last Float) prop_req_roundtrip

prop_req_double :: Gen Bool
prop_req_double = do
  val <- Last . Just <$> arbitrary
  prop_req_reify (val :: Last Double) prop_req_roundtrip

prop_req_bool :: Gen Bool
prop_req_bool = do
  val <- Last . Just <$> arbitrary
  prop_req_reify (val :: Last Bool) prop_req_roundtrip



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

prop_opt_word32 :: Gen Property
prop_opt_word32 = do
  val <- Last . Just <$> arbitrary
  full <- prop_opt_reify (val :: Last Word32) prop_opt_roundtrip
  empty <- prop_opt_reify (Last Nothing :: Last Word32) prop_opt_roundtrip
  -- return $ full .&&. once empty
  return $ full .&&. empty

prop_opt_word64 :: Gen Property
prop_opt_word64 = do
  val <- Last . Just <$> arbitrary
  full <- prop_opt_reify (val :: Last Word64) prop_opt_roundtrip
  empty <- prop_opt_reify (Last Nothing :: Last Word64) prop_opt_roundtrip
  return $ full .&&. empty

prop_opt_int32 :: Gen Property
prop_opt_int32 = do
  val <- Last . Just <$> arbitrary
  full <- prop_opt_reify (val :: Last Int32) prop_opt_roundtrip
  empty <- prop_opt_reify (Last Nothing :: Last Int32) prop_opt_roundtrip
  return $ full .&&. empty

prop_opt_int64 :: Gen Property
prop_opt_int64 = do
  val <- Last . Just <$> arbitrary
  full <- prop_opt_reify (val :: Last Int64) prop_opt_roundtrip
  empty <- prop_opt_reify (Last Nothing :: Last Int64) prop_opt_roundtrip
  return $ full .&&. empty

prop_opt_sint32 :: Gen Property
prop_opt_sint32 = do
  val <- Last . Just . Signed <$> arbitrary
  full <- prop_opt_reify (val :: Last (Signed Int32)) prop_opt_roundtrip
  empty <- prop_opt_reify (Last Nothing :: Last (Signed Int32)) prop_opt_roundtrip
  return $ full .&&. empty

prop_opt_sint64 :: Gen Property
prop_opt_sint64 = do
  val <- Last . Just . Signed <$> arbitrary
  full <- prop_opt_reify (val :: Last (Signed Int64)) prop_opt_roundtrip
  empty <- prop_opt_reify (Last Nothing :: Last (Signed Int64)) prop_opt_roundtrip
  return $ full .&&. empty

prop_opt_fixed32 :: Gen Property
prop_opt_fixed32 = do
  val <- Last . Just . Pb.Fixed <$> arbitrary
  full <- prop_opt_reify (val :: Last (Pb.Fixed Int32)) prop_opt_roundtrip
  empty <- prop_opt_reify (Last Nothing :: Last (Pb.Fixed Int32)) prop_opt_roundtrip
  return $ full .&&. empty

prop_opt_fixed64 :: Gen Property
prop_opt_fixed64 = do
  val <- Last . Just . Pb.Fixed <$> arbitrary
  full <- prop_opt_reify (val :: Last (Pb.Fixed Int64)) prop_opt_roundtrip
  empty <- prop_opt_reify (Last Nothing :: Last (Pb.Fixed Int64)) prop_opt_roundtrip
  return $ full .&&. empty

prop_opt_sfixed32 :: Gen Property
prop_opt_sfixed32 = do
  val <- Last . Just . Pb.Fixed <$> arbitrary
  full <- prop_opt_reify (val :: Last (Pb.Fixed Word32)) prop_opt_roundtrip
  empty <- prop_opt_reify (Last Nothing :: Last (Pb.Fixed Word32)) prop_opt_roundtrip
  return $ full .&&. empty

prop_opt_sfixed64 :: Gen Property
prop_opt_sfixed64 = do
  val <- Last . Just . Pb.Fixed <$> arbitrary
  full <- prop_opt_reify (val :: Last (Pb.Fixed Word64)) prop_opt_roundtrip
  empty <- prop_opt_reify (Last Nothing :: Last (Pb.Fixed Word64)) prop_opt_roundtrip
  return $ full .&&. empty

prop_opt_float :: Gen Property
prop_opt_float = do
  val <- Last . Just <$> arbitrary
  full <- prop_opt_reify (val :: Last Float) prop_opt_roundtrip
  empty <- prop_opt_reify (Last Nothing :: Last Float) prop_opt_roundtrip
  return $ full .&&. empty

prop_opt_double :: Gen Property
prop_opt_double = do
  val <- Last . Just <$> arbitrary
  full <- prop_opt_reify (val :: Last Double) prop_opt_roundtrip
  empty <- prop_opt_reify (Last Nothing :: Last Double) prop_opt_roundtrip
  return $ full .&&. empty

prop_opt_bool :: Gen Property
prop_opt_bool = do
  val <- Last . Just <$> arbitrary
  full <- prop_opt_reify (val :: Last Bool) prop_opt_roundtrip
  empty <- prop_opt_reify (Last Nothing :: Last Bool) prop_opt_roundtrip
  return $ full .&&. empty
