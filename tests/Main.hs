{-# LANGUAGE DeriveGeneric #-}
import Test.Framework (defaultMain, testGroup)
import Test.Framework.Providers.HUnit
import Test.Framework.Providers.QuickCheck2 (testProperty)
import Test.QuickCheck
import Test.QuickCheck.Monadic

import GHC.Generics (Generic)

import Control.Applicative
import qualified Data.ByteString as B
import Data.ProtocolBuffers
import Data.ProtocolBuffers.Internal
import Data.Int
import Data.List
import Data.Monoid
import Data.Serialize
import Data.Word
import Data.TypeLevel.Num (D1)

main :: IO ()
main = defaultMain tests

tests =
  [ testGroup "Single Values" singleValueTests
  ]

singleValueTests =
  [ testProperty "Int32" prop_int32
  , testProperty "Int64" prop_int64
  , testProperty "Word32" prop_word32
  , testProperty "Word64" prop_word64
  ]

data OneValue a = OneValue (Required D1 (Last a))
  deriving (Eq, Generic)

instance EncodeWire a => Encode (OneValue a)
instance DecodeWire a => Decode (OneValue a)

prop_roundtrip msg = do
  let bs = runPut $ encodeMessage msg
  case runGet decodeMessage bs of
    Right msg' -> return $ msg == msg'
    Left err   -> fail err

prop_word32 = do
  val <- OneValue . putValue . Last . Just <$> arbitrary
  prop_roundtrip (val :: OneValue Word32)

prop_word64 = do
  val <- OneValue . putValue . Last . Just <$> arbitrary
  prop_roundtrip (val :: OneValue Word64)

prop_int32 = do
  val <- OneValue . putValue . Last . Just <$> arbitrary
  prop_roundtrip (val :: OneValue Int32)

prop_int64 = do
  val <- OneValue . putValue . Last . Just <$> arbitrary
  prop_roundtrip (val :: OneValue Int64)
