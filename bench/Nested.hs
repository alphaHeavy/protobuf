{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE DeriveGeneric        #-}
{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE StandaloneDeriving #-}
module Nested where

import           Control.DeepSeq             (NFData)
import qualified Data.ByteString.Lazy        as LBS
import           Data.ProtocolBuffers
import           Data.ProtocolBuffers.Internal
import           Data.Text                   (Text, pack)
import           GHC.Generics                (Generic, K1, R)

import           Data

data MapEntry k v = MapEntry {
  key   :: Required 1 k,
  value :: Required 2 v
  } deriving (Generic)

type Map n k v = Repeated n (Message (MapEntry k v))

instance (GEncode (K1 R (Required 2 v)), GEncode (K1 R (Required 1 k))) => Encode (MapEntry k v) where
instance (GDecode (K1 R (Required 2 v)), GDecode (K1 R (Required 1 k))) => Decode (MapEntry k v) where
instance (NFData (Required 1 k), NFData (Required 2 v)) => NFData (MapEntry k v)

deriving instance (Eq (Required 2 v), Eq (Required 1 k)) => Eq (MapEntry k v)
deriving instance (Show (Required 2 v), Show (Required 1 k)) => Show (MapEntry k v)

data A = A {
  _1 :: Required 1 (Value Float),
  _2 :: Required 2 (Value Float),
  _3 :: Required 3 (Value Float),
  _4 :: Required 4 (Value Float)
  } deriving (Generic, Eq, Show)

instance Encode A
instance Decode A
instance NFData A

data B = B {
  as :: Map 1 (Value Text) (Message A)
} deriving (Generic, Eq, Show)

instance Encode B
instance Decode B
instance NFData B

data C = C {
  bs :: Map 1 (Value Text) (Message B)
} deriving (Generic, Eq, Show)

instance Encode C
instance Decode C
instance NFData C

decNested :: LBS.ByteString -> C
decNested = dec

nested :: Int -> Int -> C
nested i j = C (putField bs')
  where
    show' = pack . show
    mkMap (k, v) = MapEntry (putField k) (putField v)
    bs' = map mkMap $ zip (map show' [0..i]) $ replicate i b
    b :: B
    b = B $ putField $ map mkMap $ zip (map show' [0..j]) $ replicate j a
    a :: A
    a = A (putField 0.1) (putField 0.2) (putField 0.3) (putField 0.4)
