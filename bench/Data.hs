{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
module Data where

import Control.DeepSeq         (NFData)
import Data.ProtocolBuffers
import GHC.Generics(Generic)
import Data.Binary.Builder.Sized
import Data.Binary.Get
import Data.ByteString(ByteString)
import qualified Data.ByteString.Lazy as LBS

data Burst = Burst {
    frames :: Repeated 1 (Value ByteString)
} deriving (Generic, Eq, Show)

instance NFData Burst
instance Encode Burst
instance Decode Burst

burst :: Int -> Burst
burst = Burst . putField . flip replicate "abcd"

encodeBurst :: Int -> LBS.ByteString
encodeBurst = enc . burst

enc :: Encode a => a -> LBS.ByteString
enc = toLazyByteString . encodeMessage

dec :: Decode a => LBS.ByteString -> a
dec = runGet decodeMessage

decBurst :: LBS.ByteString -> Burst
decBurst = dec
