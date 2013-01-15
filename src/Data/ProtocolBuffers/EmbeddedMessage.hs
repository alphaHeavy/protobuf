{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Data.ProtocolBuffers.EmbeddedMessage
  ( EmbeddedMessage(..)
  ) where

import Control.Applicative
import Control.DeepSeq (NFData)
import Data.Bits
import Data.Foldable
import Data.Monoid
import Data.Serialize.Get
import Data.Serialize.Put
import Data.Traversable

import Data.ProtocolBuffers.Decode
import Data.ProtocolBuffers.Encode
import Data.ProtocolBuffers.Wire

newtype EmbeddedMessage m = EmbeddedMessage m
  deriving (Bits, Bounded, Enum, Eq, Floating, Foldable, Fractional, Functor, Integral, Monoid, NFData, Num, Ord, Real, RealFloat, RealFrac, Show, Traversable)

instance (Encode m, Decode m) => Wire (EmbeddedMessage m) where
  decodeWire (DelimitedField _ bs) =
    case runGet decodeMessage bs of
      Right val -> pure $ EmbeddedMessage val
      Left _err -> empty
  decodeWire _ = empty
  encodeWire t (EmbeddedMessage m) =
    encodeWire t . runPut $ encode m
