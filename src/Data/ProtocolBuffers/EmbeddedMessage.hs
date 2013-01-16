{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Data.ProtocolBuffers.EmbeddedMessage
  ( EmbeddedMessage(..)
  ) where

import Control.Applicative
import Control.DeepSeq (NFData)
import Data.Foldable
import Data.Monoid
import Data.Serialize.Get
import Data.Serialize.Put
import Data.Traversable

import Data.ProtocolBuffers.Decode
import Data.ProtocolBuffers.Encode
import Data.ProtocolBuffers.Wire

-- |
-- A newtype wrapper used to distinguish encoded messages from other field types.
-- These messages are stored as delimited fields.
newtype EmbeddedMessage m = EmbeddedMessage (Maybe m)
  deriving (Eq, Foldable, Functor, NFData, Ord, Show, Traversable)

instance Monoid (EmbeddedMessage m) where
  mempty = EmbeddedMessage Nothing
  _ `mappend` m = m

instance Applicative EmbeddedMessage where
  pure = EmbeddedMessage . Just
  EmbeddedMessage (Just f) <*> x = f <$> x
  EmbeddedMessage Nothing  <*> _ = EmbeddedMessage Nothing

instance Monad EmbeddedMessage where
  return = pure
  EmbeddedMessage (Just f) >>= x = x f
  EmbeddedMessage Nothing  >>= _ = EmbeddedMessage Nothing

instance Encode m => EncodeWire (EmbeddedMessage m) where
  encodeWire t (EmbeddedMessage m) =
    traverse_ (encodeWire t . runPut . encode) m

instance Decode m => DecodeWire (EmbeddedMessage m) where
  decodeWire (DelimitedField _ bs) =
    case runGet decodeMessage bs of
      Right val -> pure . EmbeddedMessage $ Just val
      Left err  -> fail $ "Embedded message decoding failed: " ++ show err
  decodeWire _ = empty
