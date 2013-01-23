{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Data.ProtocolBuffers.Message
  ( Message(..)
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
newtype Message m = Message (Maybe m)
  deriving (Eq, Foldable, Functor, NFData, Ord, Show, Traversable)

instance Monoid (Message m) where
  mempty = Message Nothing
  _ `mappend` m = m

instance Applicative Message where
  pure = Message . Just
  Message (Just f) <*> x = f <$> x
  Message Nothing  <*> _ = Message Nothing

instance Monad Message where
  return = pure
  Message (Just f) >>= x = x f
  Message Nothing  >>= _ = Message Nothing

instance Encode m => EncodeWire (Message m) where
  encodeWire t (Message m) =
    traverse_ (encodeWire t . runPut . encode) m

instance Decode m => DecodeWire (Message m) where
  decodeWire (DelimitedField _ bs) =
    case runGet decodeMessage bs of
      Right val -> pure . Message $ Just val
      Left err  -> fail $ "Embedded message decoding failed: " ++ show err
  decodeWire _ = empty
