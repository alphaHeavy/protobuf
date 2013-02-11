{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeSynonymInstances #-}

module Data.ProtocolBuffers.Message
  ( Message(..)
  , GetMessage(..)
  ) where

import Control.Applicative
import Control.DeepSeq (NFData)
import Data.Foldable
import Data.Maybe (fromMaybe)
import Data.Monoid
import Data.Serialize.Get
import Data.Serialize.Put
import Data.Traversable

import Data.ProtocolBuffers.Decode
import Data.ProtocolBuffers.Encode
import Data.ProtocolBuffers.Types
import Data.ProtocolBuffers.Wire

-- |
-- The way to embed a message within another message.
-- These embedded messages are stored as length-delimited fields.
--
-- For example:
--
-- @
--data Inner = Inner
--   { innerField :: 'Data.ProtocolBuffers.Required' 'Data.TypeLevel.D1' ('Data.Monoid.Last' 'Data.Int.Int64')
--   } deriving ('GHC.Generics.Generic', 'Prelude.Show')
--
-- instance 'Encode' Inner
--instance 'Decode' Inner
--
-- data Outer = Outer
--   { outerField :: 'Data.ProtocolBuffers.Required' 'Data.TypeLevel.D1' ('Data.ProtocolBuffers.Message' Inner)
--   } deriving ('GHC.Generics.Generic', 'Prelude.Show')
--
-- instance 'Encode' Outer
--instance 'Decode' Outer
-- @
--
newtype Message m = Message {runMessage :: Maybe m}
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


-- | Functions for wrapping and unwrapping record fields that use the 'Message' type
class GetMessage a where
  type GetMessageType a :: *
  getMessage :: a -> GetMessageType a
  putMessage :: GetMessageType a -> a
  message :: Functor f => (GetMessageType a -> f (GetMessageType a)) -> a -> f a
  message f = fmap putMessage . f . getMessage

instance GetMessage (Required n (Message a)) where
  type GetMessageType (Required n (Message a)) = a
  getMessage = fromMaybe (error "Required' getValue") . runMessage . getValue
  putMessage = putValue . Message . Just

instance GetMessage (Optional n (Message a)) where
  type GetMessageType (Optional n (Message a)) = Maybe a
  getMessage = runMessage . getValue
  putMessage = putValue . Message
