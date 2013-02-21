{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE UndecidableInstances #-}

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

import GHC.Generics

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
newtype Message m = Message {runMessage :: m}
  deriving (Eq, Foldable, Functor, NFData, Ord, Show, Traversable)

instance (Generic m, GMessageMonoid (Rep m)) => Monoid (Message m) where
  mempty = Message . to $ gmempty
  Message x `mappend` Message y = Message . to $ gmappend (from x) (from y)

class GMessageMonoid (f :: * -> *) where
  gmempty :: f a
  gmappend :: f a -> f a -> f a

instance GMessageMonoid f => GMessageMonoid (M1 i c f) where
  gmempty = M1 $ gmempty
  gmappend (M1 x) (M1 y) = M1 (gmappend x y)

instance (GMessageMonoid x, GMessageMonoid y) => GMessageMonoid (x :*: y) where
  gmempty = gmempty :*: gmempty
  gmappend (x1 :*: x2) (y1 :*: y2) = gmappend x1 y1 :*: gmappend x2 y2

instance (Monoid c) => GMessageMonoid (K1 i c) where
  gmempty = K1 mempty
  gmappend (K1 x) (K1 y) = K1 $ mappend x y

type instance Optional n (Message a) = Field n (OptionalField (Message (Maybe a)))
type instance Required n (Message a) = Field n (RequiredField (Message a))

instance Encode m => EncodeWire (Message m) where
  encodeWire t =
    encodeWire t . runPut . encode . runMessage

instance Decode m => DecodeWire (Message m) where
  decodeWire (DelimitedField _ bs) =
    case runGet decodeMessage bs of
      Right val -> pure $ Message val
      Left err  -> fail $ "Embedded message decoding failed: " ++ show err
  decodeWire _ = empty

instance HasField (Field n (RequiredField (Message a))) where
  type FieldType (Field n (RequiredField (Message a))) = a
  getField = runMessage . runRequired . runField
  putField = Field . Required . Message

instance HasField (Field n (OptionalField (Message (Maybe a)))) where
  type FieldType (Field n (OptionalField (Message (Maybe a)))) = Maybe a
  getField = runMessage . runOptional . runField
  putField = Field . Optional . Message
