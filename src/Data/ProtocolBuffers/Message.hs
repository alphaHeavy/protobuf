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
import Control.DeepSeq (NFData(..))
import Data.Foldable
import Data.Monoid
import Data.Serialize.Get
import Data.Serialize.Put
import Data.Traversable

import GHC.Generics
import GHC.TypeLits

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
--   { innerField :: 'Data.ProtocolBuffers.Required' 'Data.TypeLevel.D1' ('Data.ProtocolBuffers.Value' 'Data.Int.Int64')
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
-- It's worth noting that @ 'Message' a @ is a 'Monoid' and 'NFData' instance. The 'Monoid' behavior models
-- that of the Protocol Buffers documentation, effectively 'Data.Monoid.Last'. It's done with a fairly big hammer
-- and it isn't possible to override this behavior. This can cause some less-obvious compile errors for
-- paramterized 'Message' types:
--
-- @
--data Inner = Inner{inner :: 'Required' 'D2' ('Value' 'Float')} deriving ('Generic', 'Show')
--instance 'Encode' Inner
--instance 'Decode' Inner
--
--data Outer a = Outer{outer :: 'Required' 'D3' ('Message' a)} deriving ('Generic', 'Show')
--instance 'Encode' a => 'Encode' (Outer a)
--instance 'Decode' a => 'Decode' (Outer a)
-- @
--
-- This fails because 'Decode' needs to know that the message can be merged. The resulting error
-- implies that you may want to add a constraint to the internal 'GMessageMonoid' class:
--
-- @
-- \/tmp\/tst.hs:18:10:
--   Could not deduce (protobuf-0.1:'Data.ProtocolBuffers.Message.GMessageMonoid' ('Rep' a))
--     arising from a use of `protobuf-0.1: 'Data.ProtocolBuffers.Decode' .$gdmdecode'
--   from the context ('Decode' a)
--     bound by the instance declaration at \/tmp\/tst.hs:18:10-39
--   Possible fix:
--     add an instance declaration for
--     (protobuf-0.1:'Data.ProtocolBuffers.Message.GMessageMonoid' ('Rep' a))
--   In the expression:
--     (protobuf-0.1:'Data.ProtocolBuffers.Decode'.$gdmdecode)
--   In an equation for `decode':
--       decode = (protobuf-0.1:'Data.ProtocolBuffers.Decode' .$gdmdecode)
--   In the instance declaration for `'Decode' (Outer a)'
-- @
--
-- The correct fix is to add the 'Monoid' constraint for the message:
--
-- @
-- - instance ('Encode' a) => 'Decode' (Outer a)
-- + instance ('Monoid' ('Message' a), 'Decode' a) => 'Decode' (Outer a)
-- @
--
newtype Message m = Message {runMessage :: m}
  deriving (Eq, Foldable, Functor, Ord, Show, Traversable)

instance (Generic m, GMessageMonoid (Rep m)) => Monoid (Message m) where
  mempty = Message . to $ gmempty
  Message x `mappend` Message y = Message . to $ gmappend (from x) (from y)

instance (Decode a, Monoid (Message a), KnownNat n) => GDecode (K1 i (Field n (RequiredField (Always (Message a))))) where
  gdecode = fieldDecode (Required . Always)

instance (Decode a, Monoid (Message a), KnownNat n) => GDecode (K1 i (Field n (OptionalField (Maybe (Message a))))) where
  gdecode msg = fieldDecode (Optional . Just) msg <|> pure (K1 mempty)

class GMessageMonoid (f :: * -> *) where
  gmempty :: f a
  gmappend :: f a -> f a -> f a

instance GMessageMonoid f => GMessageMonoid (M1 i c f) where
  gmempty = M1 gmempty
  gmappend (M1 x) (M1 y) = M1 (gmappend x y)

instance (GMessageMonoid x, GMessageMonoid y) => GMessageMonoid (x :*: y) where
  gmempty = gmempty :*: gmempty
  gmappend (x1 :*: x2) (y1 :*: y2) = gmappend x1 y1 :*: gmappend x2 y2

instance (GMessageMonoid x, GMessageMonoid y) => GMessageMonoid (x :+: y) where
  gmempty = L1 gmempty
  gmappend _ = id

instance (Monoid c) => GMessageMonoid (K1 i c) where
  gmempty = K1 mempty
  gmappend (K1 x) (K1 y) = K1 $ mappend x y

instance GMessageMonoid U1 where
  gmempty = U1
  gmappend _ = id

instance (Generic m, GMessageNFData (Rep m)) => NFData (Message m) where
  rnf = grnf . from . runMessage

class GMessageNFData f where
  grnf :: f a -> ()

instance GMessageNFData f => GMessageNFData (M1 i c f) where
  grnf = grnf . unM1

instance (GMessageNFData x, GMessageNFData y) => GMessageNFData (x :*: y) where
  grnf (x :*: y) = grnf x `seq` grnf y

instance (GMessageNFData x, GMessageNFData y) => GMessageNFData (x :+: y) where
  grnf (L1 x) = grnf x
  grnf (R1 y) = grnf y

instance NFData c => GMessageNFData (K1 i c) where
  grnf = rnf . unK1

instance GMessageNFData U1 where
  grnf U1 = ()

type instance Optional n (Message a) = Field n (OptionalField (Maybe (Message a)))
type instance Required n (Message a) = Field n (RequiredField (Always (Message a)))

instance (Foldable f, Encode m) => EncodeWire (f (Message m)) where
  encodeWire t =
    traverse_ (encodeWire t . runPut . encode . runMessage)

instance Decode m => DecodeWire (Message m) where
  decodeWire (DelimitedField _ bs) =
    case runGet decodeMessage bs of
      Right val -> pure $ Message val
      Left err  -> fail $ "Embedded message decoding failed: " ++ show err
  decodeWire _ = empty

-- | Iso: @ 'FieldType' ('Required' n ('Message' a)) = a @
instance HasField (Field n (RequiredField (Always (Message a)))) where
  type FieldType (Field n (RequiredField (Always (Message a)))) = a
  getField = runMessage . runAlways. runRequired . runField
  putField = Field . Required . Always . Message

-- | Iso: @ 'FieldType' ('Optional' n ('Message' a)) = 'Maybe' a @
instance HasField (Field n (OptionalField (Maybe (Message a)))) where
  type FieldType (Field n (OptionalField (Maybe (Message a)))) = Maybe a
  getField = fmap runMessage . runOptional . runField
  putField = Field . Optional . fmap Message

-- | Iso: @ 'FieldType' ('Repeated' n ('Message' a)) = [a] @
instance HasField (Field n (RepeatedField [Message a])) where
  type FieldType (Field n (RepeatedField [Message a])) = [a]
  getField = fmap runMessage . runRepeated . runField
  putField = Field . Repeated . fmap Message
