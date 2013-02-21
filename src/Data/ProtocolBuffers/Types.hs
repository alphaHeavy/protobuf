{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Data.ProtocolBuffers.Types
  ( Field(..)
  , HasField(..)
  , Required
  , RequiredField(..)
  , Optional
  , OptionalField(..)
  , Repeated
  , RepeatedField(..)
  , Packed
  , Value(..)
  , Enumeration(..)
  , Fixed(..)
  , Signed(..)
  , PackedList(..)
  , PackedField(..)
  ) where

import Control.DeepSeq (NFData)
import Data.Bits
import Data.Foldable as Fold
import Data.Maybe (fromMaybe)
import Data.Monoid
import Data.Traversable
import Data.Typeable

newtype Value a       = Value       {runValue       :: a}
  deriving (Bounded, Eq, Enum, Foldable, Functor, Monoid, Ord, NFData, Show, Traversable, Typeable)

newtype RequiredField a    = Required    {runRequired    :: a}
  deriving (Bounded, Eq, Enum, Foldable, Functor, Monoid, Ord, NFData, Show, Traversable, Typeable)

newtype OptionalField a    = Optional    {runOptional    :: a}
  deriving (Bounded, Eq, Enum, Foldable, Functor, Monoid, Ord, NFData, Show, Traversable, Typeable)

newtype RepeatedField a    = Repeated    {runRepeated    :: a}
  deriving (Bounded, Eq, Enum, Foldable, Functor, Monoid, Ord, NFData, Show, Traversable, Typeable)

newtype Field (n :: *) a = Field {runField :: a}
  deriving (Bounded, Eq, Enum, Foldable, Functor, Monoid, Ord, NFData, Show, Traversable, Typeable)

-- | Functions for wrapping and unwrapping record fields
class HasField a where
  type FieldType a :: *

  -- | Extract a value from it's 'Field' representation.
  getField :: a -> FieldType a

  -- | Wrap it back up again.
  putField :: FieldType a -> a

  -- | An isomorphism lens compatible with the lens package
  field :: Functor f => (FieldType a -> f (FieldType a)) -> a -> f a
  field f = fmap putField . f . getField

instance HasField (Field n (RequiredField (Value (Last a)))) where
  type FieldType (Field n (RequiredField (Value (Last a)))) = a
  getField = fromMaybe (error "blah") . getLast . runValue . runRequired . runField
  putField = Field . Required . Value . Last . Just

instance HasField (Field n (RequiredField (Enumeration (Last a)))) where
  type FieldType (Field n (RequiredField (Enumeration (Last a)))) = a
  getField = fromMaybe (error "blahxz") . getLast . runEnumeration . runRequired . runField
  putField = Field . Required . Enumeration . Last . Just

instance HasField (Field n (OptionalField (Value (Last a)))) where
  type FieldType (Field n (OptionalField (Value (Last a)))) = Maybe a
  getField = getLast . runValue . runOptional . runField
  putField = Field . Optional . Value . Last

instance HasField (Field n (OptionalField (Enumeration (Last a)))) where
  type FieldType (Field n (OptionalField (Enumeration (Last a)))) = Maybe a
  getField = getLast . runEnumeration . runOptional . runField
  putField = Field . Optional . Enumeration . Last

instance HasField (Field n (RepeatedField [Value a])) where
  type FieldType (Field n (RepeatedField [Value a])) = [a]
  getField = fmap runValue . runRepeated . runField
  putField = Field . Repeated . fmap Value

instance HasField (Field n (RepeatedField [Enumeration a])) where
  type FieldType (Field n (RepeatedField [Enumeration a])) = [a]
  getField = fmap runEnumeration . runRepeated . runField
  putField = Field . Repeated . fmap Enumeration

instance HasField (Field n (PackedField (PackedList (Value a)))) where
  type FieldType (Field n (PackedField (PackedList (Value a)))) = [a]
  getField = fmap runValue . unPackedList . runPackedField . runField
  putField = Field . PackedField . PackedList . fmap Value

instance HasField (Field n (PackedField (PackedList (Enumeration a)))) where
  type FieldType (Field n (PackedField (PackedList (Enumeration a)))) = [a]
  getField = fmap runEnumeration . unPackedList . runPackedField . runField
  putField = Field . PackedField . PackedList . fmap Enumeration

-- | Optional fields. Values that are not found will return 'Nothing'.
-- type Optional n (f a) = Field n (OptionalField (f (Last a)))
type family Optional (n :: *) (a :: *) :: *
type instance Optional n (Value a)       = Field n (OptionalField (Value (Last a)))
type instance Optional n (Enumeration a) = Field n (OptionalField (Enumeration (Last a)))

-- | Required fields. Parsing will return 'Control.Alternative.empty' if a 'Required' value is not found while decoding.
type family Required (n :: *) (a :: *) :: *
type instance Required n (Value a)       = Field n (RequiredField (Value (Last a)))
type instance Required n (Enumeration a) = Field n (RequiredField (Enumeration (Last a)))

-- | Lists of values.
type Repeated n a = Field n (RepeatedField [a])

-- | Packed values.
type Packed n a = Field n (PackedField (PackedList a))

-- |
-- A newtype wrapper used to distinguish 'Prelude.Enum's from other field types.
-- 'Enumeration' fields use 'Prelude.fromEnum' and 'Prelude.toEnum' when encoding and decoding messages.
newtype Enumeration a = Enumeration {runEnumeration :: a}
  deriving (Bounded, Eq, Enum, Foldable, Functor, Ord, NFData, Show, Traversable, Typeable)

-- |
-- A traversable functor used to select packed sequence encoding/decoding.
newtype PackedField a = PackedField {runPackedField :: a}
  deriving (Eq, Foldable, Functor, Monoid, NFData, Ord, Show, Traversable, Typeable)

-- |
-- A list that is stored in a packed format.
newtype PackedList a = PackedList {unPackedList :: [a]}
  deriving (Eq, Foldable, Functor, Monoid, NFData, Ord, Show, Traversable, Typeable)

-- |
-- Signed integers are stored in a zz-encoded form.
newtype Signed a = Signed a
  deriving (Bits, Bounded, Enum, Eq, Floating, Foldable, Fractional, Functor, Integral, Monoid, NFData, Num, Ord, Real, RealFloat, RealFrac, Show, Traversable, Typeable)

-- |
-- Fixed integers are stored in little-endian form without additional encoding.
newtype Fixed a = Fixed a
  deriving (Bits, Bounded, Enum, Eq, Floating, Foldable, Fractional, Functor, Integral, Monoid, NFData, Num, Ord, Real, RealFloat, RealFrac, Show, Traversable, Typeable)
