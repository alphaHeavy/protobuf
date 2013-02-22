{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeFamilies #-}

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
  , Always(..)
  , PackedList(..)
  , PackedField(..)
  ) where

import Control.DeepSeq (NFData)
import Data.Bits
import Data.Foldable as Fold
import Data.Monoid
import Data.Traversable
import Data.Typeable

-- |
-- 'Value' selects the normal/typical way for encoding primitive values.
newtype Value a       = Value       {runValue       :: a}
  deriving (Bounded, Eq, Enum, Foldable, Functor, Monoid, Ord, NFData, Show, Traversable, Typeable)

-- |
-- 'RequiredField' is a newtype wrapped used to break overlapping instances
-- for encoding and decoding values
newtype RequiredField a    = Required    {runRequired    :: a}
  deriving (Bounded, Eq, Enum, Foldable, Functor, Monoid, Ord, NFData, Show, Traversable, Typeable)

-- |
-- 'OptionalField' is a newtype wrapped used to break overlapping instances
-- for encoding and decoding values
newtype OptionalField a    = Optional    {runOptional    :: a}
  deriving (Bounded, Eq, Enum, Foldable, Functor, Monoid, Ord, NFData, Show, Traversable, Typeable)

-- |
-- 'RepeatedField' is a newtype wrapped used to break overlapping instances
-- for encoding and decoding values
newtype RepeatedField a    = Repeated    {runRepeated    :: a}
  deriving (Bounded, Eq, Enum, Foldable, Functor, Monoid, Ord, NFData, Show, Traversable, Typeable)

-- |
-- Fields are merely a way to hold a field tag along with its type, this shouldn't normally be referenced directly.
--
-- This provides better error messages than older versions which used 'Data.Tagged.Tagged'
--
newtype Field (n :: *) a = Field {runField :: a}
  deriving (Bounded, Eq, Enum, Foldable, Functor, Monoid, Ord, NFData, Show, Traversable, Typeable)

-- |
-- To provide consistent instances for serialization a 'Traversable' 'Functor' is needed to
-- make 'Required' fields have the same shape as 'Optional', 'Repeated' and 'Packed'.
--
-- This is the 'Data.Functor.Identity.Identity' 'Functor' with a 'Show' instance.
newtype Always a = Always {runAlways :: a}
  deriving (Bounded, Eq, Enum, Foldable, Functor, Ord, NFData, Show, Traversable, Typeable)

instance Monoid (Always a) where
  mempty = error "Always is not a Monoid"
  mappend _ y = y

-- |
-- Functions for wrapping and unwrapping record fields.
-- When applied they will have types similar to these:
--
-- @
--'getField' :: 'Required' 'Data.TypeLevel.D1' ('Value' 'Data.Text.Text') -> 'Data.Text.Text'
--'putField' :: 'Data.Text.Text' -> 'Required' 'Data.TypeLevel.D1' ('Value' 'Data.Text.Text')
--
--'getField' :: 'Optional' 'Data.TypeLevel.D2' ('Value' 'Data.Int.Int32') -> 'Maybe' 'Data.Int.Int32'
--'putField' :: 'Maybe' 'Data.Int.Int32' -> 'Optional' 'Data.TypeLevel.D2' ('Value' 'Data.Int.Int32')
--
--'getField' :: 'Repeated' 'Data.TypeLevel.D3' ('Value' 'Double') -> ['Double']
--'putField' :: ['Double'] -> 'Repeated' 'Data.TypeLevel.D3' ('Value' 'Double')
--
--'getField' :: 'Packed' 'Data.TypeLevel.D4' ('Value' 'Data.Word.Word64') -> ['Data.Word.Word64']
--'putField' :: ['Data.Word.Word64'] -> 'Packed' 'Data.TypeLevel.D4' ('Value' 'Data.Word.Word64')
-- @
class HasField a where
  type FieldType a :: *

  -- | Extract a value from it's 'Field' representation.
  getField :: a -> FieldType a

  -- | Wrap it back up again.
  putField :: FieldType a -> a

  -- | An isomorphism lens compatible with the lens package
  field :: Functor f => (FieldType a -> f (FieldType a)) -> a -> f a
  field f = fmap putField . f . getField

-- | Iso: @ 'FieldType' ('Required' n ('Value' a)) = a @
instance HasField (Field n (RequiredField (Always (Value a)))) where
  type FieldType (Field n (RequiredField (Always (Value a)))) = a
  getField = runValue . runAlways . runRequired . runField
  putField = Field . Required . Always . Value

-- | Iso: @ 'FieldType' ('Required' n ('Enumeration' a)) = a @
instance HasField (Field n (RequiredField (Always (Enumeration a)))) where
  type FieldType (Field n (RequiredField (Always (Enumeration a)))) = a
  getField = runEnumeration . runAlways . runRequired . runField
  putField = Field . Required . Always . Enumeration

-- | Iso: @ 'FieldType' ('Optional' n ('Value' a)) = 'Maybe' a @
instance HasField (Field n (OptionalField (Last (Value a)))) where
  type FieldType (Field n (OptionalField (Last (Value a)))) = Maybe a
  getField = fmap runValue . getLast . runOptional . runField
  putField = Field . Optional . Last . fmap Value

-- | Iso: @ 'FieldType' ('Optional' n ('Enumeration' a)) = 'Maybe' a @
instance HasField (Field n (OptionalField (Last (Enumeration a)))) where
  type FieldType (Field n (OptionalField (Last (Enumeration a)))) = Maybe a
  getField = fmap runEnumeration . getLast . runOptional . runField
  putField = Field . Optional . Last . fmap Enumeration

-- | Iso: @ 'FieldType' ('Repeated' n ['Value' a]) = [a] @
instance HasField (Field n (RepeatedField [Value a])) where
  type FieldType (Field n (RepeatedField [Value a])) = [a]
  getField = fmap runValue . runRepeated . runField
  putField = Field . Repeated . fmap Value

-- | Iso: @ 'FieldType' ('Repeated' n ['Enumeration' a]) = [a] @
instance HasField (Field n (RepeatedField [Enumeration a])) where
  type FieldType (Field n (RepeatedField [Enumeration a])) = [a]
  getField = fmap runEnumeration . runRepeated . runField
  putField = Field . Repeated . fmap Enumeration

-- | Iso: @ 'FieldType' ('Packed' n ['Value' a]) = [a] @
instance HasField (Field n (PackedField (PackedList (Value a)))) where
  type FieldType (Field n (PackedField (PackedList (Value a)))) = [a]
  getField = fmap runValue . unPackedList . runPackedField . runField
  putField = Field . PackedField . PackedList . fmap Value

-- | Iso: @ 'FieldType' ('Packed' n ['Enumeration' a]) = [a] @
instance HasField (Field n (PackedField (PackedList (Enumeration a)))) where
  type FieldType (Field n (PackedField (PackedList (Enumeration a)))) = [a]
  getField = fmap runEnumeration . unPackedList . runPackedField . runField
  putField = Field . PackedField . PackedList . fmap Enumeration

-- | Optional fields. Values that are not found will return 'Nothing'.
type family Optional (n :: *) (a :: *) :: *
type instance Optional n (Value a)       = Field n (OptionalField (Last (Value a)))
type instance Optional n (Enumeration a) = Field n (OptionalField (Last (Enumeration a)))

-- | Required fields. Parsing will return 'Control.Alternative.empty' if a 'Required' value is not found while decoding.
type family Required (n :: *) (a :: *) :: *
type instance Required n (Value a)       = Field n (RequiredField (Always (Value a)))
type instance Required n (Enumeration a) = Field n (RequiredField (Always (Enumeration a)))

-- | Lists of values.
type Repeated n a = Field n (RepeatedField [a])

-- | Packed values.
type Packed n a = Field n (PackedField (PackedList a))

-- |
-- 'Enumeration' fields use 'Prelude.fromEnum' and 'Prelude.toEnum' when encoding and decoding messages.
newtype Enumeration a = Enumeration {runEnumeration :: a}
  deriving (Bounded, Eq, Enum, Foldable, Functor, Ord, Monoid, NFData, Show, Traversable, Typeable)

-- |
-- A 'Traversable' 'Functor' used to select packed sequence encoding/decoding.
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
