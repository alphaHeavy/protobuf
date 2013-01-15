{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE OverlappingInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Data.ProtocolBuffers.Types
  ( Tagged(..)
  , Required
  , Optional
  , Repeated
  , Enumeration(..)
  , GetValue(..)
  , GetEnum(..)
  ) where

import Control.DeepSeq (NFData)
import Control.Monad.Identity
import Data.Foldable
import Data.Monoid
import Data.Tagged
import Data.Traversable

-- field rules
type Optional (n :: *) a = Tagged n (Maybe a)
type Required (n :: *) a = Tagged n (Identity a)
type Repeated (n :: *) a = Tagged n [a]

instance Show a => Show (Required n a) where
  show (Tagged (Identity x)) = show (Tagged x :: Tagged n a)

class GetValue a where
  type GetValueType a :: *
  getValue :: a -> GetValueType a
  putValue :: GetValueType a -> a

instance GetValue (Optional n a) where
  type GetValueType (Tagged n (Maybe a)) = Maybe a
  getValue = unTagged
  putValue = Tagged

instance GetValue (Repeated n a) where
  type GetValueType (Tagged n [a]) = [a]
  getValue = unTagged
  putValue = Tagged

instance GetValue (Required n a) where
  type GetValueType (Tagged n (Identity a)) = a
  getValue = runIdentity . unTagged
  putValue = Tagged . Identity

-- | A newtype wrapper used to distinguish enums from other field types.
newtype Enumeration a = Enumeration a
  deriving (Bounded, Eq, Enum, Foldable, Functor, Ord, NFData, Show, Traversable)

instance Monoid (Enumeration a) where
  -- error case is handled by getEnum but we're exposing the instance :-(
  -- really should be a Semigroup instance... if we want a semigroup dependency
  mempty = Enumeration $ error "Empty Enumeration"
  _ `mappend` x = x

class GetEnum a where
  type GetEnumResult a :: *
  getEnum :: a -> GetEnumResult a
  putEnum :: GetEnumResult a -> a

instance GetEnum (Enumeration a) where
  type GetEnumResult (Enumeration a) = a
  getEnum (Enumeration x) = x
  putEnum = Enumeration

instance Enum a => GetEnum (Optional n (Enumeration a)) where
  type GetEnumResult (Tagged n (Maybe (Enumeration a))) = Maybe a
  getEnum = fmap getEnum . unTagged
  putEnum = Tagged . fmap putEnum

instance Enum a => GetEnum (Required n (Enumeration a)) where
  type GetEnumResult (Tagged n (Identity (Enumeration a))) = a
  getEnum = getEnum . runIdentity . unTagged
  putEnum = Tagged . Identity . putEnum

instance Enum a => GetEnum (Repeated n (Enumeration a)) where
  type GetEnumResult (Tagged n [Enumeration a]) = [a]
  getEnum = fmap getEnum . unTagged
  putEnum = Tagged . fmap putEnum
