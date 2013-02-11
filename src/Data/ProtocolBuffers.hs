-- |
--
-- An implementation of Protocol Buffers in pure Haskell. <http://code.google.com/p/protobuf/>
--
-- It is intended to be used via "GHC.Generics" and does not require @ .proto @ files to function.
--
-- Given a message definition:
--
-- @
--{-\# LANGUAGE DeriveGeneric \#-}
--
-- import "Data.ProtocolBuffers"
--import "GHC.Generics"
--
-- data Foo = Foo
--   { field1 :: 'Required' 'Data.TypeLevel.D1' ('Data.Monoid.Last' 'Data.Int.Int64') -- ^ The last field with tag = 1
--   , field2 :: 'Optional' 'Data.TypeLevel.D2' ('Data.Monoid.Last' 'Data.Text.Text') -- ^ The last field with tag = 2
--   , field3 :: 'Repeated' 'Data.TypeLevel.D3' 'Prelude.Bool' -- ^ All fields with tag = 3, ordering is preserved
--   } deriving ('GHC.Generics.Generic', 'Prelude.Show')
--
-- instance 'Encode' Foo
--instance 'Decode' Foo
-- @
--
-- It can then be used for encoding:
--
-- >>> let msg = Foo{field1 = putValue (Last (Just 42)), field2 = mempty, field3 = putValue [True, False]}
-- >>> fmap hex runPut $ encodeMessage msg
-- "082A18011800"
--
-- As well as decoding:
--
-- >>> runGet decodeMessage =<< unhex "082A18011800" :: Either String Foo
-- Right (Foo {field1 = Tagged (Last {getLast = Just 42}), field2 = Tagged (Optionally {runOptionally = Last {getLast = Nothing}}), field3 = Tagged [True,False]})
--
--
module Data.ProtocolBuffers
  ( -- * Encoding
    --
    Encode(..)
  , encodeMessage
  , encodeLengthPrefixedMessage

    -- * Decoding
    --
  , Decode(..)
  , decodeMessage
  , decodeLengthPrefixedMessage

    -- * Field Tags
    -- |
    --
    -- Restricted type aliases of 'Data.Tagged.Tagged'. Follow these rules to define fields supported by the generic encoder/decoder:
    --
    -- * The 'n' phantom type parameter specifies the Protocol Buffers field tag (id).
    --
    -- * Field tags /must/ be an instance of 'Data.TypeLevel.Nat'.
    --
    -- * Field types /must/ be an instance of 'Data.Foldable.Foldable' to support encoding.
    --
    -- * Field types /should/ be an instance of 'Data.Monoid.Monoid' to support decoding.
    --   For types that are not already 'Data.Monoid.Monoid' instances,
    --   the use of 'Data.Monoid.Last' models the behavior recommended by the Protocol Buffers documentation.
    --
  , Required
  , Optional
  , Repeated
  , Packed

    -- * Value Accessors
    --
  , GetValue(..)
  , GetEnum(..)

    -- * Simple Field Tags
    -- |
    --
    -- Fields that default to the 'Data.Monoid.Last' 'Data.Monoid.Monoid' when merging duplicates
    --
  , Required'
  , Optional'
  -- , Repeated'
  -- , Packed'

    -- * Simple Value Accessors
    --
  , GetValue'(..)
  , GetMessage(..)

    -- * Value Selectors
    --
  , Enumeration
  , PackedField(..)
  , PackedList(..)
  , Message(..)
  , Optionally
  , Signed(..)
  , Fixed(..)
  ) where

import Data.ProtocolBuffers.Decode
import Data.ProtocolBuffers.Message
import Data.ProtocolBuffers.Encode
import Data.ProtocolBuffers.Types
