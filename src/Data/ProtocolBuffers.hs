-- |
--
-- An implementation of Protocol Buffers in pure Haskell.
--
-- Extensive documentation is available at <https://developers.google.com/protocol-buffers/docs/overview>
-- and Google's reference implementation can be found at <http://code.google.com/p/protobuf/>
--
-- It is intended to be used via "GHC.Generics" and does not require @ .proto @ files to function. Tools are being developed that will convert a Haskell Protobuf definition into a @ .proto @ and vise versa.
--
-- Given a message definition:
--
-- @
--{-\# LANGUAGE DeriveGeneric \#-}
--
-- import "Data.Int"
--import "Data.ProtocolBuffers"
--import "Data.TypeLevel" ('D1', 'D2', 'D3')
--import "Data.Text"
--import "GHC.Generics" ('Generic')
--
-- data Foo = Foo
--   { field1 :: 'Required' 'Data.TypeLevel.D1' ('Value' 'Data.Int.Int64') -- ^ The last field with tag = 1
--   , field2 :: 'Optional' 'Data.TypeLevel.D2' ('Value' 'Data.Text.Text') -- ^ The last field with tag = 2
--   , field3 :: 'Repeated' 'Data.TypeLevel.D3' ('Value' 'Prelude.Bool') -- ^ All fields with tag = 3, ordering is preserved
--   } deriving ('GHC.Generics.Generic', 'Prelude.Show')
--
-- instance 'Encode' Foo
--instance 'Decode' Foo
-- @
--
-- It can then be used for encoding:
--
-- >>> let msg = Foo{field1 = putField 42, field2 = mempty, field3 = putField [True, False]}
-- >>> fmap hex runPut $ encodeMessage msg
-- "082A18011800"
--
-- As well as decoding:
--
-- >>> runGet decodeMessage =<< unhex "082A18011800" :: Either String Foo
-- Right
--   (Foo
--     { field1 = Field {runField = Required {runRequired = Always {runAlways = Value {runValue = 42}}}}
--     , field2 = Field {runField = Optional {runOptional = Last {getLast = Nothing}}}
--     , field3 = Field {runField = Repeated {runRepeated = [Value {runValue = True},Value {runValue = False}]}}
--     }
--   )
--
module Data.ProtocolBuffers
  ( -- * Message Serialization
    --
    -- ** Encoding
    --
    Encode(..)
  , encodeMessage
  , encodeLengthPrefixedMessage

    -- ** Decoding
    --
  , Decode(..)
  , decodeMessage
  , decodeLengthPrefixedMessage

    -- * Fields
    --
    -- ** Tags
    -- |
    --
    -- Restricted type aliases of 'Field'. These are used to attach a field tag (a numeric id) to a field.
    -- Each tag must be unique within a given message, though this is not currently checked or enforced.
    --
  , Required
  , Optional
  , Repeated
  , Packed

    -- ** Accessors
    -- |
    --
    -- Fields tend to have rather complex types that are unpleasant to interact with.
    -- 'HasField' was designed to hide this complexity and provide a consistent way of
    -- getting and setting fields.
    --
  , HasField(..)

    -- ** Selectors
    -- |
    --
    -- Follow these rules to define fields supported by the generic encoder/decoder:
    --
    -- * The 'n' phantom type parameter specifies the Protocol Buffers field tag (id).
    --
    -- * Field tags /must/ be an instance of 'Data.TypeLevel.Nat'.
    --
    -- * Field selectors /must/ be an instance of 'Data.Foldable.Foldable' to support encoding.
    --
    -- * Value selectors /must/ be an instance of 'Data.Monoid.Monoid' to support decoding.
    --
  , Field

    -- * Values
    --
    -- ** Selectors
    -- |
    --
    -- Each field value needs to specify the way it should be encoded.
    --
    -- There are three built-in value selectors: 'Value', 'Enumeration' and 'Message'.
    --
    -- If you're unsure what value selector to use, 'Value' is probably the correct one.
    --
  , Value
  , Enumeration
  , Message

  -- * Wire Coding
  -- |
  --
  -- Some primitive values can be more compactly represented. Fields that typically contain
  -- negative or very large numbers should use the 'Signed' or 'Fixed' wrappers to select
  -- their respective (efficient) formats.
  --
  , Signed(..)
  , Fixed(..)

  -- * Internal Goo
  -- |
  --
  -- These types are exported but normally should not be used
  --
  , RequiredField
  , OptionalField
  , RepeatedField
  , PackedField
  , PackedList
  , Always
  ) where

import Data.ProtocolBuffers.Decode
import Data.ProtocolBuffers.Message
import Data.ProtocolBuffers.Encode
import Data.ProtocolBuffers.Types
