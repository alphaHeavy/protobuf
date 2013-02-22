-- |
--
-- An implementation of Protocol Buffers in pure Haskell.
--
-- Extensive documentation is available at <https://developers.google.com/protocol-buffers/docs/overview>
-- and Google's reference implementation can be found at <http://code.google.com/p/protobuf/>.
--
-- It is intended to be used via "GHC.Generics" and does not require @ .proto @ files to function.
-- Tools are being developed that will convert a Haskell Protobuf definition into a @ .proto @ and vise versa.
--
-- The "Data.TypeLevel" dependency is required due to <http://hackage.haskell.org/trac/ghc/ticket/7459>.
-- I believe the partial fix already committed will allow migrating to "GHC.TypeLits" once GHC 7.8.1 is released.
--
-- Given a message definition:
--
-- @
--{-\# LANGUAGE DeriveGeneric \#-}
--
-- import "Data.Int"
--import "Data.ProtocolBuffers"
--import "Data.TypeLevel" ('Data.TypeLevel.D1', 'Data.TypeLevel.D2', 'Data.TypeLevel.D3')
--import "Data.Text"
--import "GHC.Generics" ('GHC.Generics.Generic')
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
-- It can then be used for encoding and decoding.
--
-- To construct a message, use 'putField' to set each field value. 'Optional', 'Repeated' and 'Packed'
-- fields can be set to their empty value by using 'Data.Monoid.mempty'. An example using record syntax for clarity:
--
-- >>> let msg = Foo{field1 = putField 42, field2 = mempty, field3 = putField [True, False]}
--
-- To serialize a message first convert it into a 'Data.Serialize.Put' by way of 'encodeMessage'
-- and then to a 'Data.ByteString.ByteString' by using 'Data.Serialize.runPut'. Lazy
-- 'Data.ByteString.Lazy.ByteString' serialization is done with 'Data.Serialize.runPutLazy'.
--
-- >>> fmap hex runPut $ encodeMessage msg
-- "082A18011800"
--
-- Decoding is done with the inverse functions: 'decodeMessage'
-- and 'Data.Serialize.runGet', or 'Data.Serialize.runGetLazy'.
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
-- Use 'getField' to read fields from a message:
--
-- >>> let Right msg = runGet decodeMessage =<< unhex "082A18011800" :: Either String Foo
-- >>> getField $ field1 msg
-- 42
-- >>> getField $ field2 msg
-- Nothing
-- >>> getField $ field3 msg
-- [True,False]
--
-- Some Protocol Buffers features are not currently implemented:
--
--   * Default values for 'Optional' fields
--
--   * Extension fields
--
--   * Storing unknown fields, those without a mapped field tag in message record
--
--   * Tag-delimited Groups, deprecated in lieu of 'Message'
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
  ) where

import Data.ProtocolBuffers.Decode
import Data.ProtocolBuffers.Message
import Data.ProtocolBuffers.Encode
import Data.ProtocolBuffers.Types
