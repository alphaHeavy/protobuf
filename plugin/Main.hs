{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Monad.Identity
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as Bl
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HashMap
import Data.Int
import Data.Monoid
import Data.ProtocolBuffers
import Data.ProtocolBuffers.Internal
import Data.Serialize
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Lazy as TextL
import qualified Data.Text.Lazy.Encoding as TextL
import qualified Data.Text.Lazy.IO as TextL
import Debug.Trace
import qualified Data.TypeLevel as Tl
import GHC.Generics
import Language.Haskell.Exts
import Language.Haskell.Exts.Pretty (prettyPrint)
import Language.Haskell.Exts.SrcLoc (noLoc)

data FieldDescriptorProto_Type
  = TYPE_DOUBLE
  | TYPE_FLOAT
  | TYPE_INT64
  | TYPE_UINT64
  | TYPE_INT32
  | TYPE_FIXED64
  | TYPE_FIXED32
  | TYPE_BOOL
  | TYPE_STRING
  | TYPE_GROUP
  | TYPE_MESSAGE
  | TYPE_BYTES
  | TYPE_UINT32
  | TYPE_ENUM
  | TYPE_SFIXED32
  | TYPE_SFIXED64
  | TYPE_SINT32
  | TYPE_SINT64
    deriving (Eq, Show)

instance Enum FieldDescriptorProto_Type where
  toEnum 1 = TYPE_DOUBLE
  toEnum 2 = TYPE_FLOAT
  toEnum 3 = TYPE_INT64
  toEnum 4 = TYPE_UINT64
  toEnum 5 = TYPE_INT32
  toEnum 6 = TYPE_FIXED64
  toEnum 7 = TYPE_FIXED32
  toEnum 8 = TYPE_BOOL
  toEnum 9 = TYPE_STRING
  toEnum 10 = TYPE_GROUP
  toEnum 11 = TYPE_MESSAGE
  toEnum 12 = TYPE_BYTES
  toEnum 13 = TYPE_UINT32
  toEnum 14 = TYPE_ENUM
  toEnum 15 = TYPE_SFIXED32
  toEnum 16 = TYPE_SFIXED64
  toEnum 17 = TYPE_SINT32
  toEnum 18 = TYPE_SINT64
  fromEnum TYPE_DOUBLE         = 1
  fromEnum TYPE_FLOAT          = 2
  fromEnum TYPE_INT64          = 3
  fromEnum TYPE_UINT64         = 4
  fromEnum TYPE_INT32          = 5
  fromEnum TYPE_FIXED64        = 6
  fromEnum TYPE_FIXED32        = 7
  fromEnum TYPE_BOOL           = 8
  fromEnum TYPE_STRING         = 9
  fromEnum TYPE_GROUP          = 10
  fromEnum TYPE_MESSAGE        = 11
  fromEnum TYPE_BYTES          = 12
  fromEnum TYPE_UINT32         = 13
  fromEnum TYPE_ENUM           = 14
  fromEnum TYPE_SFIXED32       = 15
  fromEnum TYPE_SFIXED64       = 16
  fromEnum TYPE_SINT32         = 17
  fromEnum TYPE_SINT64         = 18

data FieldDescriptorProto_Label
  = LABEL_OPTIONAL
  | LABEL_REQUIRED
  | LABEL_REPEATED
    deriving (Eq, Show)

instance Enum FieldDescriptorProto_Label where
    toEnum 1 = LABEL_OPTIONAL
    toEnum 2 = LABEL_REQUIRED
    toEnum 3 = LABEL_REPEATED
    fromEnum LABEL_OPTIONAL      = 1
    fromEnum LABEL_REQUIRED      = 2
    fromEnum LABEL_REPEATED      = 3

data FieldOptions = FieldOptions
  { fieldOptionsPacked     :: Optional Tl.D2 (Last Bool)
  , fieldOptionsDeprecated :: Optional Tl.D3 (Last Bool)
  , fieldOptionsLazy       :: Optional Tl.D5 (Last Bool) -- disable strict field?
  -- optional CType ctype = 1 [default = STRING];
  } deriving (Generic, Show)

instance Decode FieldOptions

data FieldDescriptorProto = FieldDescriptorProto
  { fieldDescriptorName         :: Optional Tl.D1 Text
  , fieldDescriptorNumber       :: Optional Tl.D3 (Last Int32)
  , fieldDescriptorLabel        :: Optional Tl.D4 (Enumeration (Maybe FieldDescriptorProto_Label))
  , fieldDescriptorType         :: Optional Tl.D5 (Enumeration (Maybe FieldDescriptorProto_Type))
  , fieldDescriptorTypeName     :: Optional Tl.D6 Text
  , fieldDescriptorExtendee     :: Optional Tl.D2 Text
  , fieldDescriptorDefaultValue :: Optional Tl.D7 Text
  , fieldDescriptorOptions      :: Optional Tl.D8 (Message FieldOptions)
  } deriving (Generic, Show)

instance Decode FieldDescriptorProto

data DescriptorProto = DescriptorProto
  { descriptorName       :: Optional Tl.D1 Text
  , descriptorField      :: Repeated Tl.D2 (Message FieldDescriptorProto)
  , descriptorExtension  :: Repeated Tl.D6 (Message FieldDescriptorProto)
  , descriptorNestedType :: Repeated Tl.D3 (Message DescriptorProto)
  -- repeated EnumDescriptorProto enum_type = 4;

  -- message ExtensionRange {
    -- optional int32 start = 1;
    -- optional int32 end = 2;
  -- }
  -- repeated ExtensionRange extension_range = 5;

  -- optional MessageOptions options = 7;
  } deriving (Generic, Show)

instance Decode DescriptorProto

data FileDescriptorProto = FileDescriptorProto
  { fileDescriptorName             :: Optional Tl.D1 Text
  , fileDescriptorPackage          :: Optional Tl.D2 Text
  , fileDescriptorDependency       :: Repeated Tl.D3 Text
  , fileDescriptorPublicDependency :: Repeated Tl.D10 Int32
  , fileDescriptorWeakDependency   :: Repeated Tl.D11 Int32

  -- All top-level definitions in this file.
  , fileDescriptorMessageType      :: Repeated Tl.D4 (Message DescriptorProto)
  -- repeated EnumDescriptorProto enum_type = 5;
  -- repeated ServiceDescriptorProto service = 6;
  , fileDescriptorExtension        :: Repeated Tl.D7 (Message FieldDescriptorProto)

  -- optional FileOptions options = 8;

  -- This field contains optional information about the original source code.
  -- You may safely remove this entire field whithout harming runtime
  -- functionality of the descriptors -- the information is needed only by
  -- development tools.
  -- optional SourceCodeInfo source_code_info = 9;
  } deriving (Generic, Show)

instance Decode FileDescriptorProto

data CodeGeneratorRequest = CodeGeneratorRequest
  { fileToGenerate :: Repeated Tl.D1 Text
  , parameter      :: Optional Tl.D2 Text
  , protoFile      :: Repeated Tl.D15 (Message FileDescriptorProto)
  } deriving (Generic, Show)

instance Decode CodeGeneratorRequest

data CodeGeneratorResponse_File = CodeGeneratorResponse_File
  { responseFileName       :: Optional Tl.D1 Text
  , responseInsertionPoint :: Optional Tl.D2 Text
  , responseContent        :: Optional Tl.D15 Text
  } deriving (Generic, Show)

instance Encode CodeGeneratorResponse_File

data CodeGeneratorResponse = CodeGeneratorResponse
  { errorStr :: Optional Tl.D1 String
  , responseFiles :: Repeated Tl.D15 (Message CodeGeneratorResponse_File)
  } deriving (Generic, Show)

instance Encode CodeGeneratorResponse

blah :: FileDescriptorProto -> Text
blah fdp = T.pack $ prettyPrint m where
  m = Module noLoc (ModuleName "Foo") pragmas Nothing exports imports decls
  pragmas = [LanguagePragma noLoc [Ident "DataKinds"]]
  exports = Nothing
  imports = []
  decls = []

blahs :: [Message FileDescriptorProto] -> [CodeGeneratorResponse_File]
blahs = fmap step where
  step (Message (Just fpd)) = CodeGeneratorResponse_File
    { responseFileName = putValue "test.hs"
    , responseInsertionPoint = putValue ""
    , responseContent = putValue $ blah fpd
    }

-- getCodeFor :: HashMap Tag [Field] -> Bl.ByteString
getCodeFor :: CodeGeneratorRequest -> CodeGeneratorResponse
getCodeFor val = traceShow val CodeGeneratorResponse
  { errorStr = putValue "" -- some failure string"
  , responseFiles = putValue . fmap (Message . Just) . blahs . getValue $ protoFile val
  }

main :: IO ()
main = Bl.interact $ \ input ->
  runPutLazy . encodeMessage $
    case runGetLazy decodeMessage input of
      Right val -> getCodeFor val
      Left  err -> CodeGeneratorResponse
       { errorStr = putValue err
       , responseFiles = putValue []
       }
