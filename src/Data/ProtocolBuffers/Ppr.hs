{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeSynonymInstances #-}

module Data.ProtocolBuffers.Ppr where

import Data.Int
import Data.Word
import Data.ProtocolBuffers.Types
import Data.Proxy
import Data.TypeLevel (Nat, D4, D5, D6, toInt)
-- import Data.Typeable
import Text.PrettyPrint
import GHC.Generics

data Foo = Foo {myField :: Required D4 Int64, theSecondField :: Optional D5 Float, aPackedField :: Packed D6 Int32} deriving (Show, Generic)

prettyProto :: forall a . (Generic a, GPrettyProto (Rep a)) => Proxy a -> Doc
prettyProto _ = gprettyProto (Proxy :: Proxy (Rep a))

class GPrettyProto (f :: * -> *) where
  gprettyProto :: Proxy f -> Doc

instance (GPrettyProto f, Datatype d) => GPrettyProto (D1 d f) where
  gprettyProto _ = text "message" <+> text (datatypeName (undefined :: t d f a)) $$ gprettyProto (Proxy :: Proxy f)

instance (GPrettyProto f, Constructor c) => GPrettyProto (C1 c f) where
  -- gprettyProto _ = text (conName (undefined :: t c f a)) $$ braces (gprettyProto (Proxy :: Proxy f))
  gprettyProto _ = braces (gprettyProto (Proxy :: Proxy f))

instance (GPrettyProtoSel f, Selector s) => GPrettyProto (S1 s f) where
  gprettyProto _ = gprettyProtoSel (Proxy :: Proxy f) selName' where
    selName' = selName (undefined :: t s f a)

instance (GPrettyProto x, GPrettyProto y) => GPrettyProto (x :*: y) where
  gprettyProto _ = gprettyProto (Proxy :: Proxy x) $+$ gprettyProto (Proxy :: Proxy y)

class GPrettyProtoSel (f :: * -> *) where
  gprettyProtoSel :: Proxy f -> String -> Doc

instance (Nat n, ProtoTypeName c) => GPrettyProtoSel (K1 i (Required n c)) where
  gprettyProtoSel _ selName' = text "required" <+> text (protoTypeName (Proxy :: Proxy c)) <+> text selName'' <+> text "=" <+> text (show (toInt (undefined :: n))) <> char ';' where
    selName'' = if null selName' then "field" ++ show (toInt (undefined :: n)) else selName'

instance (Nat n, ProtoTypeName c) => GPrettyProtoSel (K1 i (Optional n c)) where
  gprettyProtoSel _ selName' = text "optional" <+> text (protoTypeName (Proxy :: Proxy c)) <+> text selName'' <+> text "=" <+> text (show (toInt (undefined :: n))) <> char ';' where
    selName'' = if null selName' then "field" ++ show (toInt (undefined :: n)) else selName'

instance (Nat n, ProtoTypeName c) => GPrettyProtoSel (K1 i (Repeated n c)) where
  gprettyProtoSel _ selName' = text "repeated" <+> text (protoTypeName (Proxy :: Proxy c)) <+> text selName'' <+> text "=" <+> text (show (toInt (undefined :: n))) <> char ';' where
    selName'' = if null selName' then "field" ++ show (toInt (undefined :: n)) else selName'

instance (Nat n, ProtoTypeName c) => GPrettyProtoSel (K1 i (Packed n c)) where
  gprettyProtoSel _ selName' = text "repeated" <+> text (protoTypeName (Proxy :: Proxy c)) <+> text selName'' <+> text "=" <+> text (show (toInt (undefined :: n))) <+> text "[packed=true]" <> char ';' where
    selName'' = if null selName' then "field" ++ show (toInt (undefined :: n)) else selName'

class ProtoTypeName a where
  protoTypeName :: Proxy a -> String

instance ProtoTypeName Int32 where
  protoTypeName _ = "int32"

instance ProtoTypeName Int64 where
  protoTypeName _ = "int64"

instance ProtoTypeName Word32 where
  protoTypeName _ = "uint32"

instance ProtoTypeName Word64 where
  protoTypeName _ = "uint64"

instance ProtoTypeName (Fixed Int32) where
  protoTypeName _ = "sfixed32"

instance ProtoTypeName (Fixed Int64) where
  protoTypeName _ = "sfixed64"

instance ProtoTypeName (Fixed Word32) where
  protoTypeName _ = "fixed32"

instance ProtoTypeName (Fixed Word64) where
  protoTypeName _ = "fixed64"

instance ProtoTypeName Float where
  protoTypeName _ = "float"

instance ProtoTypeName Double where
  protoTypeName _ = "double"
