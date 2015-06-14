module Data.Binary.Builder.Sized where

import qualified Data.ByteString.Builder as B
import Data.Monoid (mempty, mappend, Monoid)
import qualified Data.ByteString.Lazy as LBS
import qualified Data.ByteString as BS
import qualified Data.Word as W

data Builder = Builder {-# UNPACK #-} !Int B.Builder

instance Monoid Builder where
  mempty = Builder 0 mempty
  (Builder i b) `mappend` (Builder i' b') = Builder (i + i') (mappend b b')

toLazyByteString :: Builder -> LBS.ByteString
toLazyByteString (Builder _ b) = B.toLazyByteString b

size :: Builder -> Int
size (Builder i _) = i

empty :: Builder
empty = mempty

singleton :: W.Word8 -> Builder
singleton = Builder 1 . B.word8

append :: Builder -> Builder -> Builder
append = mappend

fromByteString :: BS.ByteString -> Builder
fromByteString s = Builder (BS.length s) $ B.byteString s

fromLazyByteString :: LBS.ByteString -> Builder
fromLazyByteString s = Builder (fromIntegral $ LBS.length s) $ B.lazyByteString s

putWord16be :: W.Word16 -> Builder
putWord16be = Builder 2 . B.word16BE

putWord32be :: W.Word32 -> Builder
putWord32be = Builder 4 . B.word32BE

putWord64be :: W.Word64 -> Builder
putWord64be = Builder 8 . B.word64BE

putWord16le :: W.Word16 -> Builder
putWord16le = Builder 2 . B.word16LE

putWord32le :: W.Word32 -> Builder
putWord32le = Builder 4 . B.word32LE

putWord64le :: W.Word64 -> Builder
putWord64le = Builder 8 . B.word64LE
