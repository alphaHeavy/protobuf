module Data.Binary.Builder.Sized where

import qualified Data.Binary.Builder as B
import Data.Monoid (Sum(..), mempty, mappend)
import qualified Data.ByteString.Lazy as LBS
import qualified Data.ByteString as BS
import qualified Data.Word as W

type Builder = (Sum Int, B.Builder)

toLazyByteString :: Builder -> LBS.ByteString
toLazyByteString = B.toLazyByteString . snd

size :: Builder -> Int
size = getSum . fst

empty :: Builder
empty = mempty

singleton :: W.Word8 -> Builder
singleton = mk 1 . B.singleton

append :: Builder -> Builder -> Builder
append = mappend

fromByteString :: BS.ByteString -> Builder
fromByteString s = (Sum $ BS.length s, B.fromByteString s)

fromLazyByteString :: LBS.ByteString -> Builder
fromLazyByteString s = (Sum $ fromInteger $ toInteger $ LBS.length s, B.fromLazyByteString s)

flush :: Builder
flush = (Sum 0, B.flush)

mk :: Int -> B.Builder -> Builder
mk i f = (Sum i, f)

putWord16be :: W.Word16 -> Builder
putWord16be = mk 2 . B.putWord16be

putWord32be :: W.Word32 -> Builder
putWord32be = mk 4 . B.putWord32be

putWord64be :: W.Word64 -> Builder
putWord64be = mk 8 . B.putWord64be

putWord16le :: W.Word16 -> Builder
putWord16le = mk 2 . B.putWord16le

putWord32le :: W.Word32 -> Builder
putWord32le = mk 4 . B.putWord32le

putWord64le :: W.Word64 -> Builder
putWord64le = mk 8 . B.putWord64le

putWord16host :: W.Word16 -> Builder
putWord16host = mk 2 . B.putWord16host

putWord32host :: W.Word32 -> Builder
putWord32host = mk 4 . B.putWord32host

putWord64host :: W.Word64 -> Builder
putWord64host = mk 8 . B.putWord64host
