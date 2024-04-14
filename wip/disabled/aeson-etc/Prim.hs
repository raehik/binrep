module Binrep.Put.Prim where

import GHC.Exts
import Foreign

import Binrep.BLen ( BLenT, posIntToBLen )

import Data.Text.Unsafe qualified
import Data.Text.Foreign qualified
import Data.Text qualified as Text
import Data.Text ( Text )
import Data.Char qualified as Char

{- text-2.0 only
{-# INLINE putTextUtf8 #-}
-- | TODO use with 'Data.Text.Unsafe.lengthWord8'
putTextUtf8 :: Text -> Ptr Word8 -> IO ()
putTextUtf8 = Data.Text.Foreign.unsafeCopyToPtr

{-# INLINE blenTextUtf8 #-}
-- | TODO amazing, O(1). length of UTF-8 bytestring (no null term)
blenTextUtf8 :: Text -> BLenT
blenTextUtf8 t = posIntToBLen . Data.Text.Unsafe.lengthWord8
-}

-- | TODO 'cstringLength#' is @O(1)@ with a literal
putAscii# :: Addr# -> Ptr Word8 -> IO ()
putAscii# addr# p = copyBytes p (Ptr addr#) (I# (cstringLength# addr#))

-- TODO inline? or no?
-- only takes ASCII input (explodes on UTF-8)
blenTextUtf8AesonEscapeAsciiChar :: Word8 -> Int
blenTextUtf8AesonEscapeAsciiChar w8
  | w8 == c2w '\\' = 2
  | w8 == c2w '\"' = 2
  | w8 >= 0x20 = 1
  | w8 == c2w '\n' = 2
  | w8 == c2w '\r'  = 2
  | w8 == c2w '\t'  = 2
  | otherwise = 6

blenTextUtf8AesonEscape :: Text -> Int
-- TODO need efficient version... :(
-- TODO no foldr' even until 2.0...
blenTextUtf8AesonEscape = Text.foldr go 0
  where
    go c i =
        if   Char.isAscii c
        then i + blenTextUtf8AesonEscapeAsciiChar (c2w c)
        else i + utf8Length c

-- from 'Data.Text.Internal.Encoding.Utf8.utf8Length' (text 2.0)
utf8Length :: Char -> Int
utf8Length (C# c) = I# ((1# +# geChar# c (chr# 0x80#)) +# (geChar# c (chr# 0x800#) +# geChar# c (chr# 0x10000#)))

-- | TODO convenience. should be no-op. can we trust it?
c2w :: Char -> Word8
c2w = fromIntegral . Char.ord
