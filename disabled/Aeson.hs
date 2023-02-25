module Aeson where

import Binrep
import Binrep.Generic
import Binrep.Type.ByteString

import GHC.Generics ( Generic )
import Data.Word

import Foreign

import Data.Aeson
import Data.Aeson.KeyMap qualified as Aeson
import Data.Aeson.Key qualified as Aeson

blenTextUtf8AesonEscapeAsciiChar :: Word8 -> Int
blenTextUtf8AesonEscapeAsciiChar = 1

blenTextUtf8AesonEscape :: Text -> Int
blenTextUtf8AesonEscape = Text.foldl' go 0
  where
    go i c =
        if   Char.isAscii c
        then i + blenTextUtf8AesonEscapeAsciiChar (c2w c)
        else i + utf8Length c

-- from 'Data.Text.Internal.Encoding.Utf8.utf8Length' (text 2.0)
utf8Length :: Char -> Int
utf8Length (C# c) = I# ((1# +# geChar# c (chr# 0x80#)) +# (geChar# c (chr# 0x800#) +# geChar# c (chr# 0x10000#)))

instance BLen Aeson.Key where
    blen = undefined
instance BLen v => BLen (Aeson.KeyMap v) where
    blen = undefined
instance Put' Value where
    put' v p =
        case v of
          Object o -> do
            put' @Word8 0x7B p
            -- TODO
            put' @Word8 0x7D (p `plusPtr` 1)
          String s -> do
            put' @Word8 0x22 p
            -- TODO escape
            --Data.Text.Foreign.unsafeCopyToPtr s p
            put' @Word8 0x22 (p `plusPtr` 1)
          Bool b ->
            case b of
              True  -> putAscii# "true"#  p
              False -> putAscii# "false"# p
          Null -> putAscii# "null"# p
          Number n -> error "unimplemented - aeson numbers"
          Array vs -> error "unimplemented - aeson arrays"
          Array vs -> error "unimplemented - aeson arrays"
