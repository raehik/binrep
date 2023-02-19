module Bytezap.Example where

import Bytezap.Class
import Bytezap.Class.Generic

import GHC.Generics ( Generic )
import Data.Word
import Data.ByteString ( ByteString )

data D1 = D1 Word8 | D2 Word16 | D3 Word8 Word8 Word32 | D4 | D5 ByteString
    deriving stock (Generic, Show, Eq)

instance Put D1 where
    put = putGeneric
{-
    put = \case
     D1 w1 -> put @Word8 0x00 <> put w1
     D2 w1 -> put @Word8 0x01 <> put w1
     D3 w1 w2 w3 -> put @Word8 0x02 <> put w1 <> put w2 <> put w3
     D4    -> put @Word8 0x03
     D5 w1 -> put @Word8 0x04 <> put w1
-}
