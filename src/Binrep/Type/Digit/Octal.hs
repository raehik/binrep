module Binrep.Type.Digit.Octal where

import Data.Word ( Word8 )

data Digit = D0 | D1 | D2 | D3 | D4 | D5 | D6 | D7

fromByte :: Word8 -> Maybe Digit
fromByte = \case 0x30 -> Just D0
                 0x31 -> Just D1
                 0x32 -> Just D2
                 0x33 -> Just D3
                 0x34 -> Just D4
                 0x35 -> Just D5
                 0x36 -> Just D6
                 0x37 -> Just D7
                 _    -> Nothing

toByte :: Digit -> Word8
toByte = \case D0 -> 0x30
               D1 -> 0x31
               D2 -> 0x32
               D3 -> 0x33
               D4 -> 0x34
               D5 -> 0x35
               D6 -> 0x36
               D7 -> 0x37

fromByte' :: Word8 -> Maybe Word8
fromByte' = \case 0x30 -> Just 0
                  0x31 -> Just 1
                  0x32 -> Just 2
                  0x33 -> Just 3
                  0x34 -> Just 4
                  0x35 -> Just 5
                  0x36 -> Just 6
                  0x37 -> Just 7
                  _    -> Nothing
