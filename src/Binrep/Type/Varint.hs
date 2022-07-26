{- | Variable-length integers (varints), a method to store arbitrarily large
     integers in a space efficient manner.

Note that varints aren't particularly efficient due to their decoding being
slow. They are most interesting when you wish to provide support for large
integers, but know that many (most?) inputs will be small, and want to be space
efficient for them. Protocol Buffers uses them extensively, while Cap'n Proto
swears them off.

TODO

  * https://en.wikipedia.org/wiki/Variable-length_quantity
    * I've defined basic unsigned varints. Signed varints have lots of options.
      You can use twos comp, zigzag, a sign bit, whatever.
-}

module Binrep.Type.Varint where

import Binrep
import Binrep.Type.Common ( Endianness(..) )

import Data.Bits
import FlatParse.Basic qualified as FP

-- | A variable-length unsigned integer (natural).
--
-- The base algorithm is to split the natural into groups of 7 bits, and use the
-- MSB to indicate whether another octet follows: 0 means end, 1 means more to
-- come. You may further select byte ordering. Little-endian results an encoding
-- matching LEB128, and big-endian matches Google's Protocol Buffers.
--
-- You may select the type to use varnats at, but error handling isn't provided:
-- negatives won't work correctly, and overflow cannot be detected. So most of
-- the time, you probably want 'Natural' and 'Integer'.
newtype Varnat (e :: Endianness) i = Varnat { getVarnat :: i }
    deriving (Eq, Ord, Enum, Num, Real, Integral) via i
    deriving stock Show

-- | VLQ
instance (Integral i, Bits i) => Get (Varnat 'BE i) where
    get = go (0 :: i)
      where
        go i = do
            w8 <- FP.anyWord8
            let i' = shiftL i 7 .|. fromIntegral (clearBit w8 7)
            if testBit w8 7 then go i' else pure (Varnat i')

-- | protobuf, LEB128
--
-- not truly infinite length since shifters take 'Int', but practically infinite
instance (Integral i, Bits i) => Get (Varnat 'LE i) where
    get = go (0 :: i) (0 :: Int)
      where
        go i n = do
            w8 <- FP.anyWord8
            let i' = i .|. shiftL (fromIntegral (clearBit w8 7)) n
            if testBit w8 7 then go i' (n+7) else pure (Varnat i')
