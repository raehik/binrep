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

{-# LANGUAGE AllowAmbiguousTypes #-}

module Binrep.Type.Varint where

import Binrep
import Binrep.Type.Common ( Endianness(..) )

import Data.Bits
import FlatParse.Basic qualified as FP

import Data.Word ( Word8 )

-- | A variable-length unsigned integer (natural).
--
-- The base algorithm is to split the natural into groups of 7 bits, and use the
-- MSB to indicate whether another octet follows. You must specify a handful of
-- type variables, which select precise varint behaviour beyond this. See their
-- documentation for details.
--
-- You may select the type to use varnats at, but error handling isn't provided:
-- negatives won't work correctly, and overflow cannot be detected. So most of
-- the time, you probably want 'Natural' and 'Integer'.
--
-- Some examples:
--
--   * @'Varnat' ''Redundant' ''OnContinues'  ''BE' matches VLQ.
--   * @'Varnat' ''Redundant' ''OnContinues'  ''LE' matches LEB128, protobuf.
--   * @'Varnat' ''Bijective' ''OnContinues'  ''LE' matches Git's varints.
--   * @'Varnat' ''Bijective' ''OffContinues' ''LE' matches BPS's varints.
newtype Varnat (enc :: Encoding) (cont :: ContinuationBitBehaviour) (e :: Endianness) i = Varnat { getVarnat :: i }
    deriving (Eq, Ord, Enum, Num, Real, Integral) via i
    deriving stock Show

data ContinuationBitBehaviour
  = OnContinues
  -- ^ on=continue, off=end

  | OffContinues
  -- ^ on=end, off=continue

data Encoding
  = Redundant
  -- ^ simple, some varints have the same value

  | Bijective
  -- ^ each integer has exactly 1 varint encoding

-- | VLQ (cont=on)
instance (VarintContinuation cont, Integral i, Bits i) => Get (Varnat 'Redundant cont 'BE i) where
    get = go (0 :: i)
      where
        go i = do
            w8 <- FP.anyWord8
            let i' = unsafeShiftL i 7 .|. fromIntegral (clearBit w8 7)
            if varintContinue @cont (testBit w8 7) then go i' else pure (Varnat i')

-- | TODO nothing to test against - unsure if correct
instance (VarintContinuation cont, Integral i, Bits i) => Get (Varnat 'Bijective cont 'BE i) where
    get = go (0 :: i)
      where
        go i = do
            w8 <- FP.anyWord8
            let i' = unsafeShiftL i 7 .|. (fromIntegral (clearBit w8 7) + 1)
            if varintContinue @cont (testBit w8 7) then go i' else pure (Varnat (i'-1))

-- | protobuf (cont=on), LEB128 (cont=on)
--
-- not truly infinite length since shifters take 'Int', but practically infinite
instance (VarintContinuation cont, Integral i, Bits i) => Get (Varnat 'Redundant cont 'LE i) where
    get = go (0 :: i) (0 :: Int)
      where
        go i n = do
            w8 <- FP.anyWord8
            let i' = i .|. unsafeShiftL (fromIntegral (clearBit w8 7)) n
            if varintContinue @cont (testBit w8 7) then go i' (n+7) else pure (Varnat i')

-- | Git varint (cont=on), BPS (beat patches) (cont=off)
instance (VarintContinuation cont, Integral i, Bits i) => Get (Varnat 'Bijective cont 'LE i) where
    get = go (0 :: i) (0 :: Int)
      where
        go i n = do
            w8 <- FP.anyWord8
            let i' = i .|. unsafeShiftL (fromIntegral (clearBit w8 7) + 1) n
            if varintContinue @cont (testBit w8 7) then go i' (n+7) else pure (Varnat (i'-1))

class VarintContinuation (cont :: ContinuationBitBehaviour) where
    varintContinue :: Bool -> Bool
    varintContinueSet :: Bits a => a -> a
instance VarintContinuation 'OnContinues  where
    varintContinue = id
    varintContinueSet a = setBit a 7
instance VarintContinuation 'OffContinues where
    varintContinue = not
    varintContinueSet a = setBit a 7

-- TODO
instance (VarintContinuation cont, Integral i, Bits i) => Put (Varnat 'Redundant cont 'LE i) where
    put (Varnat i) = do
        if i < 0b10000000 then
            put @Word8 $ fromIntegral i
        else
               put @Word8 (varintContinueSet @cont (fromIntegral i))
            <> put @(Varnat 'Redundant cont 'LE i) (Varnat (unsafeShiftR i 7))
