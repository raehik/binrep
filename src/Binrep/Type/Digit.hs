{-# LANGUAGE AllowAmbiguousTypes #-}

module Binrep.Type.Digit where

import Binrep
import Binrep.Type.Digit.Octal qualified as Octal

import Numeric.Natural ( Natural )
import Data.Word ( Word8 )
import Data.List.NonEmpty ( NonEmpty( (:|) ) )
import Mason.Builder qualified as Mason
import Data.ByteString qualified as B
import Data.Semigroup ( sconcat )
import Data.Either.Combinators ( mapLeft )

--newtype TarNum n = TarNum { getTarNum :: Sized n (AsciiDigitStr Digit) }

newtype AsciiDigitStr a = AsciiDigitStr { unAsciiDigitStr :: Natural }

-- TODO easier way to categorize. since the @a@ doesn't come up in the methods,
-- we can potentially use more efficient solutions that don't go through a
-- safe representation.
--
-- I think I've gone generic enough that it could handle multi-bytewise types
-- (if that could even be sensible). But most likely each byte can be processed
-- individually.
--
-- this is essentially a plain dictionary lol.
class ByteStringNat a where
    -- Left means "this part wasn't recognized", usually just one Word8
    from :: B.ByteString -> Either B.ByteString Natural

    to   :: Natural -> Builder

instance ByteStringNat Octal.Digit where
    from = mapLeft (\w -> B.pack [w]) . asciiBytesToNat Octal.fromByte' 8
    to   = natToAsciiBytes (+ 0x30) 8

natToAsciiBytes :: (Word8 -> Word8) -> Natural -> Natural -> Builder
natToAsciiBytes f base =
    sconcat . fmap (\w -> Mason.word8 w) . fmap f . digits @Word8 base

asciiBytesToNat :: (Word8 -> Maybe Word8) -> Natural -> B.ByteString -> Either Word8 Natural
asciiBytesToNat f base bs =
    case B.foldr go (Right (0, 0)) bs of
      Left w -> Left w
      Right (n, _) -> Right n
  where
    go :: Word8 -> Either Word8 (Natural, Natural) -> Either Word8 (Natural, Natural)
    go _ (Left w) = Left w
    go w (Right (n, expo)) =
        case f w of
          Nothing -> Left w
          Just d  -> Right (n + fromIntegral d * base^expo, expo+1)

digits :: forall b a. (Integral a, Integral b) => a -> a -> NonEmpty b
digits base = go []
  where
    go s x = loop (head' :| s) tail'
      where
        head' = fromIntegral (x `mod` base)
        tail' = x `div` base
    loop s@(r :| rs) = \case
        0 -> s
        x -> go (r : rs) x
