{-| Naturals represented via ASCII numerals.

A concept which sees occasional use in places where neither speed nor size
efficiency matter.

The tar file format uses it, apparently to sidestep making a decision on byte
ordering. Though digits are encoded "big-endian", so, uh. I don't get it.

I don't really see the usage of these. It seems silly and inefficient, aimed
solely at easing debugging.
-}

{-# LANGUAGE AllowAmbiguousTypes #-}

module Binrep.Type.AsciiNat where

import Binrep
import Binrep.Util ( natVal'' )

import Data.Word ( Word8 )
import Data.List.NonEmpty ( NonEmpty( (:|) ) )
import Data.Store qualified as Store
import Data.ByteString qualified as B

import GHC.TypeNats ( Natural, KnownNat )
import GHC.Num.Natural ( naturalSizeInBase#, naturalToWord# )

import GHC.Generics ( Generic )
import Data.Data ( Data )
import Numeric ( showOct, showHex, showBin, showInt )
import Data.Foldable ( traverse_ )

-- | A 'Natural' represented in binary as an ASCII string, where each character
--   a is a digit in the given base (> 1).
--
-- 'Show' instances display the stored number in the given base. If the base has
-- a common prefix (e.g. @0x@ for hex), it is used.
newtype AsciiNat (base :: Natural) = AsciiNat { getAsciiNat :: Natural }
    deriving stock (Generic, Data)
    deriving (Eq, Ord) via Natural

instance Show (AsciiNat 2)  where showsPrec _ n = showString "0b" . showBin (getAsciiNat n)
instance Show (AsciiNat 8)  where showsPrec _ n = showString "0o" . showOct (getAsciiNat n)
instance Show (AsciiNat 10) where showsPrec _ n = showInt (getAsciiNat n)
instance Show (AsciiNat 16) where showsPrec _ n = showString "0x" . showHex (getAsciiNat n)

-- | Compare two 'AsciiNat's with arbitrary bases.
asciiNatCompare :: AsciiNat b1 -> AsciiNat b2 -> Ordering
asciiNatCompare (AsciiNat n1) (AsciiNat n2) = compare n1 n2

-- | The bytelength of an 'AsciiNat' is the number of digits in the number in
--   the given base. We can calculate this generically with great efficiency
--   using GHC primitives.
instance KnownNat base => BLen (AsciiNat base) where
    blen = Store.VarSize $ \(AsciiNat n) -> wordToBLen# (naturalSizeInBase# (naturalToWord# base) n)
      where base = natVal'' @base

--------------------------------------------------------------------------------

instance Put (AsciiNat 8) where
    put = natToAsciiBytes (+ 0x30) 8 . getAsciiNat

instance Get (AsciiNat 8) where
    get = do
        bs <- get
        case asciiBytesToNat octalFromAsciiDigit 8 bs of
          Left  w -> eBase $ EFailParse "hex ASCII natural" bs w
          Right n -> pure $ AsciiNat n

octalFromAsciiDigit :: Word8 -> Maybe Word8
octalFromAsciiDigit = \case
  0x30 -> Just 0
  0x31 -> Just 1
  0x32 -> Just 2
  0x33 -> Just 3
  0x34 -> Just 4
  0x35 -> Just 5
  0x36 -> Just 6
  0x37 -> Just 7
  _    -> Nothing

--------------------------------------------------------------------------------

-- TODO nice and slow I think
natToAsciiBytes :: (Word8 -> Word8) -> Natural -> Natural -> Store.Poke ()
natToAsciiBytes f base =
    traverse_ (\w -> put w) . fmap f . digits @Word8 base

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
