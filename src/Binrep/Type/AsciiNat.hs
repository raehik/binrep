{-| Naturals represented via ASCII digits.

A concept which sees occasional use in places where neither speed nor size
efficiency matter. The tar file format uses it, apparently to sidestep making a
decision on byte ordering. Pretty silly.

As with other binrep string-likes, you probably want to wrap this with
'Binrep.Type.Sized.Sized' or 'Binrep.Type.Prefix.Size.SizePrefixed'.

We use a refinement to permit using any numeric type, while ensuring that
negative values are not permitted.
-}

{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE OverloadedStrings #-} -- for refined error
{-# LANGUAGE UndecidableInstances #-} -- for deriving predicate instance

module Binrep.Type.AsciiNat where

import Binrep

import GHC.Exts ( Word(W#), Word#, Int(I#), word2Int#, eqWord#, plusWord# )
import Util.TypeNats ( natValWord )
import Data.Semigroup ( sconcat )

import GHC.Num.Primitives ( wordLogBase# )
import GHC.Num.Natural ( naturalSizeInBase# )

import Data.Word
import Data.Int
import Data.List.NonEmpty ( NonEmpty( (:|) ) )

import GHC.TypeNats ( Natural, KnownNat )

import Data.ByteString qualified as B
import Binrep.Type.Thin ( Thin(Thin) )

import Rerefined.Predicate
import Rerefined.Predicate.Via
import Rerefined.Predicate.Relational.Value
import Rerefined.Predicate.Relational
import Rerefined.Refine
import TypeLevelShow.Natural
import TypeLevelShow.Utils

import Data.Text.Builder.Linear qualified as TBL

-- | A natural represented in binary as an ASCII string, where each character is
--   a digit in the given base.
--
-- Only certain bases are supported: 2, 8, 10 and 16.
--
-- Hex parsing permits mixed case digits when parsing (@1-9a-fA-F@), and
-- serializes with lower-case ASCII hex digits.
data AsciiNat (base :: Natural)
--type AsciiNat base = Refined (AsciiNat base)

instance Predicate (AsciiNat base) where
    type PredicateName d (AsciiNat base) = ShowParen (d > 9)
        ("AsciiNat " ++ ShowNatDec base)

instance (KnownPredicateName (AsciiNat base), Num a, Ord a)
  => Refine (AsciiNat base) a where
    validate = validateVia @(CompareValue GTE Pos 0)

-- | Compare two 'AsciiNat's, ignoring base information.
asciiNatCompare
    :: Ord a => Refined (AsciiNat bl) a -> Refined (AsciiNat br) a -> Ordering
asciiNatCompare l r = compare (unrefine l) (unrefine r)

-- | The bytelength of an 'AsciiNat' is the number of digits in the number in
--   the given base. We can calculate this generally with great efficiency
--   using GHC (ghc-bignum) primitives!
instance (HasBaseOps a, KnownNat base) => BLen (Refined (AsciiNat base) a) where
    blen n = I# (word2Int# (sizeInBase# base# (unrefine n)))
      where
        !(W# base#) = natValWord @base

class HasBaseOps a where
    -- | See ghc-bignum internals at @GHC.Num.*@.
    sizeInBase# :: Word# -> a -> Word#

instance HasBaseOps Word    where sizeInBase# = sizeInBaseWordSize
instance HasBaseOps Natural where
    sizeInBase# base = \case
      0 -> 1##
      a -> naturalSizeInBase# base a

instance HasBaseOps Word8  where sizeInBase# = sizeInBaseWordSize
instance HasBaseOps Word16 where sizeInBase# = sizeInBaseWordSize
instance HasBaseOps Word32 where sizeInBase# = sizeInBaseWordSize

-- | TODO unsafe for 32-bit platform
instance HasBaseOps Word64 where sizeInBase# = sizeInBaseWordSize

instance HasBaseOps Int8   where sizeInBase# = sizeInBaseWordSize
instance HasBaseOps Int16  where sizeInBase# = sizeInBaseWordSize
instance HasBaseOps Int32  where sizeInBase# = sizeInBaseWordSize

-- | TODO unsafe for 32-bit platform
instance HasBaseOps Int64  where sizeInBase# = sizeInBaseWordSize

-- | 'Int' can use 'Word' size (but TODO what happens for negatives?)
instance HasBaseOps Int  where sizeInBase# = sizeInBaseWordSize

-- | Safe for types smaller than a 'Word'.
--
-- Uses ghc-bignum internals. Slightly unwrapped for better performance.
--
-- One could perhaps write faster algorithms for smaller primitive types too...
-- but performance increase would be minimal if even present.
sizeInBaseWordSize :: Integral a => Word# -> a -> Word#
sizeInBaseWordSize base a =
    case w# `eqWord#` 0## of
      1# -> 1##
      _  -> 1## `plusWord#` wordLogBase# base w#
  where
    !(W# w#) = fromIntegral a

-- | Serialize any term of an 'Integral' type to binary (base 2) ASCII.
instance Integral a => Put (Refined (AsciiNat  2) a) where
    put = sconcat . fmap (put . (+) 0x30) . unsafeDigits @Word8  2 . unrefine

-- | Serialize any term of an 'Integral' type to octal (base 8) ASCII.
instance Integral a => Put (Refined (AsciiNat  8) a) where
    put = sconcat . fmap (put . (+) 0x30) . unsafeDigits @Word8  8 . unrefine

-- | Serialize any term of an 'Integral' type to decimal (base 10) ASCII.
instance Integral a => Put (Refined (AsciiNat 10) a) where
    put = sconcat . fmap (put . (+) 0x30) . unsafeDigits @Word8 10 . unrefine

-- | Serialize any term of an 'Integral' type to hex (base 16) ASCII.
--
-- Uses lower-case ASCII.
instance Integral a => Put (Refined (AsciiNat 16) a) where
    put =
          sconcat . fmap (put . unsafeHexDigitToAsciiLower)
        . unsafeDigits @Word8 16 . unrefine

-- | Parse a  binary  (base 2) ASCII natural to any 'Num' type.
instance (Num a, Ord a) => Get (Refined (AsciiNat  2)  a) where
    get = unsafeRefine <$> getAsciiNatByByte 2  "binary"  parseBinaryAsciiDigit

-- | Parse an octal   (base 8) ASCII natural to any 'Num' type.
instance (Num a, Ord a) => Get (Refined (AsciiNat  8)  a) where
    get = unsafeRefine <$> getAsciiNatByByte 8  "octal"   parseOctalAsciiDigit

-- | Parse a  decimal (base 10) ASCII natural to any 'Num' type.
instance (Num a, Ord a) => Get (Refined (AsciiNat 10) a) where
    get = unsafeRefine <$> getAsciiNatByByte 10 "decimal" parseDecimalAsciiDigit

-- | Parse a  hex     (base 16) ASCII natural to any 'Num' type.
--
-- Parses lower and upper case (mixed permitted).
instance (Num a, Ord a) => Get (Refined (AsciiNat 16) a) where
    get = unsafeRefine <$> getAsciiNatByByte 16 "hex"     parseHexAsciiDigit

-- | Parse an ASCII natural in the given base with the given digit parser.
--
-- Parses byte-by-byte. As such, it only supports bases up to 256.
getAsciiNatByByte :: Num a => a -> TBL.Builder -> (a -> Maybe a) -> Getter a
getAsciiNatByByte base baseStr f = do
    Thin bs <- get -- no need to copy since we consume during parsing!
    if   B.null bs
    then err1 ["ASCII natural cannot be empty"]
    else case asciiBytesToNat f base bs of
          Left  b -> err1 [
            "non-"<>baseStr<>" ASCII digit in "
            <>baseStr<>" ASCII natural: "<>TBL.fromDec b]
          Right n -> pure n

{- | Get the digits in the given number as rendered in the given base.

Digits will be between 0-base. The return type must be sized to support this.

Base must be > 2. This is not checked. (Internal function eh.)

Note the 'NonEmpty' return type. Returns @[0]@ for 0 input. (This does not match
ghc-bignum's @sizeInBase@ primitives!)
-}
unsafeDigits :: forall b a. (Integral a, Integral b) => a -> a -> NonEmpty b
unsafeDigits base = go []
  where
    go s x = loop (head' :| s) tail'
      where
        head' = fromIntegral (x `mod` base)
        tail' = x `div` base
    loop s@(r :| rs) = \case
        0 -> s
        x -> go (r : rs) x

asciiBytesToNat
    :: Num a => (a -> Maybe a) -> a -> B.ByteString -> Either Word8 a
asciiBytesToNat f base bs =
    -- we use Int for exponent because it seems most sensible & gets SPECIALISEd
    case B.foldr go (Right (0, (0 :: Int))) bs of
      Left w -> Left w
      Right (n, _) -> Right n
  where
    go _ (Left w) = Left w
    go w (Right (n, expo)) =
        case f (fromIntegral w) of
          Nothing -> Left w
          Just d  -> Right (n + d * base^expo, expo+1)

parseBinaryAsciiDigit :: (Num a, Ord a) => a -> Maybe a
parseBinaryAsciiDigit = \case
  0x30 -> Just 0 -- 0
  0x31 -> Just 1 -- 1
  _    -> Nothing

parseOctalAsciiDigit :: (Num a, Ord a) => a -> Maybe a
parseOctalAsciiDigit a
  | a >= 0x30 && a <= 0x37 = Just $ a - 0x30 -- 0-7
  | otherwise = Nothing

parseDecimalAsciiDigit :: (Num a, Ord a) => a -> Maybe a
parseDecimalAsciiDigit a
  | a >= 0x30 && a <= 0x39 = Just $ a - 0x30 -- 0-9
  | otherwise = Nothing

parseHexAsciiDigit :: (Num a, Ord a) => a -> Maybe a
parseHexAsciiDigit a
  | a >= 0x30 && a <= 0x39 = Just $ a - 0x30 -- 0-9
  | a >= 0x41 && a <= 0x46 = Just $ a - 0x37 -- A-F (upper case)
  | a >= 0x61 && a <= 0x66 = Just $ a - 0x57 -- a-f (lower case)
  | otherwise = Nothing

-- | May only be called with 0<=n<=15.
unsafeHexDigitToAsciiLower :: (Num a, Ord a) => a -> a
unsafeHexDigitToAsciiLower a
  | a <= 9    = 0x30 + a
  | otherwise = 0x57 + a

{-

-- | Print a binary (base 2) ASCII natural with an @0b@ prefix.
prettyAsciiNat2 :: Integral a => Int -> a -> ShowS
prettyAsciiNat2 _ n = showString "0b" . showBin n

-- | Show binary (base 2) ASCII naturals with an @0b@ prefix.
instance Integral a => Show (AsciiNat 2 a) where
    showsPrec _ n = showString "0b" . showBin (unAsciiNat n)

-- | Show octal (base 8) ASCII naturals with an @0o@ prefix.
instance Integral a => Show (AsciiNat 8  a) where
    showsPrec _ n = showString "0o" . showOct (unAsciiNat n)

-- | Show decimal (base 10) ASCII naturals with no prefix.
instance Integral a => Show (AsciiNat 10 a) where
    showsPrec _ = showInt . unAsciiNat

-- | Show hex (base 16) ASCII naturals with an @0x@ prefix.
instance Integral a => Show (AsciiNat 16 a) where
    showsPrec _ n = showString "0x" . showHex (unAsciiNat n)

-}
