{-# LANGUAGE AllowAmbiguousTypes #-}

module Binrep.Type.AsciiNat where

import Binrep

import Data.Word ( Word8 )
import Data.List.NonEmpty ( NonEmpty( (:|) ) )
import Mason.Builder qualified as Mason
import Data.ByteString qualified as B
import Data.Semigroup ( sconcat )

import GHC.Exts ( proxy#, Proxy# )
import GHC.TypeNats ( Natural, KnownNat, natVal' )
import GHC.Num.Natural ( naturalSizeInBase#, naturalToWord#, Natural(NS) )

import GHC.Generics ( Generic )
import Data.Data ( Typeable, Data )
import Numeric ( showOct, showHex, showBin, showInt )

-- | A 'Natural' represented in binary as an ASCII string, where each character
--   a is a digit in the given base (> 1).
--
-- The 'Show' instances show the stored number in the given base.
newtype AsciiNat (n :: Natural) = AsciiNat { getAsciiNat :: Natural }
    deriving stock (Generic, Typeable, Data)
    deriving Eq via Natural

-- TODO provide function to check underlying equality, ignoring type-level

instance Show (AsciiNat 2)  where showsPrec _ = showBin . getAsciiNat
instance Show (AsciiNat 8)  where showsPrec _ = showOct . getAsciiNat
instance Show (AsciiNat 10) where showsPrec _ = showInt . getAsciiNat
instance Show (AsciiNat 16) where showsPrec _ = showHex . getAsciiNat

-- | We can calculate this with supreme efficiency using GHC primitives.
instance KnownNat n => BLen (AsciiNat n) where
    blen (AsciiNat n) = NS (naturalSizeInBase# (naturalToWord# base) n)
      where base = natVal' (proxy# :: Proxy# n)

instance Put (AsciiNat 8) where
    put = natToAsciiBytes (+ 0x30) 8 . getAsciiNat

instance Get (AsciiNat 8) where
    get = do
        bs <- get
        case asciiBytesToNat octalFromAsciiDigit 8 bs of
          Left bs' -> fail $ "TODO " <> show bs'
          Right n  -> return $ AsciiNat n

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

