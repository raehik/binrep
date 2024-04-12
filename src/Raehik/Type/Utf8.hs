{-# LANGUAGE UndecidableInstances #-} -- naturally

-- | Type-level UTF-8 codepoint conversion.

module Raehik.Type.Utf8
  ( type SymbolToUtf8
  , type CharToUtf8
  , type UnicodeCodePointToUtf8
  ) where

import GHC.TypeLits
import Raehik.Type.Common ( type IfNatLte, type (++) )

-- | Convert a type-level symbol to UTF-8.
--
-- Each 'Natural' in the return list is "byte-sized" (0 <= n <= 0xFF).
type SymbolToUtf8 sym = SymbolToUtf8' '[] (UnconsSymbol sym)

type family SymbolToUtf8' bs (mchsym :: Maybe (Char, Symbol)) where
    SymbolToUtf8' bs ('Just '(ch, sym)) =
        SymbolToUtf8' (bs ++ CharToUtf8 ch) (UnconsSymbol sym)
    SymbolToUtf8' bs 'Nothing = bs

-- | Convert a type-level character to UTF-8.
--
-- Each 'Natural' in the return list is "byte-sized" (0 <= n <= 0xFF).
type CharToUtf8 ch = UnicodeCodePointToUtf8 (CharToNat ch)

-- | Convert a type-level Unicode code point to UTF-8
--
-- Emits a type error if you give it an invalid codepoint.
type UnicodeCodePointToUtf8 n =
    IfNatLte n 0x7F '[n]
      ( IfNatLte n 0x07FF (UCP2 n)
        ( IfNatLte n 0xFFFF (UCP3 n)
          ( IfNatLte n 0x10FFFF (UCP4 n)
            ( TypeError ('Text "not a Unicode codepoint: " :<>: 'ShowType n)
      ))))

type UCP2 n = [UCP21 n, UCP22 n]
type UCP21 n = 0b11000000 + (n `Div` (2^6))
type UCP22 n = 0b10000000 + (n `Mod` (2^6))

type UCP3 n = [UCP31 n, UCP32 n, UCP33 n]
type UCP31 n = 0b11100000 + (n `Div` (2^12))
type UCP32 n = 0b10000000 + ((n `Div` (2^6)) `Mod` (2^6))
type UCP33 n = 0b10000000 + (n `Mod` (2^6))

type UCP4 n = [UCP41 n, UCP42 n, UCP43 n, UCP44 n]
type UCP41 n = 0b11110000 + (n `Div` (2^18))
type UCP42 n = 0b10000000 + ((n `Div` (2^12)) `Mod` (2^12))
type UCP43 n = 0b10000000 + ((n `Div` (2^6)) `Mod` (2^6))
type UCP44 n = 0b10000000 + (n `Mod` (2^6))
