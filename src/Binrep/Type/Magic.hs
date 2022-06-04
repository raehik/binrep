{- | Magic numbers (also just magic): short constant bytestrings usually
     found at the top of a file, included as a safety check for parsing.

TODO rename: MagicBytes -> MagicVals, and have ByteVal be a "consumer" of
MagicVals where each value must be a byte. (It's conceivable that we have
another consumer which makes each value into a non-empty list of bytes, LE/BE.)

There are two main flavors of magics:

  * "random" bytes e.g. Zstandard: @28 B5 2F FD@
  * printable ASCII bytes e.g. Ogg: @4F 67 67 53@ -> OggS

For bytewise magics, use type-level 'Natural' lists.
For ASCII magics, use 'Symbol's (type-level strings).

Previously, I squashed these into a representationally-safe type. Now the check
only occurs during reification. So you are able to define invalid magics now
(bytes over 255, non-ASCII characters), and potentially use them, but you'll get
a clear type error like "no instance for ByteVal 256" when attempting to reify.

String magics are restricted to ASCII, and will type error during reification
otherwise. If you really want UTF-8, please read 'Binrep.Type.Magic.UTF8'.
-}

module Binrep.Type.Magic where

import Binrep
import Binrep.Type.Byte

import GHC.TypeLits
import Data.ByteString qualified as B
import FlatParse.Basic qualified as FP

import GHC.Generics ( Generic )
import Data.Data ( Data )

import Mason.Builder qualified as Mason

data Magic (a :: k) = Magic
    deriving stock (Generic, Data, Show, Eq)

-- | Assumes magic values are individual bytes.
type instance CBLen (Magic a) = Length (MagicVals a)

-- | Assumes magic values are individual bytes.
deriving anyclass instance KnownNat (Length (MagicVals a)) => BLen (Magic a)

-- | Forces magic values to be individual bytes.
instance (bs ~ MagicVals a, ByteVals bs) => Put (Magic a) where
    put Magic = byteVals @bs

-- | Forces magic values to be individual bytes.
--
-- TODO improve show - maybe hexbytestring goes here? lol
instance (bs ~ MagicVals a, ByteVals bs) => Get (Magic a) where
    get = do
        let expected = Mason.toStrictByteString $ byteVals @bs
        actual <- FP.take $ B.length expected
        if   actual == expected
        then return Magic
        else FP.err $ "bad magic: expected "<>show expected<>", got "<>show actual

{-
I do lots of functions on lists, because they're structurally simple. But you
can't pass type-level functions as arguments between type families. singletons
solves a related (?) problem using defunctionalization, where you manually write
out the function applications or something. Essentially, you can't do this:

    type family Map (f :: x -> y) (a :: [x]) :: [y] where
        Map _ '[]       = '[]
        Map f (a ': as) = f a ': Map f as

So you have to write that out for every concrete function over lists.
-}

type family MagicVals (a :: k) :: [Natural]
type instance MagicVals (a :: Symbol)    = SymbolUnicodeCodepoints a
type instance MagicVals (a :: [Natural]) = a

type family SymbolUnicodeCodepoints (a :: Symbol) :: [Natural] where
    SymbolUnicodeCodepoints a = CharListUnicodeCodepoints (SymbolAsCharList a)

type family CharListUnicodeCodepoints (a :: [Char]) :: [Natural] where
    CharListUnicodeCodepoints '[]       = '[]
    CharListUnicodeCodepoints (c ': cs) = CharToNat c ': CharListUnicodeCodepoints cs

type family SymbolAsCharList (a :: Symbol) :: [Char] where
    SymbolAsCharList a = SymbolAsCharList' (UnconsSymbol a)

type family SymbolAsCharList' (a :: Maybe (Char, Symbol)) :: [Char] where
    SymbolAsCharList' 'Nothing = '[]
    SymbolAsCharList' ('Just '(c, s)) = c ': SymbolAsCharList' (UnconsSymbol s)
