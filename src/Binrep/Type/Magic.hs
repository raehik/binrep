{- | Magic numbers (also just magic): short constant bytestrings usually
     found at the top of a file, included as a safety check for parsing.

There are two main flavors of magics:

  * "random" bytes e.g. Zstandard: @28 B5 2F FD@
  * printable ASCII bytes e.g. Ogg: @4F 67 67 53@ -> OggS

We allow both. Here is how to use them.

For bytewise magics, use type-level 'Natural' lists:

>>> :k! MagicBytes '[0x4F, 0x67, 0x67, 0x53]
MagicBytes '[0x4F, 0x67, 0x67, 0x53] :: [Byte]
= '[ 'B4F, 'B67, 'B67, 'B53]

You will get a type error if you try to use a natural larger than @0xFF@.
Alternatively, you may use the representationally-safe 'Byte' kind:

>>> :k! MagicBytes '[ 'B4F]
MagicBytes '[ 'B4F] :: [Byte]
= '[ 'B4F]

For ASCII magics, use 'Symbol's (type-level strings):

>>> :k! MagicBytes "OggS"
MagicBytes "OggS" :: [Byte]
= '[ 'B4F, 'B67, 'B67, 'B53]

String magics are restricted to ASCII, and will type error if not. If you really
want them, please read 'Binrep.Type.Magic.UTF8'.
-}

module Binrep.Type.Magic where

import Binrep
import Binrep.Type.Byte
import Binrep.Type.Byte.TypeLevel

import GHC.TypeLits
import Data.ByteString qualified as B
import FlatParse.Basic qualified as FP

import GHC.Generics ( Generic )
import Data.Data ( Data )

data Magic (a :: k) = Magic
    deriving stock (Generic, Data, Show, Eq)

type instance CBLen (Magic a) = Length (MagicBytes a)
deriving anyclass instance KnownNat (Length (MagicBytes a)) => BLen (Magic a)

instance (bs ~ MagicBytes a, ByteVals bs) => Put (Magic a) where
    put Magic = put $ byteVals @bs

-- TODO improve show - maybe hexbytestring goes here? lol
instance (bs ~ MagicBytes a, ByteVals bs) => Get (Magic a) where
    get = do
        let expected = byteVals @bs
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

type family Length (a :: [k]) :: Natural where
    Length '[]       = 0
    Length (a ': as) = 1 + Length as

type family SymbolAsCharList (a :: Symbol) :: [Char] where
    SymbolAsCharList a = SymbolAsCharList' (UnconsSymbol a)

type family SymbolAsCharList' (a :: Maybe (Char, Symbol)) :: [Char] where
    SymbolAsCharList' 'Nothing = '[]
    SymbolAsCharList' ('Just '(c, s)) = c ': SymbolAsCharList' (UnconsSymbol s)

type family SymbolUnicodeCodepoints (a :: Symbol) :: [Natural] where
    SymbolUnicodeCodepoints a = CharListUnicodeCodepoints (SymbolAsCharList a)

type family CharListUnicodeCodepoints (a :: [Char]) :: [Natural] where
    CharListUnicodeCodepoints '[]       = '[]
    CharListUnicodeCodepoints (c ': cs) = CharToNat c ': CharListUnicodeCodepoints cs

type family NatsToBytes (a :: [Natural]) :: [Byte] where
    NatsToBytes '[]       = '[]
    NatsToBytes (n ': ns) = NatToByte' n ': NatsToBytes ns

type family MagicBytes (a :: k) :: [Byte]

type instance MagicBytes (a :: [Byte]) = a

-- | Only permits ASCII.
type instance MagicBytes (a :: Symbol) = NatsToBytes (SymbolUnicodeCodepoints a)

-- | Only permits values between 0-255 (0x00-0xFF).
type instance MagicBytes (a :: [Natural]) = NatsToBytes a
