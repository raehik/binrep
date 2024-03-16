{-# LANGUAGE UndecidableInstances #-} -- for weirder type families

{- | Magic numbers (also just magic): short constant bytestrings usually
     found at the top of a file, often used as an early sanity check.

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
import FlatParse.Basic qualified as FP
import Data.ByteString qualified as B
import Util.TypeNats ( natValInt )

import GHC.TypeLits

import GHC.Generics ( Generic )
import Data.Data ( Data )

import Strongweak

-- | A singleton data type representing a "magic number" via a phantom type.
--
-- The phantom type variable unambiguously defines a constant bytestring.
-- A handful of types are supported for using magics conveniently, e.g. for pure
-- ASCII magics, you may use a 'Symbol' type-level string.
data Magic (a :: k) = Magic deriving stock (Generic, Data, Show, Eq)

-- | Weaken a @'Magic' a@ to the unit.
instance Weaken (Magic a) where
    type Weak (Magic a) = ()
    weaken Magic = ()

-- | Strengthen the unit to some @'Magic' a@.
instance Strengthen (Magic a) where strengthen () = pure Magic

-- | The byte length of a magic is known at compile time.
instance IsCBLen (Magic a) where type CBLen (Magic a) = Length (MagicBytes a)

-- | The byte length of a magic is obtained via reifying.
deriving via ViaCBLen (Magic a) instance
    KnownNat (Length (MagicBytes a)) => BLen (Magic a)

instance (bs ~ MagicBytes a, ReifyBytes bs) => Put (Magic a) where
    put Magic = reifyBytes @bs

instance (bs ~ MagicBytes a, ReifyBytes bs, KnownNat (Length bs))
  => Get (Magic a) where
    get = do
        -- Nice case where we _want_ flatparse's no-copy behaviour, because
        -- 'actual' is only in scope for this parser. Except, of course, if we
        -- error, in which case _now_ we copy. Efficient!
        actual <- FP.take (natValInt @(Length bs))
        -- silly optimization: we could skip comparing lengths because we know
        -- they must be the same. very silly though
        if   actual == expected
        then pure magic
        else eBase $ EExpected expected (B.copy actual)
      where
        expected = runPut magic
        magic = Magic :: Magic a

-- TODO might wanna move this
-- | The length of a type-level list.
type family Length (a :: [k]) :: Natural where
    Length '[]       = 0
    Length (a ': as) = 1 + Length as

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

--------------------------------------------------------------------------------

-- | Types which define a magic value.
class Magical (a :: k) where
    -- | How to turn the type into a list of bytes.
    type MagicBytes a :: [Natural]

-- | Type-level naturals go as-is. (Make sure you don't go over 255, though!)
instance Magical (ns :: [Natural]) where type MagicBytes ns = ns

-- | Type-level symbols are turned into their Unicode codepoints - but
--   multibyte characters aren't handled, so they'll simply be overlarge bytes,
--   which will fail further down.
instance Magical (sym :: Symbol) where
    type MagicBytes sym = SymbolUnicodeCodepoints sym
