{-# LANGUAGE UndecidableInstances #-} -- for tons of stuff
{-# LANGUAGE PatternSynonyms #-} -- TODO wip
{-# LANGUAGE OverloadedStrings #-} -- for easy error building

{- | Magic numbers (also just magic): short constant bytestrings usually
     found at the top of a file, often used as an early sanity check.

There are two main flavors of magics:

  * byte magics e.g. Zstandard: @28 B5 2F FD@
  * printable magics e.g. Ogg: @4F 67 67 53@ -> @OggS@ (in ASCII)

For byte magics, use type-level 'Natural' lists e.g. @'Magic' \@'[0xFF, 0x01]@
For printable (UTF-8) magics, use 'Symbol's e.g. @'Magic' \@"hello"@.
-}

module Binrep.Type.Magic
  ( Magic(Magic)
  , Magical(type MagicBytes)
  , type Length
  ) where

import Data.Type.Symbol.Utf8 ( type SymbolToUtf8 )

import GHC.TypeLits ( type Natural, type Symbol, type KnownNat, type (+) )

import GHC.Generics ( Generic )
import Data.Data ( Data )
import Strongweak

import Binrep
import Bytezap.Struct.TypeLits.Bytes ( ReifyBytesW64(reifyBytesW64) )
import Bytezap.Parser.Struct.TypeLits.Bytes
  ( ParseReifyBytesW64(parseReifyBytesW64) )
import Bytezap.Parser.Struct qualified as BZ
import GHC.Exts ( Int(I#) )
import FlatParse.Basic qualified as FP
import Data.Text.Builder.Linear qualified as TBL

{- | A unit data type representing a "magic number" via a phantom type.

The phantom type unambiguously defines a bytestring at compile time. This
depends on the type's kind. See 'MagicBytes' for details.

This is defined using GADT syntax to permit labelling the phantom type kind as
/inferred/, which effectively means hidden (not available for visible type
applications). That kind is always evident from the type, so it's just nicer.
-}
data Magic a where Magic :: forall {k} (a :: k). Magic a
    deriving stock (Generic, Data, Show, Eq)

-- | Weaken a @'Magic' a@ to the unit '()'.
instance Weaken   (Magic a) where
    type Weakened (Magic a) = ()
    weaken Magic = ()

-- | Strengthen the unit '()' to some @'Magic' a@.
instance Strengthen (Magic a) where strengthen () = Right Magic

-- | The byte length of a magic is known at compile time.
instance IsCBLen (Magic a) where type CBLen (Magic a) = Length (MagicBytes a)

deriving via ViaCBLen (Magic a) instance
    KnownNat (Length (MagicBytes a)) => BLen (Magic a)

-- | Efficiently serialize a @'Magic' a@.
instance (bs ~ MagicBytes a, ReifyBytesW64 bs) => PutC (Magic a) where
    putC Magic = reifyBytesW64 @bs

deriving via (ViaPutC (Magic a)) instance
  (bs ~ MagicBytes a, ReifyBytesW64 bs, KnownNat (Length bs)) => Put (Magic a)

-- | Efficiently parse a @'Magic' a@. Serialization constraints are included as
--   we emit the expected bytestring in errors.
instance (bs ~ MagicBytes a, ParseReifyBytesW64 0 bs) => GetC (Magic a) where
    getC = BZ.ParserT $ \fpc base# os# st0 ->
        case BZ.runParserT# (parseReifyBytesW64 @0 @bs) fpc base# os# st0 of
          BZ.OK#   st1 ()             -> BZ.OK#   st1 Magic
          BZ.Err#  st1 (pos, bActual) -> BZ.Err#  st1 (parseError1
            ["TODO magic parse error: "<>TBL.fromDec bActual]
            (pos + I# os#))
          BZ.Fail# st1                -> BZ.Fail# st1 -- shouldn't occur

deriving via ViaGetC (Magic a) instance
  ( bs ~ MagicBytes a, ParseReifyBytesW64 0 bs
  , ReifyBytesW64 bs, KnownNat (Length bs)
  ) => Get (Magic a)

-- | Types which define a magic value.
class Magical (a :: k) where
    -- | How to turn the type into a list of bytes (stored using 'Natural's).
    type MagicBytes a :: [Natural]

-- | Type-level naturals go as-is. (Make sure you don't go over 255, though!)
instance Magical (bs :: [Natural]) where type MagicBytes bs = bs

-- | Type-level symbols are converted to UTF-8.
instance Magical (sym :: Symbol) where type MagicBytes sym = SymbolToUtf8 sym

-- | The length of a type-level list.
type family Length (a :: [k]) :: Natural where
    Length (a ': as) = 1 + Length as
    Length '[]       = 0
