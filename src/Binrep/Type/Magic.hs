{-# LANGUAGE UndecidableInstances #-} -- for weirder type families

{- | Magic numbers (also just magic): short constant bytestrings usually
     found at the top of a file, often used as an early sanity check.

There are two main flavors of magics:

  * byte magics e.g. Zstandard: @28 B5 2F FD@
  * printable magics e.g. Ogg: @4F 67 67 53@ -> @OggS@ (in ASCII)

For byte magics, use type-level 'Natural' lists.
For printable magics, use 'Symbol's (type-level strings).
-}

module Binrep.Type.Magic
  ( Magic(Magic)
  , Magical(type MagicBytes)
  , type Length
  ) where

import Raehik.Type.Utf8 ( type SymbolToUtf8 )

import Util.TypeNats ( natValInt )
import GHC.TypeLits ( type Natural, type Symbol, type KnownNat, type (+) )

import GHC.Generics ( Generic )
import Data.Data ( Data )
import Strongweak

import Binrep
import Bytezap.Struct.TypeLits.Bytes ( ReifyBytesW64(reifyBytesW64) )
import Bytezap.Parser.Struct.TypeLits.Bytes
  ( ParseReifyBytesW64(parseReifyBytesW64) )
import Bytezap.Parser.Struct qualified as BZ
import Data.ByteString.Internal qualified as B
import GHC.Exts ( Int(I#), plusAddr#, Ptr(Ptr) )
import Foreign.Marshal.Utils ( copyBytes )
import FlatParse.Basic qualified as FP

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

instance (bs ~ MagicBytes a, ReifyBytesW64 bs) => PutC (Magic a) where
    putC Magic = reifyBytesW64 @bs

deriving via (ViaPutC (Magic a)) instance
  (bs ~ MagicBytes a, ReifyBytesW64 bs, KnownNat (Length bs)) => Put (Magic a)

{- this works, but is ugly.
* we have to duplicate our error wrapping because errors use parser internals
* we throw the magic into the error, so we need the serializer constraints too
I mean, it's fine. It's correct. It's as fast as possible. But it looks bad :<
-}
instance
  ( bs ~ MagicBytes a, ParseReifyBytesW64 bs
  , ReifyBytesW64 bs, KnownNat (Length bs)
  ) => GetC (Magic a) where
    getC = BZ.ParserT $ \fpc base os# st0 ->
        case BZ.runParserT# (parseReifyBytesW64 @bs) fpc base os# st0 of
          BZ.OK#   st1 () -> BZ.OK#  st1 Magic
          BZ.Fail# st1    ->
            let bsActual = B.unsafeCreate len (\buf -> copyBytes buf (Ptr (base `plusAddr#` os#)) len)
                eb = EExpected bsExpected bsActual
            in  BZ.Err# st1 (E (I# os#) $ EBase eb)
          BZ.Err#  st1 e  ->
            let bsActual = B.unsafeCreate len (\buf -> copyBytes buf (Ptr (base `plusAddr#` os#)) len)
                eb = EExpected bsExpected bsActual
            in  BZ.Err# st1 (E (I# os#) $ EAnd e eb)
      where
        len = natValInt @(Length bs)
        bsExpected = runPutC (Magic :: Magic a)

deriving via ViaGetC (Magic a) instance
  ( bs ~ MagicBytes a, ParseReifyBytesW64 bs
  , ReifyBytesW64 bs, KnownNat (Length bs)
  ) => Get (Magic a)

-- | Types which define a magic value.
class Magical (a :: k) where
    -- | How to turn the type into a list of bytes.
    type MagicBytes a :: [Natural]

-- | Type-level naturals go as-is. (Make sure you don't go over 255, though!)
instance Magical (ns :: [Natural]) where type MagicBytes ns = ns

-- | Type-level symbols are turned into UTF-8.
instance Magical (sym :: Symbol) where type MagicBytes sym = SymbolToUtf8 sym

-- | The length of a type-level list.
type family Length (a :: [k]) :: Natural where
    Length '[]       = 0
    Length (a ': as) = 1 + Length as
