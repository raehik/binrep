-- | Data null-padded to a given length.

{- TODO
Null padding using the underlying type's instances doesn't necessarily work.
'ByteString's must parse until the end of the string.
Or maybe that's correct, and we must use null terminated bytestrings with null
padding...? Huh.

...well, doing that fixes my issue. And thinking about it, I imagine that's how
C does it (you're still going to be wanting to deal with cstrings regardless of
null padding). Cool!!

OK, all good. But because of that, I should provide a convenience wrapper to put
nullpad+nullterm together.
-}

{-# LANGUAGE OverloadedStrings #-}

module Binrep.Type.NullPadded where

import Binrep
import Bytezap.Poke qualified as BZ
import Bytezap.Struct qualified as BZ.Struct
import FlatParse.Basic qualified as FP
import Raehik.Compat.FlatParse.Basic.WithLength qualified as FP
import Control.Monad.Combinators ( skipCount )

import Binrep.Util ( tshow )

import Refined
import Refined.Unsafe

import GHC.TypeNats
import Util.TypeNats ( natValInt )

import Data.Typeable ( typeRep )

import Bytezap.Parser.Struct qualified as BZG
import GHC.Exts ( Int(I#) )

data NullPad (n :: Natural)

{- | A type which is to be null-padded to a given total length.

Given some @a :: 'NullPadded' n a@, it is guaranteed that

@
'blen' a '<=' 'natValInt' \@n
@

thus

@
'natValInt' \@n '-' 'blen' a '>=' 0
@

That is, the serialized stored data will not be longer than the total length.
-}
type NullPadded n a = Refined (NullPad n) a

instance IsCBLen (NullPadded n a) where type CBLen (NullPadded n a) = n
deriving via ViaCBLen (NullPadded n a) instance KnownNat n => BLen (NullPadded n a)

-- | Assert that term will fit.
instance (BLen a, KnownNat n) => Predicate (NullPad n) a where
    validate p a
      | len <= n = success
      | otherwise
          = throwRefineOtherException (typeRep p) $
                   "too long: " <> tshow len <> " > " <> tshow n
      where
        n = natValInt @n
        len = blen a

instance (BLen a, KnownNat n, PutC a) => PutC (NullPadded n a) where
    putC ra = BZ.Struct.sequencePokes (putC a) len
        (BZ.Struct.replicateByte paddingLen 0x00)
      where
        a = unrefine ra
        len = blen a
        paddingLen = natValInt @n - len
        -- ^ refinement guarantees >=0

instance (BLen a, KnownNat n, Put a) => Put (NullPadded n a) where
    put ra = put a <> BZ.replicateByte paddingLen 0x00
      where
        a = unrefine ra
        paddingLen = natValInt @n - blen a
        -- ^ refinement guarantees >=0

-- | Run a @Getter a@ isolated to @n@ bytes.
instance (KnownNat n, Get a) => GetC (NullPadded n a) where
    getC = fpToBz get len# $ \a _unconsumed# ->
        -- TODO consume nulls lol
        BZG.constParse $ reallyUnsafeRefine a
      where
        !(I# len#) = natValInt @n

instance (Get a, KnownNat n) => Get (NullPadded n a) where
    get = do
        (a, len) <- FP.parseWithLength get
        let paddingLen = natValInt @n - len
        if   paddingLen < 0
        then eBase $ EFailNamed "TODO used to be EOverlong, cba"
        else do
            skipCount paddingLen (FP.word8 0x00)
            pure $ reallyUnsafeRefine a
