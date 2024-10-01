-- | Data null-padded to a given length.

{-# LANGUAGE UndecidableInstances #-} -- for PredicateName
{-# LANGUAGE OverloadedStrings #-} -- for refine error builder

module Binrep.Type.NullPadded where

import Binrep
import Bytezap.Poke qualified as BZ
import Bytezap.Struct qualified as BZ.Struct
import FlatParse.Basic qualified as FP
import Raehik.Compat.FlatParse.Basic.WithLength qualified as FP
import Control.Monad.Combinators ( skipCount )

import Rerefined.Predicate.Common
import Rerefined.Refine
import TypeLevelShow.Natural
import TypeLevelShow.Utils
import Data.Text.Builder.Linear qualified as TBL

import GHC.TypeNats
import Util.TypeNats ( natValInt )

import Bytezap.Parser.Struct qualified as BZG
import GHC.Exts ( Int(I#) )

data NullPad (n :: Natural)
instance Predicate (NullPad n) where
    type PredicateName d (NullPad n) = ShowParen (d > 9)
        ("NullPad " ++ ShowNatDec n)

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
instance (KnownPredicateName (NullPad n), BLen a, KnownNat n)
  => Refine (NullPad n) a where
    validate p a = validateBool p (len <= n) $
        "too long: " <> TBL.fromDec len <> " > " <> TBL.fromDec n
      where
        n = natValInt @n
        len = blen a

instance (BLen a, KnownNat n, Put a) => PutC (NullPadded n a) where
    putC ra = BZ.Struct.sequencePokes (BZ.toStructPoke (put a)) len
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
        BZG.constParse $ unsafeRefine a
      where
        !(I# len#) = natValInt @n

instance (Get a, KnownNat n) => Get (NullPadded n a) where
    get = do
        (a, len) <- FP.parseWithLength get
        let paddingLen = natValInt @n - len
        if   paddingLen < 0
        then err1 ["TODO used to be EOverlong, cba"]
        else do skipCount paddingLen (FP.word8 0x00)
                pure $ unsafeRefine a
