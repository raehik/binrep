-- | Data null-padded to a given length.

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

data NullPad (n :: Natural)

-- | A type which is to be null-padded to a given total length.
--
-- Given some @a :: 'NullPadded' n a@, it is guaranteed that
--
-- @
-- 'blen' a '<=' 'natValInt' \@n
-- @
--
-- thus
--
-- @
-- 'natValInt' \@n '-' 'blen' a '>=' 0
-- @
--
-- That is, the serialized stored data will not be longer than the total length.
--
-- The binrep instances are careful not to construct bytestrings unnecessarily.
type NullPadded n a = Refined (NullPad n) a

instance (BLen a, KnownNat n) => Predicate (NullPad n) a where
    validate p a
      | len <= n = success
      | otherwise
          = throwRefineOtherException (typeRep p) $
                   "too long: " <> tshow len <> " > " <> tshow n
      where
        n = natValInt @n
        len = blen a

instance IsCBLen (NullPadded n a) where type CBLen (NullPadded n a) = n
deriving via ViaCBLen (NullPadded n a) instance KnownNat n => BLen (NullPadded n a)

instance (BLen a, KnownNat n, PutC a) => PutC (NullPadded n a) where
    putC ra = BZ.Struct.sequencePokes (putC a) len
        (BZ.Struct.replicateByte paddingLen 0x00)
      where
        len = blen a
        a = unrefine ra
        paddingLen = natValInt @n - len
        -- ^ refinement guarantees >=0

instance (BLen a, KnownNat n, Put a) => Put (NullPadded n a) where
    put ra = put a <> BZ.replicateByte paddingLen 0x00
      where
        a = unrefine ra
        paddingLen = natValInt @n - blen a
        -- ^ refinement guarantees >=0

instance (Get a, KnownNat n) => Get (NullPadded n a) where
    get = do
        (a, len) <- FP.parseWithLength get
        let paddingLen = natValInt @n - len
        if   paddingLen < 0
        then eBase $ EFailNamed "TODO used to be EOverlong, cba"
        else do
            skipCount paddingLen (FP.word8 0x00)
            pure $ reallyUnsafeRefine a
