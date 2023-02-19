{-# LANGUAGE OverloadedStrings #-}

module Binrep.Type.NullPadded where

import Binrep
import Bytezap.Bytes qualified as BZ
import FlatParse.Basic qualified as FP
import Control.Monad.Combinators qualified as Monad

import Binrep.Util ( tshow )

import Refined
import Refined.Unsafe
import Data.Typeable ( typeRep )

import GHC.TypeNats
import Util.TypeNats ( natValInt )

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

instance (BLen a, Put a, KnownNat n) => Put (NullPadded n a) where
    put ra = put a <> BZ.pokeByteReplicate paddingLen 0x00
      where
        a = unrefine ra
        paddingLen = natValInt @n - blen a
        -- ^ refinement guarantees >=0

instance (BLen a, Get a, KnownNat n) => Get (NullPadded n a) where
    get = do
        a <- get
        let paddingLen = natValInt @n - blen a
        if   paddingLen < 0
        then eBase $ EFailNamed "TODO used to be EOverlong, cba"
        else do
            Monad.skipCount paddingLen (FP.word8 0x00)
            pure $ reallyUnsafeRefine a
