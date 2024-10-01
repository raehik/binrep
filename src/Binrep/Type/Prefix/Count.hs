{-# LANGUAGE UndecidableInstances #-} -- required for type-level stuff
{-# LANGUAGE OverloadedStrings #-} -- required for refined errors

module Binrep.Type.Prefix.Count where

import Binrep.Type.Prefix.Internal
import Binrep
import Control.Monad.Combinators qualified as Monad

import GHC.TypeNats
import Util.TypeNats ( natValInt )

import Rerefined.Predicate.Common
import Rerefined.Refine
import TypeLevelShow.Utils

import Data.Kind ( type Type )

import Data.Foldable qualified as Foldable

-- TODO put monofoldable in here, instead of that useless @(f a)@ stuff

data CountPrefix (pfx :: Type)

instance Predicate (CountPrefix pfx) where
    type PredicateName d (CountPrefix pfx) = ShowParen (d > 9)
        ("CountPrefix " ++ LenNatName pfx)

instance
  ( KnownPredicateName (CountPrefix pfx), KnownNat (LenNatMax pfx), Foldable f
  ) => Refine1 (CountPrefix pfx) f where
    validate1 p fa =
        validateBool p (Foldable.length fa <= natValInt @(LenNatMax pfx)) $
            "TODO too large for count prefix"

instance
  ( KnownPredicateName (CountPrefix pfx), KnownNat (LenNatMax pfx), Foldable f
  ) => Refine (CountPrefix pfx) (f a) where
    validate = validate1

type CountPrefixed pfx = Refined1 (CountPrefix pfx)

-- | We can know byte length at compile time /if/ we know it for the prefix and
--   the list-like.
--
-- This is extremely unlikely, because then what counting are we even
-- performing for the list-like? But it's a valid instance.
instance IsCBLen (CountPrefixed pfx f a) where
    type CBLen (CountPrefixed pfx f a) = CBLen pfx + CBLen (f a)

-- | The byte length of a count-prefixed type is the length of the prefix type
--   (holding the length of the type) plus the length of the type.
--
-- Bit confusing. How to explain this? TODO
instance (LenNat pfx, Foldable f, BLen pfx, BLen (f a))
  => BLen (CountPrefixed pfx f a) where
    blen rfa = blen (lenToNat @pfx (Foldable.length fa)) + blen fa
      where fa = unrefine1 rfa

instance (LenNat pfx, Foldable f, Put pfx, Put (f a))
  => Put (CountPrefixed pfx f a) where
    put rfa = put (lenToNat @pfx (Foldable.length fa)) <> put fa
      where fa = unrefine1 rfa

class GetCount f where getCount :: Get a => Int -> Getter (f a)
instance GetCount [] where getCount n = Monad.count n get

instance (LenNat pfx, GetCount f, Get pfx, Get a)
  => Get (CountPrefixed pfx f a) where
    get = do
        pfx <- get @pfx
        fa <- getCount (natToLen pfx)
        pure $ unsafeRefine1 fa
