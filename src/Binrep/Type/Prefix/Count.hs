{-# LANGUAGE UndecidableInstances #-} -- required for type-level stuff
{-# LANGUAGE OverloadedStrings #-} -- required for refined errors

module Binrep.Type.Prefix.Count where

import Binrep.Type.Prefix
import Binrep
import Control.Monad.Combinators qualified as Monad

import GHC.TypeNats
import Util.TypeNats ( natValInt )
import Refined hiding ( Weaken(..), strengthen )
import Refined.Unsafe ( reallyUnsafeRefine1 )

import Data.Typeable ( Typeable, typeRep )
import Data.Kind

import Data.Foldable qualified as Foldable

data CountPrefix (pfx :: Type)
type CountPrefixed pfx = Refined1 (CountPrefix pfx)

instance (KnownNat (Max pfx), Foldable f, Typeable pfx)
  => Predicate1 (CountPrefix pfx) f where
    validate1 p fa
      | Foldable.length fa <= natValInt @(Max pfx) = success
      | otherwise = throwRefineOtherException (typeRep p) "TODO bad"

instance (KnownNat (Max pfx), Foldable f, Typeable pfx)
  => Predicate (CountPrefix pfx) (f a) where
    validate = validate1

-- TODO no idea if this is sensible
instance IsCBLen (CountPrefixed pfx f a) where
    type CBLen (CountPrefixed pfx f a) = CBLen pfx + CBLen (f a)

instance (Prefix pfx, Foldable f, BLen pfx, BLen (f a))
  => BLen (CountPrefixed pfx f a) where
    blen rfa = blen (lenToPfx @pfx (Foldable.length fa)) + blen fa
      where fa = unrefine1 rfa

instance (Prefix pfx, Foldable f, Put pfx, Put (f a))
  => Put (CountPrefixed pfx f a) where
    put rfa = put (lenToPfx @pfx (Foldable.length fa)) <> put fa
      where fa = unrefine1 rfa

class GetCount f where getCount :: Get a => Int -> Getter (f a)
instance GetCount [] where getCount n = Monad.count n get

instance (Prefix pfx, GetCount f, Get pfx, Get a)
  => Get (CountPrefixed pfx f a) where
    get = do
        pfx <- get @pfx
        fa <- getCount (pfxToLen pfx)
        pure $ reallyUnsafeRefine1 fa
