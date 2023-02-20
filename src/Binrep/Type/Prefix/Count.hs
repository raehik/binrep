{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE UndecidableInstances #-} -- required for easier instances
{-# LANGUAGE OverloadedStrings #-} -- for refined errors

module Binrep.Type.Prefix.Count where

import Binrep.Type.Prefix
import Binrep
import Control.Monad.Combinators qualified as Monad

import GHC.TypeNats
import Util.TypeNats ( natValInt )
import Refined hiding ( Weaken(..), strengthen )
import Refined.Unsafe

import Data.Typeable ( Typeable, typeRep )

import Data.Foldable qualified as Foldable

data CountPrefix pfx
type CountPrefixed pfx = Refined (CountPrefix pfx)

instance (KnownNat (Max pfx), Foldable f, Typeable pfx)
  => Predicate (CountPrefix pfx) (f a) where
    validate p a
      | Foldable.length a <= natValInt @(Max pfx) = Nothing
      | otherwise = throwRefineOtherException (typeRep p) $
          "thing too big for length prefix type"

-- TODO no idea if this is sensible
instance IsCBLen (CountPrefixed pfx a) where
    type CBLen (CountPrefixed pfx a) = CBLen pfx + CBLen a

instance (Prefix pfx, Foldable f, BLen pfx, BLen (f a))
  => BLen (CountPrefixed pfx (f a)) where
    blen ra = blen (lenToPfx @pfx (Foldable.length a)) + blen a
      where a = unrefine ra

instance (Prefix pfx, Foldable f, Put pfx, Put (f a))
  => Put (CountPrefixed pfx (f a)) where
    put ra = put (lenToPfx @pfx (Foldable.length a)) <> put a
      where a = unrefine ra

-- Fucking lol dude this is wicked
class GetCount f where getCount :: Get a => Int -> Getter (f a)
instance GetCount [] where getCount n = Monad.count n get

-- Suck my nuts
instance (Prefix pfx, GetCount f, Get pfx, Get a)
  => Get (CountPrefixed pfx (f a)) where
    get = do
        pfx <- get @pfx
        a <- getCount (pfxToLen pfx)
        pure $ reallyUnsafeRefine a
