{- TODO left-to-right
-}

module Generic.Data.FoldMap
  ( GenericFoldMap(..)
  , genericFoldMapNonSum, GFoldMapNonSum
  , genericFoldMapSum,    GFoldMapSum
  ) where

import GHC.Generics

import Generic.Data.FoldMap.NonSum
import Generic.Data.FoldMap.Sum
import Generic.Data.FoldMap.Constructor

genericFoldMapNonSum
    :: forall m a
    .  (Generic a, GFoldMapNonSum m (Rep a))
    => a -> m
genericFoldMapNonSum = gFoldMapNonSum . from

genericFoldMapSum
    :: forall m a
    .  (Generic a, GFoldMapSum m (Rep a))
    => (String -> m) -> a -> m
genericFoldMapSum f = gFoldMapSum f . from
