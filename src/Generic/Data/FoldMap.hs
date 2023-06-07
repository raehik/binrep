-- | Left-to-right 'foldMap' for generic data types.

module Generic.Data.FoldMap
  ( GenericFoldMap(..)
  , genericFoldMapNonSum, GFoldMapNonSum
  , genericFoldMapSum,    GFoldMapSum
  ) where

import GHC.Generics

import Generic.Data.FoldMap.NonSum
import Generic.Data.FoldMap.Sum
import Generic.Data.FoldMap.Constructor

-- | Generic left-to-right 'foldMap' over a term of non-sum data type @a@.
--
-- @a@ must have exactly one constructor.
genericFoldMapNonSum
    :: forall m a
    .  (Generic a, GFoldMapNonSum m (Rep a))
    => a -> m
genericFoldMapNonSum = gFoldMapNonSum . from

-- | Generic left-to-right 'foldMap' over a term of sum data type @a@.
--
-- @a@ must have at least two constructors.
--
-- You must provide a function for mapping constructor names to monoidal values.
genericFoldMapSum
    :: forall m a
    .  (Generic a, GFoldMapSum m (Rep a))
    => (String -> m)
    -> a -> m
genericFoldMapSum f = gFoldMapSum f . from
