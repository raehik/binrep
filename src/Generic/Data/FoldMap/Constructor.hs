{-# LANGUAGE UndecidableInstances #-} -- due to type class design

module Generic.Data.FoldMap.Constructor where

import GHC.Generics
import Data.Kind ( type Constraint )

import Generic.Data.Via
import GHC.TypeLits ( TypeError )

-- | 'Monoid's that can be generically 'foldMap'ped to.
class GenericFoldMap m where
    -- | The type class that enables mapping permitted types to the monoid.
    --
    -- Should provide a function like 'genericFoldMapF'.
    type GenericFoldMapC m a :: Constraint

    -- | The "map" function in 'foldMap' (first argument).
    genericFoldMapF :: GenericFoldMapC m a => a -> m

-- | 'foldMap' over types with no fields in any constructor.
instance GenericFoldMap (NoRec0 m) where
    type GenericFoldMapC (NoRec0 m) _ = TypeError ENoRec0
    genericFoldMapF = undefined

-- | 'foldMap' over types where all fields map to 'mempty'.
instance Monoid m => GenericFoldMap (EmptyRec0 m) where
    type GenericFoldMapC (EmptyRec0 m) _ = ()
    genericFoldMapF _ = EmptyRec0 mempty

-- | 'foldMap' on individual constructors (products).
class GFoldMapC m f where gFoldMapC :: f p -> m

-- | 'foldMap' on individual constructors (products).
instance (Semigroup m, GFoldMapC m l, GFoldMapC m r)
  => GFoldMapC m (l :*: r) where
    gFoldMapC (l :*: r) = gFoldMapC l <> gFoldMapC r

instance (GenericFoldMap m, GenericFoldMapC m a)
  => GFoldMapC m (S1 c (Rec0 a)) where
    gFoldMapC (M1 (K1 a)) = genericFoldMapF a

-- | Wow, look! Nothing!
instance Monoid m => GFoldMapC m U1 where gFoldMapC U1 = mempty
