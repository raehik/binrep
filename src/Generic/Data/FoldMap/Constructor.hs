{-# LANGUAGE UndecidableInstances #-} -- due to type class design

module Generic.Data.FoldMap.Constructor where

import GHC.Generics
import Data.Kind ( type Type, type Constraint )

-- | Monoids that can be generically @foldMap@-ped to.
class Monoid m => GenericFoldMap m where
    type GenericFoldMapC m :: Type -> Constraint
    genericFoldMapF :: GenericFoldMapC m a => a -> m

class GFoldMapC m f where gFoldMapC :: f p -> m

instance (Semigroup m, GFoldMapC m l, GFoldMapC m r)
  => GFoldMapC m (l :*: r) where
    gFoldMapC (l :*: r) = gFoldMapC l <> gFoldMapC r

instance (GenericFoldMap m, GenericFoldMapC m a)
  => GFoldMapC m (S1 c (Rec0 a)) where
    gFoldMapC (M1 (K1 a)) = genericFoldMapF a

-- | Wow, look! Nothing!
instance Monoid m => GFoldMapC m U1 where gFoldMapC U1 = mempty
