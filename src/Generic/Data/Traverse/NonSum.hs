{-# LANGUAGE UndecidableInstances #-} -- required below GHC 9.6
{-# LANGUAGE AllowAmbiguousTypes #-} -- due to type class design

module Generic.Data.Traverse.NonSum where

import GHC.Generics
import GHC.TypeLits ( TypeError )
import Generic.Data.Error ( type ENoEmpty, type EUnexpectedSum )
import Generic.Data.Traverse.Constructor ( GTraverseC(gTraverseC) )

class GTraverseNonSum f f' where gTraverseNonSum :: f (f' p)

instance (Functor f, GTraverseNonSum' cd f f') => GTraverseNonSum f (D1 cd f') where
    gTraverseNonSum = M1 <$> gTraverseNonSum' @cd

class GTraverseNonSum' cd f f' where gTraverseNonSum' :: f (f' p)

instance TypeError EUnexpectedSum => GTraverseNonSum' cd f (l :+: r) where
    gTraverseNonSum' = undefined

instance (Functor f, GTraverseC cd cc 0 f f')
  => GTraverseNonSum' cd f (C1 cc f') where
    gTraverseNonSum' = M1 <$> gTraverseC @cd @cc @0

instance TypeError ENoEmpty => GTraverseNonSum' cd f V1 where
    gTraverseNonSum' = undefined
