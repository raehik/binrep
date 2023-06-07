{-# LANGUAGE UndecidableInstances #-} -- required below GHC 9.6

module Generic.Data.FoldMap.NonSum where

import GHC.Generics
import GHC.TypeLits ( TypeError )
import Senserial.Internal.Error ( type ENoEmpty, type EUnexpectedSum )
import Generic.Data.FoldMap.Constructor ( GFoldMapC(gFoldMapC) )

{- | Left-to-right 'foldMap' over generic product data types.

Take a generic representation, map each field in the data type to a 'Monoid',
and combine the results with ('<>').

We combine left-to-right because it's most common.
-}
class GFoldMapNonSum m f where gFoldMapNonSum :: f p -> m

instance GFoldMapNonSum m f => GFoldMapNonSum m (D1 c f) where
    gFoldMapNonSum (M1 a) = gFoldMapNonSum a

instance TypeError EUnexpectedSum => GFoldMapNonSum m (l :+: r) where
    gFoldMapNonSum = undefined

instance GFoldMapC m f => GFoldMapNonSum m (C1 c f) where
    gFoldMapNonSum (M1 a) = gFoldMapC a

instance TypeError ENoEmpty => GFoldMapNonSum m V1 where
    gFoldMapNonSum = undefined
