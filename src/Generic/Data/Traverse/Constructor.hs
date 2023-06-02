{-# LANGUAGE UndecidableInstances #-} -- due to type class design
{-# LANGUAGE AllowAmbiguousTypes #-}  -- due to type class design
{-# LANGUAGE ApplicativeDo #-} -- TODO because I'm lazy

module Generic.Data.Traverse.Constructor where

import GHC.Generics
import GHC.TypeNats ( Natural, KnownNat, type (+) )
import Util.Generic ( datatypeName', conName', selName'' )
import Util.TypeNats ( natVal'' )

import Control.Applicative ( liftA2 )

import Data.Kind ( type Type, type Constraint )

class Applicative f => GenericTraverse f where
    type GenericTraverseC f :: Type -> Constraint

    -- include data type metadata because this is especially useful for
    -- (monadic) parsers which would like to record that in error messages. we
    -- don't do it for foldMap because that's pure.
    genericTraverseAction
        :: GenericTraverseC f a
        => String -> String -> Maybe String -> Natural -> f a

class GTraverseC cd cc (si :: Natural) f f' where gTraverseC :: f (f' p)

instance (Applicative f, GTraverseC cd cc si f l, GTraverseC cd cc (si + ProdArity r) f r)
  => GTraverseC cd cc si f (l :*: r) where
    gTraverseC = liftA2 (:*:)
                   (gTraverseC @cd @cc @si)
                   (gTraverseC @cd @cc @(si + ProdArity r))

instance (GenericTraverse f, GenericTraverseC f a, Applicative f, KnownNat si, Selector cs, Constructor cc, Datatype cd)
  => GTraverseC cd cc si f (S1 cs (Rec0 a)) where
    gTraverseC = do
        -- TODO rewrite in Applicative style
        -- ApplicativeDo figures it out but it's still messy w
        a <- genericTraverseAction cd cc cs si
        pure $ M1 $ K1 a
      where
        cs = selName'' @cs
        cd = datatypeName' @cd
        cc = conName' @cc
        si = natVal'' @si

instance Applicative f => GTraverseC cd cc 0 f U1 where gTraverseC = pure U1

type family ProdArity (f :: Type -> Type) :: Natural where
    ProdArity (S1 c f)  = 1
    ProdArity (l :*: r) = ProdArity l + ProdArity r
