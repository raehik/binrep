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

import Generic.Data.Via
import GHC.TypeLits ( TypeError )

import Data.Monoid
data A a = A a (Sum Int) ()
    deriving stock (Generic, Show)

class GenericTraverse f where
    type GenericTraverseC f a :: Constraint

    -- include data type metadata because this is especially useful for
    -- (monadic) parsers which would like to record that in error messages. we
    -- don't do it for foldMap because that's pure.
    genericTraverseAction
        :: GenericTraverseC f a
        => String -> String -> Maybe String -> Natural -> f a

-- | 'traverse' over types with no fields in any constructor.
instance GenericTraverse NoRec0 where
    type GenericTraverseC NoRec0 a = TypeError ENoRec0
    genericTraverseAction = undefined

-- | 'traverse' over types where all fields map to their respective 'mempty'.
--
-- Can result in type errors lacking context: a field missing a 'Monoid'
-- instance will type error with a regular "no instance for" message, without
-- telling you the surrounding type.
--
-- Maybe silly.
instance GenericTraverse EmptyRec0 where
    type GenericTraverseC EmptyRec0 a = Monoid a
    genericTraverseAction _ _ _ _ = EmptyRec0 mempty

class GTraverseC cd cc (si :: Natural) f f' where gTraverseC :: f (f' p)

instance (Applicative f, GTraverseC cd cc si f l, GTraverseC cd cc (si + ProdArity r) f r)
  => GTraverseC cd cc si f (l :*: r) where
    gTraverseC = liftA2 (:*:)
                   (gTraverseC @cd @cc @si)
                   (gTraverseC @cd @cc @(si + ProdArity r))

instance (GenericTraverse f, GenericTraverseC f a, Functor f, KnownNat si, Selector cs, Constructor cc, Datatype cd)
  => GTraverseC cd cc si f (S1 cs (Rec0 a)) where
    gTraverseC = (M1 . K1) <$> genericTraverseAction cd cc cs si
      where
        cs = selName'' @cs
        cd = datatypeName' @cd
        cc = conName' @cc
        si = natVal'' @si

instance Applicative f => GTraverseC cd cc 0 f U1 where gTraverseC = pure U1

type family ProdArity (f :: Type -> Type) :: Natural where
    ProdArity (S1 c f)  = 1
    ProdArity (l :*: r) = ProdArity l + ProdArity r
