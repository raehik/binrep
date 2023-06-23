{- | 'traverse' for generic data types.

TODO This is harder to conceptualize than generic 'foldMap'. No nice clean
explanation yet.

This function can provide generic support for simple parser-esque types.
-}

module Generic.Data.Function.Traverse
  ( GenericTraverse(..)
  , genericTraverseNonSum , GTraverseNonSum
  , GenericTraverseSum(..), PfxTagCfg(..)
  , genericTraverseSum,     GTraverseSum
  , eqShowPfxTagCfg
  ) where

import GHC.Generics

import Generic.Data.Function.Traverse.NonSum
import Generic.Data.Function.Traverse.Sum
import Generic.Data.Function.Traverse.Constructor

import Data.Text qualified as Text

-- | Generic 'traverse' over a term of non-sum data type @f a@.
--
-- @f a@ must have exactly one constructor.
genericTraverseNonSum
    :: forall f a
    .  (Generic a, GTraverseNonSum f (Rep a), Functor f)
    => f a
genericTraverseNonSum = to <$> gTraverseNonSum

-- | Generic 'traverse' over a term of sum data type @f a@.
--
-- @f a@ must have at least two constructors.
--
-- You must provide a configuration for how to handle constructors.
genericTraverseSum
    :: forall f a pt
    .  (Generic a, GTraverseSum f (Rep a), GenericTraverseC f pt, Functor f)
    => PfxTagCfg pt
    -> f a
genericTraverseSum ptc = to <$> gTraverseSum ptc

-- | Construct a prefix tag config using existing 'Eq' and 'Show' instances.
--
-- The user only needs to provide the constructor name parser.
eqShowPfxTagCfg :: (Eq a, Show a) => (String -> a) -> PfxTagCfg a
eqShowPfxTagCfg f = PfxTagCfg
    { pfxTagCfgFromCstr = f
    , pfxTagCfgEq = (==)
    , pfxTagCfgShow = Text.pack . show
    }
