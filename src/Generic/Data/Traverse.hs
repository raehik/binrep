-- | Left-to-right 'traverse' for generic data types.

module Generic.Data.Traverse
  ( GenericTraverse(..)
  , genericTraverseNonSum , GTraverseNonSum
  , GenericTraverseSum(..), PfxTagCfg(..)
  , genericTraverseSum,     GTraverseSum
  , eqShowPfxTagCfg
  ) where

import GHC.Generics

import Generic.Data.Traverse.NonSum
import Generic.Data.Traverse.Sum
import Generic.Data.Traverse.Constructor

import Data.Text qualified as Text

{-
-- | Generic left-to-right 'traverse' over a term of non-sum data type @a@.
--
-- @a@ must have exactly one constructor.
genericTraverseNonSum
    :: forall f a
    .  (Generic a, GTraverseNonSum f (Rep a), Functor f)
    => f a
genericTraverseNonSum = to <$> gTraverseNonSum
-}

genericTraverseNonSum
    :: forall f a
    .  (Generic1 f, GTraverseNonSum f (Rep1 f))
    => f a
genericTraverseNonSum = to1 <$> gTraverseNonSum

-- | Generic left-to-right 'traverse' over a term of sum data type @a@.
--
-- @a@ must have at least two constructors.
--
-- You must provide a configuration for how to handle constructors.
genericTraverseSum
    :: forall f pt a
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
