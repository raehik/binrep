{- TODO left-to-right
-}

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

genericTraverseNonSum
    :: forall f a
    .  (Generic a, GTraverseNonSum f (Rep a), Functor f)
    => f a
genericTraverseNonSum = to <$> gTraverseNonSum

genericTraverseSum
    :: forall f pt a
    .  (Generic a, GTraverseSum f (Rep a), GenericTraverseC f pt, Functor f)
    => PfxTagCfg pt -> f a
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
