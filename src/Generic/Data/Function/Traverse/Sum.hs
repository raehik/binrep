{-# LANGUAGE UndecidableInstances #-} -- required below GHC 9.6
{-# LANGUAGE AllowAmbiguousTypes #-} -- required due to generic typeclass design

module Generic.Data.Function.Traverse.Sum where

import GHC.Generics
import GHC.TypeLits ( TypeError )
import Util.Generic ( datatypeName', conName' )
import Generic.Data.Function.Error ( type ENoEmpty, type EUnexpectedNonSum )
import Generic.Data.Function.Traverse.Constructor ( GTraverseC(gTraverseC), GenericTraverse(..) )

import Data.Text ( Text )
import Control.Applicative qualified as Applicative
import Control.Applicative ( Alternative((<|>)) )

{- | Sum-type monads that can be generically 'traverse'd.

For sum types, we require a monad with choice to differentiate constructors.
-}
class (GenericTraverse f, Alternative f, Monad f) => GenericTraverseSum f where
    -- | Try to parse a prefix tag of type 'pt'.
    --
    -- Relevant metadata is provided as arguments.
    genericTraverseSumPfxTagAction
        :: GenericTraverseC f pt
        => String   -- ^ data type name
        -> f pt

    -- | Parse error due to no constructor matching the parsed prefix tag.
    --
    -- Relevant metadata is provided as arguments.
    genericTraverseSumNoMatchingCstrAction
        :: String   -- ^ data type name
        -> [String] -- ^ non-matching constructor names
        -> Text     -- ^ prefix tag, prettified
        -> f a

-- | How to use a type as a prefix tag in a generic sum type parser.
data PfxTagCfg a = PfxTagCfg
  { pfxTagCfgFromCstr :: String -> a
  -- ^ How to turn a constructor name into a prefix tag.

  , pfxTagCfgEq :: a -> a -> Bool
  -- ^ How to compare prefix tags for equality.
  --
  -- By shoving this into our generic derivation config, we can avoid adding an
  -- insidious 'Eq' constraint. In general, you will want to set this to '(==)'.

  , pfxTagCfgShow :: a -> Text
  -- ^ Make a prefix tag human-readable. 'show' is often appropriate.
  }

class GTraverseSum f f' where
    gTraverseSum :: GenericTraverseC f pt => PfxTagCfg pt -> f (f' p)

instance (Functor f, GTraverseSum' cd f f') => GTraverseSum f (D1 cd f') where
    gTraverseSum pt = M1 <$> gTraverseSum' @cd pt

class GTraverseSum' cd f f' where
    gTraverseSum' :: GenericTraverseC f pt => PfxTagCfg pt -> f (f' p)

instance (GenericTraverseSum f, GTraverseCSum cd f (l :+: r), Datatype cd)
  => GTraverseSum' cd f (l :+: r) where
    gTraverseSum' ptc = do
        pt <- genericTraverseSumPfxTagAction cd
        gTraverseCSum @cd ptc pt <|> parseErrorNoMatch pt
      where
        cd = datatypeName' @cd
        parseErrorNoMatch pt =
            genericTraverseSumNoMatchingCstrAction cd testedCstrs ((pfxTagCfgShow ptc) pt)
        testedCstrs = [] -- TODO

-- | Refuse to derive a non-sum instance if we expected a sum data type.
instance TypeError EUnexpectedNonSum => GTraverseSum' cd f (C1 cc f') where
    gTraverseSum' = undefined

-- | Refuse to derive an instance for an empty data type.
instance TypeError ENoEmpty => GTraverseSum' cd f V1 where
    gTraverseSum' = undefined

-- | Generic getter (constructor sum level).
class GTraverseCSum cd f f' where
    gTraverseCSum :: PfxTagCfg pt -> pt -> f (f' p)

instance (Functor f, Alternative f, GTraverseCSum cd f l, GTraverseCSum cd f r)
  => GTraverseCSum cd f (l :+: r) where
    gTraverseCSum ptc pt = l <|> r
      where
        l = L1 <$> gTraverseCSum @cd ptc pt
        r = R1 <$> gTraverseCSum @cd ptc pt

instance (Alternative f, GTraverseC cd cc 0 f f', Constructor cc)
  => GTraverseCSum cd f (C1 cc f') where
    gTraverseCSum ptc pt = do
        if   (pfxTagCfgEq ptc) pt ptCstr
        then M1 <$> gTraverseC @cd @cc @0
        else Applicative.empty
      where
        ptCstr = (pfxTagCfgFromCstr ptc) (conName' @cc)
