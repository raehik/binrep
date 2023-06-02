{-# LANGUAGE UndecidableInstances #-} -- required below GHC 9.6
{-# LANGUAGE AllowAmbiguousTypes #-} -- required due to generic typeclass design

{- | Generic sequential sum type parsing.

Here, we use the the provided function to turn the given value's constructor
name into a prefix tag, then pass to the constructor serializer.
-}

module Senserial.Sequential.Parse.Sum where

import Senserial.Sequential.Parse.Parser
import Senserial.Sequential.Parse.Constructor
import Senserial.Internal.Error ( type ENoEmpty, type EUnexpectedNonSum )

import GHC.Generics
import GHC.TypeLits ( TypeError )
import Util.Generic ( datatypeName', conName' )
import Data.Text qualified as Text
import Data.Text ( Text )
import Control.Applicative qualified as Applicative
import Control.Applicative ( Alternative((<|>)) )

-- | Sequentially parse a term of the sum type @a@ generically.
seqParseSum
    :: forall prs pt a
    .  (Generic a, GSeqParseDSum prs (Rep a), SeqParserC prs pt, Functor prs)
    => PfxTagCfg pt -> prs a
seqParseSum ptc = to <$> gSeqParseDSum ptc

-- | Easier user shorthand for the top-level parser.
type SeqParseSum = GSeqParseDSum

{- | Sequential sum-type parsers.

A type may be used as a sequential parser for a sum type provided it can already
be used for non-sum types and supports choice (via 'Alternative').

This is an sort of "enumeration" type class, which enables selecting a class
to use in a generic instance @S1@ base case.
-}
class (SeqParser prs, Alternative prs) => SeqParserSum prs where
    -- | Try to parse a prefix tag of type 'pt'.
    --
    -- Relevant metadata is provided as arguments.
    seqParserSumParsePfxTag
        :: SeqParserC prs pt
        => String   -- ^ data type name
        -> prs pt

    -- | Parse error due to no constructor matching the parsed prefix tag.
    --
    -- Relevant metadata is provided as arguments.
    seqParserSumErrNoMatchingCstr
        :: String   -- ^ data type name
        -> [String] -- ^ non-matching constructor names
        -> Text     -- ^ prefix tag, prettified
        -> prs a

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

-- | Construct a prefix tag config using existing 'Eq' and 'Show' instances.
--
-- The user only needs to provide the constructor name parser.
eqShowPfxTagCfg :: (Eq a, Show a) => (String -> a) -> PfxTagCfg a
eqShowPfxTagCfg f = PfxTagCfg
    { pfxTagCfgFromCstr = f
    , pfxTagCfgEq = (==)
    , pfxTagCfgShow = Text.pack . show
    }

-- | Generic sum type parser (data type/top level).
class GSeqParseDSum prs f where
    gSeqParseDSum :: SeqParserC prs pt => PfxTagCfg pt -> prs (f p)

-- | Unwrap meta (data type/top level).
instance (Functor prs, GSeqParseDSum' cd prs f) => GSeqParseDSum prs (D1 cd f) where
    gSeqParseDSum pt = M1 <$> gSeqParseDSum' @cd pt

-- | Generic sum type parser (data type/top level, unwrapped meta).
class GSeqParseDSum' cd prs f where
    gSeqParseDSum' :: SeqParserC prs pt => PfxTagCfg pt -> prs (f p)

-- | TODO
instance (Alternative prs, SeqParserSum prs, Monad prs, GSeqParseCSum cd prs (l :+: r), Datatype cd)
  => GSeqParseDSum' cd prs (l :+: r) where
    gSeqParseDSum' ptc = do
        pt <- seqParserSumParsePfxTag cd
        gSeqParseCSum @cd ptc pt <|> parseErrorNoMatch pt
      where
        cd = datatypeName' @cd
        parseErrorNoMatch pt =
            seqParserSumErrNoMatchingCstr cd testedCstrs ((pfxTagCfgShow ptc) pt)
        testedCstrs = [] -- TODO

-- | Refuse to derive a non-sum instance if we expected a sum data type.
instance TypeError EUnexpectedNonSum => GSeqParseDSum' cd prs (C1 cc f) where
    gSeqParseDSum' = undefined

-- | Refuse to derive an instance for an empty data type.
instance TypeError ENoEmpty => GSeqParseDSum' cd prs V1 where
    gSeqParseDSum' = undefined

-- | Generic getter (constructor sum level).
class GSeqParseCSum cd prs f where
    gSeqParseCSum :: PfxTagCfg pt -> pt -> prs (f p)

instance (Functor prs, Alternative prs, GSeqParseCSum cd prs l, GSeqParseCSum cd prs r)
  => GSeqParseCSum cd prs (l :+: r) where
    gSeqParseCSum ptc pt = l <|> r
      where
        l = L1 <$> gSeqParseCSum @cd ptc pt
        r = R1 <$> gSeqParseCSum @cd ptc pt

instance (Alternative prs, GSeqParseC cd cc 0 prs f, Constructor cc)
  => GSeqParseCSum cd prs (C1 cc f) where
    gSeqParseCSum ptc pt = do
        if   (pfxTagCfgEq ptc) pt ptCstr
        then M1 <$> gSeqParseC @cd @cc @0
        else Applicative.empty
      where
        ptCstr = (pfxTagCfgFromCstr ptc) (conName' @cc)
