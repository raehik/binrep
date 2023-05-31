{-| Generic sequential non-sum type parsing.

All we do here is unwrap, then pass to the constructor parser.
-}

{-# LANGUAGE AllowAmbiguousTypes #-} -- due to type class design

module Senserial.Sequential.Parse.NonSum where

import Senserial.Sequential.Parse.Constructor
import Senserial.Internal.Error ( type ENoEmpty, type EUnexpectedSum )

import GHC.Generics
import GHC.TypeError ( TypeError )

-- | Sequentially parse a term of the non-sum type @a@ generically.
seqParseNonSum
    :: forall prs a
    .  (Generic a, GSeqParseDNonSum prs (Rep a), Functor prs)
    => prs a
seqParseNonSum = to <$> gSeqParseDNonSum

-- | Easier user shorthand for the top-level parser.
type SeqParseNonSum = GSeqParseDNonSum

-- | Generic non-sum sequential parser (data type/top level).
class GSeqParseDNonSum prs f where gSeqParseDNonSum :: prs (f p)

-- | Unwrap meta (data type/top level).
instance (Functor prs, GSeqParseDNonSum' cd prs f) => GSeqParseDNonSum prs (D1 cd f) where
    gSeqParseDNonSum = M1 <$> gSeqParseDNonSum' @cd

-- | Generic non-sum sequential parser (data type/top level, unwrapped meta).
class GSeqParseDNonSum' cd prs f where gSeqParseDNonSum' :: prs (f p)

-- | Refuse to derive a sum instance if we expected a non-sum data type.
instance TypeError EUnexpectedSum => GSeqParseDNonSum' cd prs (l :+: r) where
    gSeqParseDNonSum' = undefined

-- | Parse the single constructor of a non-sum data type.
instance (Functor prs, GSeqParseC cd cc 0 prs f)
  => GSeqParseDNonSum' cd prs (C1 cc f) where
    gSeqParseDNonSum' = M1 <$> gSeqParseC @cd @cc @0

-- | Refuse to derive an instance for an empty data type.
instance TypeError ENoEmpty => GSeqParseDNonSum' cd prs V1 where
    gSeqParseDNonSum' = undefined
