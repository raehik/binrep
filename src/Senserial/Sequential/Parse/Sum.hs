{-| Generic binrep parsers via flatparse.

Parser construction is split into multiple classes to allow gathering data type
metadata, to be inserted into parse errors should they arise. As we move down
the SOP tree, we reflect data type metadata and ferry it through the "lower"
parsers.

Tuned for binrep-style parsing: sum types must be handled explicitly. TODO

Note your type's 'Generic' instance _must_ be decorated with metadata. So only
use these with GHC's automatically derived 'Generic' instances.
-}

{-# LANGUAGE UndecidableInstances #-} -- required for TypeError >:(
{-# LANGUAGE AllowAmbiguousTypes #-} -- required due to generic typeclass design

module Senserial.Sequential.Parse.Wip where

import GHC.Generics
import GHC.TypeError ( TypeError )
import Util.Generic ( datatypeName', conName', selName'' )
import Binrep.Util.Class
import Binrep.Util.Generic
import Data.Text ( Text )
import Binrep.Get.Flatparse
import Data.Kind
import GHC.TypeNats
import Util.TypeNats ( natVal'' )

import FlatParse.Basic qualified as FP
import Control.Applicative ( (<|>), liftA2 )

import Data.Kind ( type Type, type Constraint )

{- | Sequential parsers.

A type may be used as a sequential parser provided it is a 'Monad' and has a
class for parsing from compatible types.

This is an sort of "enumeration" type class, which enables selecting a class
to use in a generic instance @S1@ base case.
-}
class Monad prs => SeqParser prs where
    -- | Parser type class.
    type SeqParserC prs :: Type -> Constraint

    -- | Parse using the selected parser's type class.
    --
    -- This should be set to the appropriate function in the parser type class.
    seqParse :: SeqParserC prs a => prs a

{-
-- | Generic sum type parser (data type/top level).
class GSeqParseDSum prs f where
    gSeqParseDSum :: Get w => Cfg w -> Getter (f p)
-}

-- | Generic non-sum type parser (data type/top level).
class GSeqParseDNonSum prs f where gSeqParseDNonSum :: prs (f p)

-- | Unwrap meta (data type/top level).
instance (SeqParser prs, GSeqParseDNonSum' cd prs f) => GSeqParseDNonSum prs (D1 cd f) where
    gSeqParseDNonSum = M1 <$> gSeqParseDNonSum' @cd

-- | Generic non-sum type parser (data type/top level, unwrapped meta).
class GSeqParseDNonSum' cd prs f where gSeqParseDNonSum' :: prs (f p)

-- | Refuse to derive a sum instance if we expected a non-sum data type.
instance TypeError EUnexpectedSum => GSeqParseDNonSum' cd prs (l :+: r) where
    gSeqParseDNonSum' = undefined

-- | Parse the single constructor of a non-sum data type.
instance GGetC cd cc 0 f => GSeqParseDNonSum' cd prs (C1 cc f) where
    gSeqParseDNonSum' = M1 <$> ggetC @cd @cc @0

-- | Refuse to derive an instance for an empty data type.
instance TypeError ENoEmpty => GSeqParseDNonSum' cd prs V1 where
    gSeqParseDNonSum' = undefined

-- | Generic getter (constructor level).
class GGetC cd cc (si :: Natural) f where ggetC :: Getter (f p)

-- | Parse fields left to right.
instance (GGetC cd cc si l, GGetC cd cc (si + ProdArity r) r)
  => GGetC cd cc si (l :*: r) where
    ggetC = liftA2 (:*:)
                   (ggetC @cd @cc @si @l)
                   (ggetC @cd @cc @(si + ProdArity r))

-- | Parse a field using its existing 'Get' instance.
--
-- Fills out detailed error information by reflecting bits from the various
-- generic meta types ferried through from above type classes.
instance (Get a, KnownNat si, Selector cs, Constructor cc, Datatype cd)
  => GGetC cd cc si (S1 cs (Rec0 a)) where
    ggetC = do
        a <- getEWrap $ EGeneric cd . EGenericField cc cs si
        pure $ M1 $ K1 a
      where
        cs = selName'' @cs
        cd = datatypeName' @cd
        cc = conName' @cc
        si = natVal'' @si

-- | Wow, look! Nothing!
instance GGetC cd cc 0 U1 where ggetC = pure U1

type family ProdArity (f :: Type -> Type) :: Natural where
    ProdArity (S1 c f)  = 1
    ProdArity (l :*: r) = ProdArity l + ProdArity r

{-

-- | Generic sum type getter (data type/top level).
class GGetDSum f where ggetDSum :: Get w => Cfg w -> Getter (f p)

-- | Unwrap meta (data type/top level).
instance GGetDSum' cd f => GGetDSum (D1 cd f) where
    ggetDSum cfg = M1 <$> ggetDSum' @cd cfg

-- | Generic sum type getter (data type/top level, unwrapped meta).
class GGetDSum' cd f where ggetDSum' :: Get w => Cfg w -> Getter (f p)

-- | TODO
instance (GGetCSum cd (l :+: r), Datatype cd) => GGetDSum' cd (l :+: r) where
    -- TODO should I use regular (<|>) or flatparse's re-associated one?
    ggetDSum' cfg = do
        tag <- getEWrap $ EGeneric cd . EGenericSum . EGenericSumTag
        ggetCSum @cd cfg tag <|> parseErrorNoMatch tag
      where
        cd = datatypeName' @cd
        parseErrorNoMatch tag =
              FP.err
            $ EGeneric cd
            $ EGenericSum
            $ EGenericSumTagNoMatch []
            $ cSumTagShow cfg
            $ tag

-- | Refuse to derive a non-sum instance if we expected a sum data type.
instance TypeError EUnexpectedNonSum => GGetDSum' cd (C1 cc f) where
    ggetDSum' = undefined

-- | Refuse to derive an instance for an empty data type.
instance TypeError ENoEmpty => GGetDSum' cd V1 where
    ggetDSum' = undefined

-- | Generic getter (constructor sum level).
class GGetCSum cd f where
    ggetCSum :: Get w => Cfg w -> w -> Getter (f p)

instance (GGetCSum cd l, GGetCSum cd r) => GGetCSum cd (l :+: r) where
    -- TODO should I use regular (<|>) or flatparse's re-associated one?
    ggetCSum cfg tag = l <|> r
      where
        l = L1 <$> ggetCSum @cd cfg tag
        r = R1 <$> ggetCSum @cd cfg tag

instance (GGetC cd cc 0 f, Constructor cc) => GGetCSum cd (C1 cc f) where
    ggetCSum cfg tag = do
        if   (cSumTagEq cfg) tag cTag
        then M1 <$> ggetC @cd @cc @0
        else FP.failed
      where
        cTag = (cSumTag cfg) (conName' @cc)

-}
