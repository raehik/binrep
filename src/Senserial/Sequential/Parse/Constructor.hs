{-# LANGUAGE UndecidableInstances #-} -- due to type class design
{-# LANGUAGE AllowAmbiguousTypes #-}  -- due to type class design

{- | Generic sequential constructor parsing.

Reusable between sum and non-sum types, so we write it once here.
-}

module Senserial.Sequential.Parse.Constructor where

import Senserial.Sequential.Parse.Parser

import GHC.Generics
import GHC.TypeNats ( Natural, KnownNat, type (+) )
import Util.Generic ( datatypeName', conName', selName'' )
import Util.TypeNats ( natVal'' )
import Data.Kind ( Type )

import Control.Applicative ( liftA2 )

-- | Generic sequential parser (constructor level).
class GSeqParseC cd cc (si :: Natural) prs f where gSeqParseC :: prs (f p)

-- | Parse fields left to right.
instance (Applicative prs, GSeqParseC cd cc si prs l, GSeqParseC cd cc (si + ProdArity r) prs r)
  => GSeqParseC cd cc si prs (l :*: r) where
    gSeqParseC = liftA2 (:*:)
                   (gSeqParseC @cd @cc @si)
                   (gSeqParseC @cd @cc @(si + ProdArity r))

-- | Parse a field using its existing 'Get' instance.
--
-- Fills out detailed error information by reflecting bits from the various
-- generic meta types ferried through from above type classes.
instance (SeqParser prs, SeqParserC prs a, Monad prs, KnownNat si, Selector cs, Constructor cc, Datatype cd)
  => GSeqParseC cd cc si prs (S1 cs (Rec0 a)) where
    gSeqParseC = do
        a <- seqParse cd cc cs si
        pure $ M1 $ K1 a
      where
        cs = selName'' @cs
        cd = datatypeName' @cd
        cc = conName' @cc
        si = natVal'' @si

-- | Wow, look! Nothing!
instance Applicative prs => GSeqParseC cd cc 0 prs U1 where gSeqParseC = pure U1

type family ProdArity (f :: Type -> Type) :: Natural where
    ProdArity (S1 c f)  = 1
    ProdArity (l :*: r) = ProdArity l + ProdArity r
