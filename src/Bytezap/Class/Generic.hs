{-# LANGUAGE CPP #-}
{-# LANGUAGE UnboxedTuples #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE AllowAmbiguousTypes #-}

module Bytezap.Class.Generic where

import Bytezap
import Bytezap.Class
import GHC.Generics
import GHC.TypeError
import Data.Kind
import GHC.TypeLits
import Util.TypeNats ( natVal'' )

import Bytezap.Int qualified as Write

putGeneric :: (Generic a, GPut (Rep a)) => a -> Write
putGeneric = gput . from

class GPut f where gput :: f p -> Write
instance GPut U1 where gput _ = mempty
instance Put a => GPut (K1 i a) where gput (K1 a) = put a
instance GPut f => GPut (M1 i d f) where gput (M1 a) = gput a

instance (GPut l, GPut r) => GPut (l :*: r) where
    gput (l :*: r) = gput l <> gput r

-- | only 255 constructors matey (soz)
instance
  ( FitsInByte (SumArity (a :+: b))
  , GPutSumConstr (a :+: b)
  , GPutSumTag 0 (a :+: b)
  ) => GPut (a :+: b) where
    gput x = gputSumTag @0 x <> gputSumConstr x

class GPutSumConstr f where gputSumConstr :: f p -> Write
instance (GPutSumConstr l, GPutSumConstr r) => GPutSumConstr (l :+: r) where
    gputSumConstr = \case L1 l -> gputSumConstr l
                          R1 r -> gputSumConstr r
instance (GPut a) => GPutSumConstr (C1 c a) where
    gputSumConstr (M1 a) = gput a

class GPutSumTag (n :: Natural) f where gputSumTag :: f p -> Write
instance (GPutSumTag n l, GPutSumTag (n + SumArity l) r, KnownNat n)
  => GPutSumTag n (l :+: r) where
    gputSumTag = \case L1 l -> gputSumTag @n                l
                       R1 r -> gputSumTag @(n + SumArity l) r
instance KnownNat n => GPutSumTag n (C1 c a) where
    gputSumTag _ = Write.w8 $ fromIntegral $ natVal'' @n

--------------------------------------------------------------------------------

type family SumArity (a :: Type -> Type) :: Natural where
    SumArity (C1 c a) = 1
    SumArity (x :+: y) = SumArity x + SumArity y

type FitsInByte n = FitsInByteResult (n <=? 255)

type family FitsInByteResult (b :: Bool) :: Constraint where
    FitsInByteResult 'True = ()
    FitsInByteResult 'False = TypeErrorMessage
        "Generic deriving of Store instances can only be used on datatypes with fewer than 256 constructors."

type family TypeErrorMessage (a :: Symbol) :: Constraint where
#if MIN_VERSION_base(4,9,0)
    TypeErrorMessage a = TypeError ('Text a)
-- GHC < 8.0 does not support empty closed type families
#elif __GLASGOW_HASKELL__ < 800
    TypeErrorMessage a = a ~ ""
#endif
