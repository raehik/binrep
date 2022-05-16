{-# LANGUAGE FunctionalDependencies #-}

module Raehik.Validate.Strengthen where

import GHC.TypeNats ( Natural, KnownNat )
import Data.Word
import Data.Int
import Refined ( Refined, refine, Predicate )
import Data.Vector.Sized qualified as Vector
import Data.Vector.Sized ( Vector )

import Util ( coerceBounded )
import Data.Either.Combinators ( mapLeft )

class Strengthen w s | s -> w where strengthen :: w -> Either String s

instance Strengthen w s => Strengthen [w] [s] where
    strengthen = traverse strengthen

instance KnownNat n => Strengthen [a] (Vector n a) where
    strengthen w =
        case Vector.fromList w of
          Nothing -> Left "TODO bad size vector"
          Just s  -> Right s

instance Predicate p a => Strengthen a (Refined p a) where
    strengthen = mapLeft show . refine

instance Strengthen Natural Word8  where strengthen = coerceBounded
instance Strengthen Natural Word16 where strengthen = coerceBounded
instance Strengthen Natural Word32 where strengthen = coerceBounded
instance Strengthen Natural Word64 where strengthen = coerceBounded
instance Strengthen Integer Int8   where strengthen = coerceBounded
instance Strengthen Integer Int16  where strengthen = coerceBounded
instance Strengthen Integer Int32  where strengthen = coerceBounded
instance Strengthen Integer Int64  where strengthen = coerceBounded
