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

{- | Any 'w' can be "strengthened" into an 's' by asserting some properties.

For example, you may strengthen some 'Natural' @n@ into a 'Word8' by asserting
@0 <= n <= 255@.

Note that we restrict strengthened types to having only one corresponding weak
representation using functional dependencies.
-}
class Strengthen w s | s -> w where strengthen :: w -> Either String s

-- | Strengthen each element of a list.
instance Strengthen w s => Strengthen [w] [s] where
    strengthen = traverse strengthen

-- | Obtain a sized vector by asserting the size of a plain list.
instance KnownNat n => Strengthen [a] (Vector n a) where
    strengthen w =
        case Vector.fromList w of
          Nothing -> Left "TODO bad size vector"
          Just s  -> Right s

-- | Obtain a refined type by applying its associated refinement.
instance Predicate p a => Strengthen a (Refined p a) where
    strengthen = mapLeft show . refine

-- Strengthen 'Natural's into Haskell's bounded unsigned numeric types.
instance Strengthen Natural Word8  where strengthen = coerceBounded
instance Strengthen Natural Word16 where strengthen = coerceBounded
instance Strengthen Natural Word32 where strengthen = coerceBounded
instance Strengthen Natural Word64 where strengthen = coerceBounded

-- Strengthen 'Integer's into Haskell's bounded signed numeric types.
instance Strengthen Integer Int8   where strengthen = coerceBounded
instance Strengthen Integer Int16  where strengthen = coerceBounded
instance Strengthen Integer Int32  where strengthen = coerceBounded
instance Strengthen Integer Int64  where strengthen = coerceBounded
