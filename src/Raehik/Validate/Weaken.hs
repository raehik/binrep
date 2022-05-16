{-# LANGUAGE FunctionalDependencies #-}

module Raehik.Validate.Weaken where

import Refined ( Refined, unrefine )
import Numeric.Natural ( Natural )
import Data.Word
import Data.Int
import Data.Vector.Sized qualified as Vector
import Data.Vector.Sized ( Vector )

class Weaken s w | s -> w where weaken :: s -> w

instance Weaken s w => Weaken [s] [w] where weaken = map weaken
instance Weaken (Vector n a) [a] where weaken = Vector.toList
instance Weaken (Refined p a) a where weaken = unrefine

instance Weaken Word8  Natural where weaken = fromIntegral
instance Weaken Word16 Natural where weaken = fromIntegral
instance Weaken Word32 Natural where weaken = fromIntegral
instance Weaken Word64 Natural where weaken = fromIntegral
instance Weaken Int8   Integer where weaken = fromIntegral
instance Weaken Int16  Integer where weaken = fromIntegral
instance Weaken Int32  Integer where weaken = fromIntegral
instance Weaken Int64  Integer where weaken = fromIntegral
