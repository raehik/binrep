{-# LANGUAGE FunctionalDependencies #-}

module Raehik.Validate.Weaken where

import Refined ( Refined, unrefine )
import Numeric.Natural ( Natural )
import Data.Word
import Data.Int
import Data.Vector.Sized qualified as Vector
import Data.Vector.Sized ( Vector )

{- | Any 's' can be "weakened" into a 'w'.

For example, you may weaken a 'Word8' into a 'Natural'.

Note that we restrict strengthened types to having only one corresponding weak
representation using functional dependencies.
-}
class Weaken s w | s -> w where weaken :: s -> w

-- | Weaken each element of a list.
instance Weaken s w => Weaken [s] [w] where weaken = map weaken

-- | Weaken sized vectors into plain lists.
instance Weaken (Vector n a) [a] where weaken = Vector.toList

-- | Strip the refinement from refined types.
instance Weaken (Refined p a) a where weaken = unrefine

-- Weaken the bounded Haskell numeric types using 'fromIntegral'.
instance Weaken Word8  Natural where weaken = fromIntegral
instance Weaken Word16 Natural where weaken = fromIntegral
instance Weaken Word32 Natural where weaken = fromIntegral
instance Weaken Word64 Natural where weaken = fromIntegral
instance Weaken Int8   Integer where weaken = fromIntegral
instance Weaken Int16  Integer where weaken = fromIntegral
instance Weaken Int32  Integer where weaken = fromIntegral
instance Weaken Int64  Integer where weaken = fromIntegral
