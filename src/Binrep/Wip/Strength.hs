{-# LANGUAGE OverloadedStrings #-}

module Binrep.Wip.Strength where

import Refined hiding ( Weaken )
import Binrep.Type.LenPfx
import Data.Vector.Sized ( Vector )
import Data.Vector.Sized qualified as V
import Data.Kind ( Type )
import Data.Word ( Word8 )
import Numeric.Natural ( Natural )
import Data.Text ( Text )

data Strength = Strong | Weak

data Both (s :: Strength) a = Both
  { bothList  :: Switch s (Vector 5 a)
  , bothWord8 :: Switch s Word8
  }
deriving stock instance Show (Weaken a) => Show (Both 'Weak a)

-- Annoying that I have to make this closed, because I need the final catch-all.
type family Weaken (a :: Type) :: Type where
    Weaken (LenPfx size end a) = [Weaken a]
    Weaken (Vector n a) = [Weaken a]
    Weaken (Refined p a) = Weaken a
    Weaken Word8 = Natural
    Weaken (Both 'Strong a) = Both 'Weak (Weaken a)
    Weaken a = a

type family Switch (s :: Strength) a :: Type where
    Switch 'Strong a = a
    Switch 'Weak   a = Weaken a

bothWeak :: Both 'Weak Text
bothWeak = Both ["hi"] 10000

class StrongToWeak s w where
    strongToWeak :: s -> w

instance StrongToWeak Word8 Natural where
    strongToWeak = fromIntegral

class WeakToStrong w s where
    weakToStrong :: w -> Either String s

instance WeakToStrong Natural Word8 where
    weakToStrong = natToBounded

natToBounded :: forall i. (Integral i, Bounded i) => Natural -> Either String i
natToBounded n =
    if   n > fromIntegral (maxBound @i)
    then Left "TODO too large"
    else Right $ fromIntegral n

instance StrongToWeak a (Weaken a) => StrongToWeak (Both 'Strong a) (Both 'Weak a) where
    strongToWeak (Both l w) = Both l' w'
      where
        l' = map strongToWeak $ V.toList l
        w' = fromIntegral w
