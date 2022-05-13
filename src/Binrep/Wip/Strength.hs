{-# LANGUAGE OverloadedStrings #-}

module Binrep.Wip.Strength where

import Refined hiding ( Weaken )
import Binrep.Type.LenPfx
import Data.Vector.Sized ( Vector )
import Data.Vector.Sized qualified as V
import Data.Kind ( Type )
import Data.Word ( Word8 )
import Data.Text ( Text )
import GHC.TypeNats

data Strength = Strong | Weak

data Both (s :: Strength) a = Both
  { bothList  :: Switch s (Vector 1 a)
  , bothWord8 :: Switch s Word8
  }
deriving stock instance Show (Weaken a) => Show (Both 'Weak a)
deriving stock instance Show a => Show (Both 'Strong a)

-- Annoying that I have to make this closed, because I need the final catch-all.
-- I guess the solution is to make it open, and handle each weakest option
-- manually.
type family Weaken (a :: Type) :: Type
type instance Weaken (LenPfx size end a) = [Weaken a]
type instance Weaken (Vector n a) = [Weaken a]
type instance Weaken (Refined p a) = Weaken a
type instance Weaken Word8 = Natural
type instance Weaken (Both 'Strong a) = Both 'Weak (Weaken a)
type instance Weaken String = String
type instance Weaken Text = Text
type instance Weaken [a] = [a]

type family Switch (s :: Strength) a :: Type where
    Switch 'Strong a = a
    Switch 'Weak   a = Weaken a

bothWeak :: Both 'Weak Text
bothWeak = Both ["hi"] 255

class StrongToWeak s w where
    strongToWeak :: s -> w

instance {-# OVERLAPPABLE #-} StrongToWeak a a where
    strongToWeak = id

instance StrongToWeak Word8 Natural where
    strongToWeak = fromIntegral

instance StrongToWeak (Vector n a) [a] where
    strongToWeak = V.toList

class WeakToStrong w s where
    weakToStrong :: w -> Either String s

instance {-# OVERLAPPABLE #-} WeakToStrong a a where
    weakToStrong = Right

instance WeakToStrong Natural Word8 where
    weakToStrong = natToBounded

instance KnownNat n => WeakToStrong [a] (Vector n a) where
    weakToStrong w = case V.fromList w of Nothing -> Left "TODO nope"
                                          Just s  -> Right s

natToBounded :: forall i. (Integral i, Bounded i) => Natural -> Either String i
natToBounded n =
    if   n > fromIntegral (maxBound @i)
    then Left "TODO too large"
    else Right $ fromIntegral n

instance StrongToWeak a (Weaken a) => StrongToWeak (Both 'Strong a) (Both 'Weak a) where
    strongToWeak (Both l w) = Both l' w'
      where
        l' = map strongToWeak $ strongToWeak @_ @[a] l
        w' = strongToWeak w

instance WeakToStrong (Weaken a) a => WeakToStrong (Both 'Weak a) (Both 'Strong a) where
    weakToStrong (Both l w) = do
        w' <- weakToStrong w
        l'' <- traverse (weakToStrong @_ @a) l
        l' <- weakToStrong l''
        return $ Both l' w'
