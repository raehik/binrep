{-
The StrongToWeak, WeakToStrong typeclasses are plain dictionaries. There's only
ever one correct way to go between two types, so putting it in a typeclass saves
on definitions.

I might like to use fundeps? But I can't with the overlappable base instance.

I need the overlappables to end the recursive constraint chains. They are a part
of the composability. But they also prevent me doing some things, like I can't
use refineds in `a` any more. That makes it seem like I'm doing the wrong thing
here.

TODO

  * typeclass methods should be internal, user should use methods that reorder
    the typevars for better visible type application
-}

{-# LANGUAGE OverloadedStrings #-}

module Binrep.Wip.Strength where

import Binrep
import Binrep.Generic
import Binrep.Type.Common ( Endianness(..) )
import Binrep.Type.Int
import Binrep.Type.LenPfx
import Binrep.Type.NullPadded

import Refined hiding ( Weaken )

import Data.Vector.Sized ( Vector )
import Data.Vector.Sized qualified as V
import Data.Kind ( Type )
import Data.Word ( Word8 )
import Data.Text ( Text )
import GHC.TypeNats

import GHC.Generics ( Generic)
import Data.Data ( Typeable, Data )

import Data.Either.Combinators ( mapLeft )

import Data.ByteString qualified as B

data Strength = Strong | Weak

data Both (s :: Strength) a = Both
  { bothList  :: Switch s (Vector 1 a)
  , bothWord8 :: Switch s Word8
  }
deriving stock instance Show a => Show (Both 'Weak a)
deriving stock instance Show a => Show (Both 'Strong a)

-- Annoying that I have to make this closed, because I need the final catch-all.
-- I guess the solution is to make it open, and handle each weakest option
-- manually.
type family Weaken (a :: Type) :: Type

-- recursive cases
type instance Weaken (LenPfx size end a) = [a]
type instance Weaken (Vector n a) = [a]
type instance Weaken (Refined p a) = a
type instance Weaken [a] = [a]
type instance Weaken (Both 'Strong a) = Both 'Weak a

-- primitives
type instance Weaken Word8 = Natural
type instance Weaken (I 'U size end) = Natural
type instance Weaken (I 'S size end) = Integer

-- as-is
type instance Weaken Text = Text
type instance Weaken Char = Char
type instance Weaken B.ByteString = B.ByteString

type family Switch (s :: Strength) a :: Type where
    Switch 'Strong a = a
    Switch 'Weak   a = Weaken a

bothWeak :: Both 'Weak Text
bothWeak = Both ["hi"] 255

bothWeakStr :: Both 'Weak String
bothWeakStr = Both ["hi"] 255

class StrongToWeak s w where
    strongToWeak :: s -> w

{-
instance {-# OVERLAPPABLE #-} StrongToWeak a a where
    strongToWeak = id
-}

instance StrongToWeak Word8 Natural where
    strongToWeak = fromIntegral

instance (irep ~ IRep 'U size, Integral irep) => StrongToWeak (I 'U size end) Natural where
    strongToWeak = fromIntegral

instance (irep ~ IRep 'S size, Integral irep) => StrongToWeak (I 'S size end) Integer where
    strongToWeak = fromIntegral

instance StrongToWeak (Vector n a) [a] where
    strongToWeak = V.toList

instance StrongToWeak (Refined p a) a where
    strongToWeak = unrefine

class WeakToStrong w s where
    weakToStrong :: w -> Either String s

{-
instance {-# OVERLAPPABLE #-} WeakToStrong a a where
    weakToStrong = Right
-}

instance WeakToStrong Natural Word8 where
    weakToStrong = natToBounded

instance (irep ~ IRep 'U size, Integral irep, Bounded irep) => WeakToStrong Natural (I 'U size end) where
    weakToStrong = natToBounded

instance (irep ~ IRep 'S size, Integral irep, Bounded irep) => WeakToStrong Integer (I 'S size end) where
    weakToStrong = intToBounded

instance KnownNat n => WeakToStrong [a] (Vector n a) where
    weakToStrong w = case V.fromList w of Nothing -> Left "TODO nope"
                                          Just s  -> Right s

instance Predicate p a => WeakToStrong a (Refined p a) where
    weakToStrong = mapLeft show . refine

natToBounded :: forall i. (Integral i, Bounded i) => Natural -> Either String i
natToBounded n =
    if   n > fromIntegral (maxBound @i)
    then Left "TODO too large"
    else Right $ fromIntegral n

intToBounded :: forall i. (Integral i, Bounded i) => Integer -> Either String i
intToBounded n = error "TODO"

instance StrongToWeak (Both 'Strong a) (Both 'Weak a) where
    strongToWeak (Both l w) = Both l' w'
      where
        l' = strongToWeak @_ @[a] l
        w' = strongToWeak w

instance WeakToStrong (Both 'Weak a) (Both 'Strong a) where
    weakToStrong (Both l w) = do
        w' <- weakToStrong w
        l' <- weakToStrong l
        return $ Both l' w'

data ExEnum
  = ExEnum1This
  | ExEnum2That
    deriving stock (Generic, Typeable, Data, Show, Eq)

instance BLen ExEnum where blen = blenGeneric cDef
instance Put  ExEnum where put  = putGeneric  cDef
instance Get  ExEnum where get  = getGeneric  cDef

data ExProd (s :: Strength) a = ExProd
  { exProdIndex :: Switch s (I 'U 'I4 'LE)
  , exProdEnum  :: ExEnum
  , exProdStr1  :: Switch s (NullPadded 8 a)
  , exProdStr2  :: Switch s (NullPadded 4 a)
  } deriving stock (Generic, Typeable)
deriving stock instance Show a => Show (ExProd 'Strong a)
deriving stock instance Eq   a => Eq   (ExProd 'Strong a)

deriving stock instance (Typeable a, Data a) => Data (ExProd 'Weak a)
deriving stock instance Show a => Show (ExProd 'Weak a)
deriving stock instance Eq a => Eq (ExProd 'Weak a)

instance Functor (ExProd 'Weak) where
    fmap f (ExProd i e s1 s2) = ExProd i e (f s1) (f s2)

instance BLen a => BLen (ExProd 'Strong a) where blen = blenGeneric cDef
instance (Put a, BLen a) => Put (ExProd 'Strong a) where put = putGeneric cDef
instance (Get a, BLen a) => Get (ExProd 'Strong a) where get = getGeneric  cDef

instance StrongToWeak (ExProd 'Strong a) (ExProd 'Weak a) where
    strongToWeak (ExProd i e s1 s2) = ExProd i' e s1' s2'
      where
        i'  = strongToWeak i
        s1' = strongToWeak s1
        s2' = strongToWeak s2

instance BLen a => WeakToStrong (ExProd 'Weak a) (ExProd 'Strong a) where
    weakToStrong (ExProd i e s1 s2) = do
        i' <- weakToStrong i
        s1' <- weakToStrong s1
        s2' <- weakToStrong s2
        return $ ExProd i' e s1' s2'

exProdWeak :: ExProd 'Weak Text
exProdWeak = ExProd 0 ExEnum1This "hello" "hiaa"
