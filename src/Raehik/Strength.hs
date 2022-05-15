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

module Raehik.Strength where

import Binrep
import Binrep.Generic
--import Binrep.Type.Common ( Endianness(..) )
import Binrep.Type.Int
import Binrep.Type.LenPfx
import Binrep.Type.NullPadded

import Refined hiding ( strengthen, Weaken(..) )

import Data.Vector.Sized ( Vector )
import Data.Vector.Sized qualified as V
import Data.Kind ( Type )
import Data.Word ( Word8 )
import Data.Text ( Text )
import GHC.TypeNats

import GHC.Generics ( Generic)
import Data.Data ( Data )

import Data.Either.Combinators ( mapLeft )

import Data.ByteString qualified as B

import Data.Text.Encoding qualified as Text

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
type family Weak (a :: Type) :: Type

-- primitives
type instance Weak [a] = [a]
type instance Weak Word8 = Natural
type instance Weak (I 'U size end) = Natural
type instance Weak (I 'S size end) = Integer

-- as-is
type instance Weak Text = Text
type instance Weak Char = Char
type instance Weak B.ByteString = B.ByteString

type instance Weak (LenPfx size end a) = [a]
type instance Weak (Vector n a) = [a]
type instance Weak (Refined p a) = a
type instance Weak (Both 'Strong a) = Both 'Weak a

type family Switch (s :: Strength) a :: Type where
    Switch 'Strong a = a
    Switch 'Weak   a = Weak a

bothWeak :: Both 'Weak Text
bothWeak = Both ["hi"] 255

bothWeakStr :: Both 'Weak String
bothWeakStr = Both ["hi"] 255

class Weaken s w where
    weaken :: s -> w

{-
instance {-# OVERLAPPABLE #-} Weaken a a where
    weaken = id
-}

instance Weaken Word8 Natural where
    weaken = fromIntegral

instance (irep ~ IRep 'U size, Integral irep) => Weaken (I 'U size end) Natural where
    weaken = fromIntegral

instance (irep ~ IRep 'S size, Integral irep) => Weaken (I 'S size end) Integer where
    weaken = fromIntegral

instance Weaken (Vector n a) [a] where
    weaken = V.toList

instance Weaken (Refined p a) a where
    weaken = unrefine

class Strengthen w s where
    strengthen :: w -> Either String s

{-
instance {-# OVERLAPPABLE #-} Strengthen s s where
    strengthen = Right
-}

instance Strengthen Natural Word8 where
    strengthen = natToBounded

instance (irep ~ IRep 'U size, Integral irep, Bounded irep) => Strengthen Natural (I 'U size end) where
    strengthen = natToBounded

instance (irep ~ IRep 'S size, Integral irep, Bounded irep) => Strengthen Integer (I 'S size end) where
    strengthen = intToBounded

instance KnownNat n => Strengthen [a] (Vector n a) where
    strengthen w = case V.fromList w of Nothing -> Left "TODO nope"
                                        Just s  -> Right s

instance Predicate p a => Strengthen a (Refined p a) where
    strengthen = mapLeft show . refine

natToBounded :: forall i. (Integral i, Bounded i) => Natural -> Either String i
natToBounded n =
    if   n > fromIntegral (maxBound @i)
    then Left $ "todo" -- "can't fit "<>show n<>" into "<>typeOf 
    else Right $ fromIntegral n

intToBounded :: forall i. (Integral i, Bounded i) => Integer -> Either String i
intToBounded _n = error "TODO"

instance Weaken (Both 'Strong a) (Both 'Weak a) where
    weaken (Both l w) = Both l' w'
      where
        l' = weaken l
        w' = weaken w

instance Strengthen (Both 'Weak a) (Both 'Strong a) where
    strengthen (Both l w) = do
        l' <- strengthen l
        w' <- strengthen w
        return $ Both l' w'

data ExEnum
  = ExEnum1This
  | ExEnum2That
    deriving stock (Generic, Data, Show, Eq)

instance BLen ExEnum where blen = blenGeneric cDef
instance Put  ExEnum where put  = putGeneric  cDef
instance Get  ExEnum where get  = getGeneric  cDef

data ExProd (s :: Strength) a = ExProd
  { exProdIndex :: Switch s Word8
  , exProdEnum  :: ExEnum
  , exProdStr1  :: Switch s (NullPadded 8 a)
  , exProdStr2  :: Switch s (NullPadded 4 a)
  } deriving stock (Generic)
deriving stock instance Show a => Show (ExProd 'Strong a)
deriving stock instance Eq   a => Eq   (ExProd 'Strong a)

deriving stock instance Data a => Data (ExProd 'Weak a)
deriving stock instance Show a => Show (ExProd 'Weak a)
deriving stock instance Eq a => Eq (ExProd 'Weak a)

instance Functor (ExProd 'Weak) where
    fmap f (ExProd i e s1 s2) = ExProd i e (f s1) (f s2)

instance BLen a => BLen (ExProd 'Strong a) where blen = blenGeneric cDef
instance (Put a, BLen a) => Put (ExProd 'Strong a) where put = putGeneric cDef
instance (Get a, BLen a) => Get (ExProd 'Strong a) where get = getGeneric  cDef

instance Weaken (ExProd 'Strong a) (ExProd 'Weak a) where
    weaken (ExProd i e s1 s2) = ExProd i' e s1' s2'
      where
        i'  = weaken i
        s1' = weaken s1
        s2' = weaken s2

instance BLen a => Strengthen (ExProd 'Weak a) (ExProd 'Strong a) where
    strengthen (ExProd i e s1 s2) = do
        i'  <- strengthen i
        s1' <- strengthen s1
        s2' <- strengthen s2
        return $ ExProd i' e s1' s2'

exProdWeak :: ExProd 'Weak Text
exProdWeak = ExProd 255 ExEnum1This "hello" "hiaa"

exProdStrong :: Either String (ExProd 'Strong B.ByteString)
exProdStrong = strengthen $ fmap Text.encodeUtf8 exProdWeak
