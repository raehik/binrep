{-# LANGUAGE OverloadedStrings #-}

module Raehik.Validate.Example where

import Raehik.Validate
import Raehik.Validate.Generic
import Binrep
import Binrep.Generic
import Binrep.Type.NullPadded

import Data.Word ( Word8 )
import Data.Int  ( Int8 )
import Data.Text ( Text )
import Data.Text.Encoding qualified as Text
import Data.ByteString qualified as B
import Data.Vector.Sized ( Vector )

import GHC.Generics ( Generic )
import Data.Data ( Data )

type V  = 'Validated
type UV = 'Unvalidated

bothWeak :: Both 'Unvalidated Text
bothWeak = Both ["hi"] 255

bothWeakStr :: Both 'Unvalidated String
bothWeakStr = Both ["hi"] 255

data ExEnum
  = ExEnum1This
  | ExEnum2That
    deriving stock (Generic, Data, Show, Eq)

instance BLen ExEnum where blen = blenGeneric cDef
instance Put  ExEnum where put  = putGeneric  cDef
instance Get  ExEnum where get  = getGeneric  cDef

data ExProd (v :: Validation) a = ExProd
  { exProdIndex :: Switch v Int8
  , exProdEnum  :: ExEnum
  , exProdStr1  :: Switch v (NullPadded 8 a)
  , exProdStr2  :: Switch v (NullPadded 4 a)
  } deriving stock (Generic)
deriving stock instance Show a => Show (ExProd V a)
deriving stock instance Eq   a => Eq   (ExProd V a)

deriving stock instance Data a => Data (ExProd UV a)
deriving stock instance Show a => Show (ExProd UV a)
deriving stock instance Eq a => Eq (ExProd UV a)

instance Functor (ExProd UV) where
    fmap f (ExProd i e s1 s2) = ExProd i e (f s1) (f s2)

instance BLen a => BLen (ExProd V a) where blen = blenGeneric cDef
instance (Put a, BLen a) => Put (ExProd V a) where put = putGeneric cDef
instance (Get a, BLen a) => Get (ExProd V a) where get = getGeneric  cDef

instance           Weaken     (ExProd V  a) (ExProd UV a) where weaken     = weakenGeneric
instance BLen a => Strengthen (ExProd UV a) (ExProd V  a) where strengthen = strengthenGeneric

exProdUV :: ExProd UV Text
exProdUV = ExProd (-129) ExEnum1This "hello" "hiaa"

exProdV :: Either String (ExProd V B.ByteString)
exProdV = strengthen $ fmap Text.encodeUtf8 exProdUV

data Both (v :: Validation) a = Both
  { bothList  :: Switch v (Vector 1 a)
  , bothWord8 :: Switch v Word8
  } deriving (Generic)
deriving stock instance Show a => Show (Both UV a)
deriving stock instance Show a => Show (Both V  a)
type instance Weak (Both V a) = Both UV a

{-
instance Weaken (Both V a) (Both UV a) where
    weaken (Both l w) = Both l' w'
      where
        l' = weaken l
        w' = weaken w
-}
instance Weaken     (Both V  a) (Both UV a) where weaken     = weakenGeneric
instance Strengthen (Both UV a) (Both V  a) where strengthen = strengthenGeneric

{-
instance Strengthen (Both UV a) (Both V a) where
    strengthen (Both l w) = do
        l' <- strengthen l
        w' <- strengthen w
        return $ Both l' w'
-}
