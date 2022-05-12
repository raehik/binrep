{-|
Note that @Array n B.ByteString == Sized n B.ByteString@.
-}

{-# LANGUAGE OverloadedStrings #-}

module Binrep.Type.Array where

import Binrep
import Binrep.Util ( tshow, unsafePosIntToNat )

import Refined
import Refined.Unsafe

import GHC.TypeNats
import Data.Typeable
import GHC.Exts ( proxy#, Proxy# )

import Data.ByteString qualified as B
import Data.Foldable qualified as Foldable
import Control.Monad ( replicateM )
import Data.Serialize qualified as Cereal

data Elements (n :: Natural)

type Array n a = Refined (Elements n) a

type instance CBLen (Array n B.ByteString) = n

instance BLen a => BLen (Array n a) where
    blen = blen . unrefine

instance (ALen a, KnownNat n) => Predicate (Elements n) a where
    validate p a
     | len > n
        = throwRefineOtherException (typeRep p) $
            "not correctly sized: "<>tshow len<>" /= " <>tshow n
     | otherwise = success
      where
        n = natVal' (proxy# :: Proxy# n)
        len = alen a

instance Put a => Put (Array n a) where put = put . unrefine

instance (Get a, KnownNat n) => Get (Array n [a]) where
    get = do
        as <- replicateM (fromIntegral n) get
        return $ reallyUnsafeRefine as
      where n = natVal' (proxy# :: Proxy# n)

instance KnownNat n => Get (Array n B.ByteString) where
    get = reallyUnsafeRefine <$> Cereal.getBytes (fromIntegral n)
      where n = natVal' (proxy# :: Proxy# n)

-- | TODO The "length" of an array type.
class ALen a where alen :: a -> Natural

-- | Bytestrings are byte-wise.
instance ALen B.ByteString where alen = blen

-- | List-likes are element-wise.
instance Foldable t => ALen (t a) where alen = unsafePosIntToNat . Foldable.length
