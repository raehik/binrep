-- | Constant-size data.
--
-- This is a predicate and a type, but it's really intended for constant-size
-- bytestrings. It's just easier to define over more types.

{-# LANGUAGE OverloadedStrings #-}

module Binrep.Type.Sized where

import Binrep
import Binrep.Util ( tshow )

import Refined
import Refined.Unsafe

import GHC.TypeNats
import Data.Typeable
import GHC.Exts ( proxy#, Proxy# )
import Data.Serialize qualified as Cereal
import Data.ByteString qualified as B

data Size (n :: Natural)

type Sized n a = Refined (Size n) a

type instance CBLen (Sized n a) = n

deriving anyclass instance KnownNat n => BLen (Sized n a)

instance (BLen a, KnownNat n) => Predicate (Size n) a where
    validate p a
      | len > n
          = throwRefineOtherException (typeRep p) $
                   "not correctly sized: " <> tshow len <> " /= " <> tshow n
      | otherwise = success
      where
        n = natVal' (proxy# :: Proxy# n)
        len = blen a

instance Put a => Put (Sized n a) where
    put = put . unrefine

-- TODO explain safety
instance KnownNat n => Get (Sized n B.ByteString) where
    get = do
        bs <- Cereal.getBytes $ fromIntegral n
        return $ reallyUnsafeRefine bs
      where
        n = natVal' (proxy# :: Proxy# n)
