module Binrep.Put.Bytezap where

import Binrep.BLen.Simple

import Bytezap
import Bytezap.Poke.Bytes
import Bytezap.Poke.Int

import Data.Void
import Data.Word
import Data.Int
import Data.ByteString qualified as B

class Put a where put :: a -> Poke

runPut :: (BLen a, Put a) => a -> B.ByteString
runPut a = runPoke (blen a) (put a)
{-# INLINE runPut #-}

-- | Unit type serializes to nothing. How zen.
instance Put () where
    {-# INLINE put #-}
    put = mempty

instance Put Void where
    {-# INLINE put #-}
    put = absurd

instance Put Write where
    {-# INLINE put #-}
    put = writePoke

instance Put B.ByteString where
    {-# INLINE put #-}
    put = byteString

instance Put Word8 where
    {-# INLINE put #-}
    put = w8

instance Put Int8  where
    {-# INLINE put #-}
    put = i8
