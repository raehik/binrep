{-# LANGUAGE UndecidableInstances #-} -- for 'TypeError'

module Binrep.Put.Bytezap where

import Bytezap
import Bytezap.Poke.Bytes
import Bytezap.Poke.Int
import Data.ByteString qualified as B
import Binrep.BLen.Simple

import Binrep.Util.Class
import GHC.TypeError

import Data.Void
import Data.Word
import Data.Int

class Put a where put :: a -> Poke

instance TypeError ENoEmpty => Put Void where put = undefined
instance TypeError ENoSum => Put (Either a b) where put = undefined

runPut :: (BLen a, Put a) => a -> B.ByteString
runPut a = runPoke (blen a) (put a)
{-# INLINE runPut #-}

instance Put Write where
    {-# INLINE put #-}
    put = writePoke

-- | Fairly useless because 'Poke' doesn't have a 'BLen' instance.
instance Put Poke where
    {-# INLINE put #-}
    put = id

-- | Unit type serializes to nothing. How zen.
instance Put () where
    {-# INLINE put #-}
    put = mempty

instance (Put l, Put r) => Put (l, r) where
    {-# INLINE put #-}
    put (l, r) = put l <> put r

instance Put a => Put [a] where
    {-# INLINE put #-}
    put = mconcat . map put

instance Put B.ByteString where
    {-# INLINE put #-}
    put = byteString

instance Put Word8 where
    {-# INLINE put #-}
    put = w8

instance Put Int8  where
    {-# INLINE put #-}
    put = i8
