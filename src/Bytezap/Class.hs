module Bytezap.Class where

import Bytezap
import Bytezap.Bytes qualified as W
import Bytezap.Int qualified as W

import Data.ByteString ( ByteString )
import Data.Word
import Data.Int

class Put a where put :: a -> Write

instance Put Write where
    {-# INLINE put #-}
    put = id

instance Put ByteString where
    {-# INLINE put #-}
    put = W.byteString

instance Put Word8 where
    {-# INLINE put #-}
    put = W.w8

instance Put Word16 where
    {-# INLINE put #-}
    put = W.w16

instance Put Word32 where
    {-# INLINE put #-}
    put = W.w32

instance Put Word64 where
    {-# INLINE put #-}
    put = W.w64

instance Put Int8 where
    {-# INLINE put #-}
    put = W.i8

instance Put Int16 where
    {-# INLINE put #-}
    put = W.i16

instance Put Int32 where
    {-# INLINE put #-}
    put = W.i32

instance Put Int64 where
    {-# INLINE put #-}
    put = W.i64
