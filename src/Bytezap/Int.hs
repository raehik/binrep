-- | Sized machine integers.

{-# LANGUAGE UnboxedTuples #-}
{-# LANGUAGE CPP #-}

module Bytezap.Int where

import Bytezap
--import Bytezap.Prim.Integer qualified as Prim
import GHC.Exts
import Data.Word
import GHC.Word
import Data.Int
import GHC.Int

w8 :: Word8 -> Write
w8 (W8# a#) = write 1 $ \addr# st# ->
    case writeWord8OffAddr# addr# 0# a# st# of
      st'# -> (# st'#, addr# `plusAddr#` 1# #)
{-# INLINE w8 #-}

w16 :: Word16 -> Write
w16 (W16# a#) = write 2 $ \addr# st# ->
    case writeWord16OffAddr# addr# 0# a# st# of
      st'# -> (# st'#, addr# `plusAddr#` 2# #)
{-# INLINE w16 #-}

w32 :: Word32 -> Write
w32 (W32# a#) = write 4 $ \addr# st# ->
    case writeWord32OffAddr# addr# 0# a# st# of
      st'# -> (# st'#, addr# `plusAddr#` 4# #)
{-# INLINE w32 #-}

w64 :: Word64 -> Write
w64 (W64# a#) = write 8 $ \addr# st# ->
    case writeWord64OffAddr# addr# 0# a# st# of
      st'# -> (# st'#, addr# `plusAddr#` 8# #)
{-# INLINE w64 #-}

{-# INLINE w16le #-}
{-# INLINE w16be #-}
w16le, w16be :: Word16 -> Write
#ifdef WORDS_BIGENDIAN
w16le = w16 . byteSwap16
w16be = w16
#else
w16le = w16
w16be = w16 . byteSwap16
#endif

{-# INLINE w32le #-}
{-# INLINE w32be #-}
w32le, w32be :: Word32 -> Write
#ifdef WORDS_BIGENDIAN
w32le = w32 . byteSwap32
w32be = w32
#else
w32le = w32
w32be = w32 . byteSwap32
#endif

{-# INLINE w64le #-}
{-# INLINE w64be #-}
w64le, w64be :: Word64 -> Write
#ifdef WORDS_BIGENDIAN
w64le = w64 . byteSwap64
w64be = w64
#else
w64le = w64
w64be = w64 . byteSwap64
#endif

i8 :: Int8 -> Write
i8 (I8# a#) = write 1 $ \addr# st# ->
    case writeInt8OffAddr# addr# 0# a# st# of
      st'# -> (# st'#, addr# `plusAddr#` 1# #)
{-# INLINE i8 #-}

i16 :: Int16 -> Write
i16 (I16# a#) = write 2 $ \addr# st# ->
    case writeInt16OffAddr# addr# 0# a# st# of
      st'# -> (# st'#, addr# `plusAddr#` 2# #)
{-# INLINE i16 #-}

i32 :: Int32 -> Write
i32 (I32# a#) = write 4 $ \addr# st# ->
    case writeInt32OffAddr# addr# 0# a# st# of
      st'# -> (# st'#, addr# `plusAddr#` 4# #)
{-# INLINE i32 #-}

i64 :: Int64 -> Write
i64 (I64# a#) = write 8 $ \addr# st# ->
    case writeInt64OffAddr# addr# 0# a# st# of
      st'# -> (# st'#, addr# `plusAddr#` 8# #)
{-# INLINE i64 #-}

byteSwapI16 :: Int16 -> Int16
byteSwapI16 = undefined

byteSwapI32 :: Int32 -> Int32
byteSwapI32 = undefined

byteSwapI64 :: Int64 -> Int64
byteSwapI64 = undefined

{-# INLINE i16le #-}
{-# INLINE i16be #-}
i16le, i16be :: Int16 -> Write
#ifdef WORDS_BIGENDIAN
i16le = i16 . byteSwapI16
i16be = i16
#else
i16le = i16
i16be = i16 . byteSwapI16
#endif

{-# INLINE i32le #-}
{-# INLINE i32be #-}
i32le, i32be :: Int32 -> Write
#ifdef WORDS_BIGENDIAN
i32le = i32 . byteSwapI32
i32be = i32
#else
i32le = i32
i32be = i32 . byteSwapI32
#endif

{-# INLINE i64le #-}
{-# INLINE i64be #-}
i64le, i64be :: Int64 -> Write
#ifdef WORDS_BIGENDIAN
i64le = i64 . byteSwapI64
i64be = i64
#else
i64le = i64
i64be = i64 . byteSwapI64
#endif

-- TODO assumes 64-bit
int# :: Int# -> Write
int# a# = write 8 $ \addr# st# ->
    case writeIntOffAddr# addr# 0# a# st# of
      st'# -> (# st'#, addr# `plusAddr#` 8# #)
{-# INLINE int# #-}

{-

-- | Construct a 'Write' of the following length using the given primitive poke.
writeViaPrim
    :: Int#
    -> (forall s. Addr# -> Int# -> a -> State# s -> State# s)
    -> a -> Write
writeViaPrim len# writeOffPrim a = write (I# len#) $ \addr# os# st# ->
    case writeOffPrim addr# os# a st# of
      st'# -> (# st'#, os# +# len# #)
{-# INLINE writeViaPrim #-}

-}
