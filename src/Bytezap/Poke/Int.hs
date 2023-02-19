{-# LANGUAGE UnboxedTuples #-}
{-# LANGUAGE CPP #-}

module Bytezap.Poke.Int where

import Bytezap
import GHC.Exts
import Data.Word
import GHC.Word
import Data.Int
import GHC.Int

w8 :: Word8 -> Poke
w8 (W8# a#) = Poke $ \addr# st# ->
    case writeWord8OffAddr# addr# 0# a# st# of
      st'# -> (# st'#, addr# `plusAddr#` 1# #)
{-# INLINE w8 #-}

w16 :: Word16 -> Poke
w16 (W16# a#) = Poke $ \addr# st# ->
    case writeWord16OffAddr# addr# 0# a# st# of
      st'# -> (# st'#, addr# `plusAddr#` 2# #)
{-# INLINE w16 #-}

w32 :: Word32 -> Poke
w32 (W32# a#) = Poke $ \addr# st# ->
    case writeWord32OffAddr# addr# 0# a# st# of
      st'# -> (# st'#, addr# `plusAddr#` 4# #)
{-# INLINE w32 #-}

w64 :: Word64 -> Poke
w64 (W64# a#) = Poke $ \addr# st# ->
    case writeWord64OffAddr# addr# 0# a# st# of
      st'# -> (# st'#, addr# `plusAddr#` 8# #)
{-# INLINE w64 #-}

{-# INLINE w16le #-}
{-# INLINE w16be #-}
w16le, w16be :: Word16 -> Poke
#ifdef WORDS_BIGENDIAN
w16le = w16 . byteSwap16
w16be = w16
#else
w16le = w16
w16be = w16 . byteSwap16
#endif

{-# INLINE w32le #-}
{-# INLINE w32be #-}
w32le, w32be :: Word32 -> Poke
#ifdef WORDS_BIGENDIAN
w32le = w32 . byteSwap32
w32be = w32
#else
w32le = w32
w32be = w32 . byteSwap32
#endif

{-# INLINE w64le #-}
{-# INLINE w64be #-}
w64le, w64be :: Word64 -> Poke
#ifdef WORDS_BIGENDIAN
w64le = w64 . byteSwap64
w64be = w64
#else
w64le = w64
w64be = w64 . byteSwap64
#endif

i8 :: Int8 -> Poke
i8 (I8# a#) = Poke $ \addr# st# ->
    case writeInt8OffAddr# addr# 0# a# st# of
      st'# -> (# st'#, addr# `plusAddr#` 1# #)
{-# INLINE i8 #-}

i16 :: Int16 -> Poke
i16 (I16# a#) = Poke $ \addr# st# ->
    case writeInt16OffAddr# addr# 0# a# st# of
      st'# -> (# st'#, addr# `plusAddr#` 2# #)
{-# INLINE i16 #-}

i32 :: Int32 -> Poke
i32 (I32# a#) = Poke $ \addr# st# ->
    case writeInt32OffAddr# addr# 0# a# st# of
      st'# -> (# st'#, addr# `plusAddr#` 4# #)
{-# INLINE i32 #-}

i64 :: Int64 -> Poke
i64 (I64# a#) = Poke $ \addr# st# ->
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
i16le, i16be :: Int16 -> Poke
#ifdef WORDS_BIGENDIAN
i16le = i16 . byteSwapI16
i16be = i16
#else
i16le = i16
i16be = i16 . byteSwapI16
#endif

{-# INLINE i32le #-}
{-# INLINE i32be #-}
i32le, i32be :: Int32 -> Poke
#ifdef WORDS_BIGENDIAN
i32le = i32 . byteSwapI32
i32be = i32
#else
i32le = i32
i32be = i32 . byteSwapI32
#endif

{-# INLINE i64le #-}
{-# INLINE i64be #-}
i64le, i64be :: Int64 -> Poke
#ifdef WORDS_BIGENDIAN
i64le = i64 . byteSwapI64
i64be = i64
#else
i64le = i64
i64be = i64 . byteSwapI64
#endif

-- TODO assumes 64-bit
int# :: Int# -> Poke
int# a# = Poke $ \addr# st# ->
    case writeIntOffAddr# addr# 0# a# st# of
      st'# -> (# st'#, addr# `plusAddr#` 8# #)
{-# INLINE int# #-}
