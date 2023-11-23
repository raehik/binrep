-- | Poke operations on strict 'ByteString's and primitive byte arrays.

{-# LANGUAGE UnboxedTuples #-}

module Bytezap.Bytes where

import Bytezap

import GHC.Exts
import Data.ByteString qualified as B
import Data.ByteString.Internal qualified as B
import GHC.IO
import Data.Word
import Foreign.ForeignPtr
import Foreign.Marshal.Utils qualified

byteString :: B.ByteString -> Poke
byteString (B.BS fptr len) = pokeForeignPtr fptr len
{-# INLINE byteString #-}

pokeForeignPtr :: ForeignPtr Word8 -> Int -> Poke
pokeForeignPtr fptr len@(I# len#) = poke $ \addr# st# ->
    case unIO (memcpyForeignPtr (Ptr addr#) fptr len) st# of
      (# st'#, () #) -> (# st'#, addr# `plusAddr#` len# #)
{-# INLINE pokeForeignPtr #-}

memcpyForeignPtr :: Ptr Word8 -> ForeignPtr Word8 -> Int -> IO ()
memcpyForeignPtr ptrTo fptrFrom len =
    B.unsafeWithForeignPtr fptrFrom $ \ptrFrom ->
        Foreign.Marshal.Utils.copyBytes ptrTo ptrFrom len
{-# INLINE memcpyForeignPtr #-}

pokeByteArray# :: ByteArray# -> Int# -> Int# -> Poke
pokeByteArray# arr# off# len# = poke $ \addr# st# ->
    case copyByteArrayToAddr# arr# off# addr# len# st# of
      st'# -> (# st'#, addr# `plusAddr#` len# #)
{-# INLINE pokeByteArray# #-}

-- TODO this seems to work but like, really? wow lol
pokeByteReplicate :: Int -> Word8 -> Poke
pokeByteReplicate n@(I# n#) w8 = poke $ \addr# st# ->
    case unIO (Foreign.Marshal.Utils.fillBytes (Ptr addr#) w8 (fromIntegral n)) st# of
      (# st'#, _ #) -> (# st'#, addr# `plusAddr#` n# #)
{-# INLINE pokeByteReplicate #-}
