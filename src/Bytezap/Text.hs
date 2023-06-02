{-# LANGUAGE UnboxedTuples #-}
{-# LANGUAGE CPP #-}

module Bytezap.Text where

import Bytezap
import Bytezap.Int

import Data.Text.Internal

-- unused import warnings due to messy CPP
import Bytezap.Bytes
import Data.Text.Array qualified as A
import GHC.Exts

import Data.Char ( ord )
import Data.Foldable ( foldl' )
import Data.Bits ( shiftR, (.&.) )

textUtf8 :: Text -> Write
{-# INLINE textUtf8 #-}
#if MIN_VERSION_text(2,0,0)
textUtf8 (Text (A.ByteArray arr#) (I# off#) len@(I# len#)) =
    Write len $ pokeByteArray# arr# off# len#
#else
textUtf8 = error "Bytezap.Text.textUtf8: cba for text-1"
#endif

-- TODO adapted from utf8-string
charUtf8 :: Char -> Write
charUtf8 = go . ord
 where
  go oc
   | oc <= 0x7f       = w8 $ fromIntegral oc

   | oc <= 0x7ff      =    w8 (fromIntegral (0xc0 + (oc `shiftR` 6)))
                        <> w8 (fromIntegral (0x80 + oc .&. 0x3f))

   | oc <= 0xffff     =    w8 (fromIntegral (0xe0 + (oc `shiftR` 12)))
                        <> w8 (fromIntegral (0x80 + ((oc `shiftR` 6) .&. 0x3f)))
                        <> w8 (fromIntegral (0x80 + oc .&. 0x3f))
   | otherwise        =    w8 (fromIntegral (0xf0 + (oc `shiftR` 18)))
                        <> w8 (fromIntegral (0x80 + ((oc `shiftR` 12) .&. 0x3f)))
                        <> w8 (fromIntegral (0x80 + ((oc `shiftR` 6) .&. 0x3f)))
                        <> w8 (fromIntegral (0x80 + oc .&. 0x3f))
{-# INLINE charUtf8 #-}

-- | TODO
--
-- In a perfect world, functions like this would not exist. But this is not a
-- perfect world. 'String's suck for a number of reasons. One big one is that
-- they are horrendous to serialize. Worse, as of GHC 9.6, type-level strings
-- only reflect to 'String'. This function does the best it can to efficiently
-- serialize 'String's. It would be much easier and probably similarly fast to
-- go through 'Text' instead, but who doesn't like a little challenge?
stringUtf8 :: String -> Write
stringUtf8 = foldl' (\w c -> w <> charUtf8 c) mempty
{-# INLINE stringUtf8 #-}
