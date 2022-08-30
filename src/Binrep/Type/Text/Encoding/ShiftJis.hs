{-# LANGUAGE CPP #-}

module Binrep.Type.Text.Encoding.ShiftJis where

import Refined

import Data.Text ( Text )

#ifdef HAVE_ICU
import Data.Text.ICU.Convert qualified as ICU
import System.IO.Unsafe qualified
import Data.ByteString qualified as B

import Binrep.Type.Text.Internal
#endif

data ShiftJis

-- | TODO Unsafely assume all 'Text's are valid Shift-JIS.
instance Predicate ShiftJis Text where validate _ _ = success

#ifdef HAVE_ICU
instance Encode ShiftJis where encode' = encodeViaTextICU' "Shift-JIS"
instance Decode ShiftJis where
    decode  = decodeText id $ decodeViaTextICU' "Shift-JIS"

-- | Encode some 'Text' to the given character set using text-icu.
--
-- No guarantees about correctness. Encodings are weird. e.g. Shift JIS's
-- yen/backslash problem is apparently to do with OSs treating it differently.
--
-- Expects a 'Text' that is confirmed valid for converting to the character set.
--
-- The charset must be valid, or it's exception time. See text-icu.
encodeViaTextICU :: String -> Text -> IO B.ByteString
encodeViaTextICU charset t = do
    conv <- ICU.open charset Nothing
    return $ ICU.fromUnicode conv t

encodeViaTextICU' :: String -> Text -> B.ByteString
encodeViaTextICU' charset t =
    System.IO.Unsafe.unsafeDupablePerformIO $ encodeViaTextICU charset t

-- TODO Shitty library doesn't let us say how to handle errors. Apparently, the
-- only solution is to scan through the resulting 'Text' to look for @\SUB@
-- characters, or lie about correctness. Sigh.
decodeViaTextICU :: String -> B.ByteString -> IO (Either String Text)
decodeViaTextICU charset t = do
    conv <- ICU.open charset Nothing
    return $ Right $ ICU.toUnicode conv t

decodeViaTextICU' :: String -> B.ByteString -> Either String Text
decodeViaTextICU' charset t = do
    System.IO.Unsafe.unsafeDupablePerformIO $ decodeViaTextICU charset t
#endif
