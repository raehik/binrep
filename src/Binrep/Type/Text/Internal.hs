{-# LANGUAGE AllowAmbiguousTypes #-}

module Binrep.Type.Text.Internal where

import Data.Text ( Text )
import Data.ByteString qualified as B
import Refined
import Refined.Unsafe ( reallyUnsafeRefine )

import System.IO.Unsafe qualified
import Control.Exception qualified
import Data.Text.Encoding.Error qualified
import Data.Either.Combinators qualified as Either

type Bytes = B.ByteString

-- | A string of a given encoding, stored in the 'Text' type.
--
-- Essentially 'Text' carrying a proof that it can be successfully encoded into
-- the given encoding. For example, @'AsText' 'ASCII'@ means the 'Text' stored
-- is pure ASCII.
type AsText enc = Refined enc Text

-- | Bytestring encoders for text validated for a given encoding.
class Encode enc where
    -- | Encode text to bytes. Internal function, use 'encode'.
    encode' :: Text -> Bytes

class Decode enc where
    -- | Decode a 'ByteString' to 'Text' with an explicit encoding.
    --
    -- This is intended to be used with visible type applications.
    decode :: Bytes -> Either String (AsText enc)

--------------------------------------------------------------------------------
-- Internal helpers

-- | Helper for decoding a 'Bytes' to a 'Text' tagged with its encoding.
decodeText
    :: forall enc e
    .  (e -> String) -> (Bytes -> Either e Text) -> Bytes
    -> Either String (AsText enc)
decodeText g f = Either.mapBoth g reallyUnsafeRefine . f

-- | Run an unsafe decoder safely.
--
-- Copied from @Data.Text.Encoding.decodeUtf8'@, so should be bulletproof?
wrapUnsafeDecoder
    :: (Bytes -> Text)
    -> Bytes -> Either Data.Text.Encoding.Error.UnicodeException Text
wrapUnsafeDecoder f =
      System.IO.Unsafe.unsafeDupablePerformIO
    . Control.Exception.try
    . Control.Exception.evaluate
    . f
