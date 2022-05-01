-- | Pretty bytestrings via printing each byte as two hex digits.
--
-- This is primarily for aeson and when we want better 'show'ing of non-textual
-- bytestrings. It's not really binrep-related, but it needs _somewhere_ to go
-- and my projects that need it usually also touch binrep, so here it is.

module Binrep.Extra.HexByteString where

import GHC.Generics ( Generic )
import Data.Data ( Typeable, Data )

import Data.ByteString qualified as B
import Data.ByteString.Short qualified as B.Short
import Data.Char qualified as Char
import Data.Word
import Data.Text qualified as Text
import Data.Text ( Text )
import Data.List as List

import Text.Megaparsec hiding ( parse )
import Text.Megaparsec.Char qualified as MC
import Data.Void

import Data.Aeson

-- No harm in being polymorphic over the byte representation.
newtype Hex a = Hex { unHex :: a }
    deriving stock (Generic, Typeable, Data)
    deriving Eq via a

-- But most users will probably just want this.
type HexByteString = Hex B.ByteString

instance Show (Hex B.ByteString) where
    show = Text.unpack . prettyHexByteString B.unpack . unHex

instance FromJSON (Hex B.ByteString) where
    parseJSON = withText "hex bytestring" $ \t ->
        case parseMaybe @Void (parseHexByteString B.pack) t of
          Nothing -> fail "failed to parse hex bytestring (TODO)"
          Just t' -> pure (Hex t')

instance ToJSON   (Hex B.ByteString) where
    toJSON = String . prettyHexByteString B.unpack . unHex

instance Show (Hex B.Short.ShortByteString) where
    show = Text.unpack . prettyHexByteString B.Short.unpack . unHex

instance FromJSON (Hex B.Short.ShortByteString) where
    parseJSON = withText "hex bytestring" $ \t ->
        case parseMaybe @Void (parseHexByteString B.Short.pack) t of
          Nothing -> fail "failed to parse hex bytestring (TODO)"
          Just t' -> pure (Hex t')

instance ToJSON   (Hex B.Short.ShortByteString) where
    toJSON = String . prettyHexByteString B.Short.unpack . unHex

-- | A hex bytestring looks like this: @00 01 89 8a   FEff@. You can mix and
-- match capitalization and spacing, but I prefer to space each byte, full caps.
parseHexByteString
    :: (MonadParsec e s m, Token s ~ Char)
    => ([Word8] -> a) -> m a
parseHexByteString pack = pack <$> parseHexByte `sepBy` MC.hspace

-- | Parse a byte formatted as two hex digits e.g. EF. You _must_ provide both
-- nibbles e.g. @0F@, not @F@. They cannot be spaced e.g. @E F@ is invalid.
--
-- Returns a value 0-255, so can fit in any Num type that can store that.
parseHexByte :: (MonadParsec e s m, Token s ~ Char, Num a) => m a
parseHexByte = do
    c1 <- MC.hexDigitChar
    c2 <- MC.hexDigitChar
    return $ 0x10 * fromIntegral (Char.digitToInt c1) + fromIntegral (Char.digitToInt c2)

-- | Pretty print to default format @00 12 AB FF@: space between each byte, all
--   caps.
--
-- This format I consider most human readable. I prefer caps to draw attention
-- to this being data instead of text (you don't see that many capital letters
-- packed together in prose).
prettyHexByteString :: (a -> [Word8]) -> a -> Text
prettyHexByteString unpack =
      Text.concat
    . List.intersperse (Text.singleton ' ')
    . fmap (f . prettyHexByte Char.toUpper)
    . unpack
  where
    f :: (Char, Char) -> Text
    f (c1, c2) = Text.cons c1 $ Text.singleton c2

prettyHexByte :: (Char -> Char) -> Word8 -> (Char, Char)
prettyHexByte f w = (prettyNibble h, prettyNibble l)
  where
    (h,l) = fromIntegral w `divMod` 0x10
    prettyNibble = f . Char.intToDigit -- Char.intToDigit returns lower case

-- | Pretty print to "compact" format @0012abff@ (often output by hashers).
prettyHexByteStringCompact :: (a -> [Word8]) -> a -> Text
prettyHexByteStringCompact unpack =
    Text.concat . fmap (f . prettyHexByte id) . unpack
  where
    f :: (Char, Char) -> Text
    f (c1, c2) = Text.cons c1 $ Text.singleton c2
