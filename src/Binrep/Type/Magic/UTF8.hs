{- | Inefficient attempt at UTF-8 magics.

To encode UTF-8 strings to bytestrings at compile time, we really need more
support from the compiler. We can go @Char -> Natural@, but we can't go @Natural
-> [Natural]@ where each value is @<= 255@. Doing so is hard without bit
twiddling.

The best we can do is get reify the 'Symbol' directly, then encode as UTF-8 at
runtime. It's a bit of a farce, and we can't derive a 'CBLen' instance, but
works just fine. Actually, I dunno, it might be faster than the bytewise magic
handling, depending on how GHC optimizes its instances.
-}

{-# LANGUAGE AllowAmbiguousTypes #-}

module Binrep.Type.Magic.UTF8 where

import Binrep
import Binrep.Util ( unsafePosIntToNat )

import GHC.TypeLits
import GHC.Exts ( proxy#, Proxy# )
import Data.Text qualified as Text
import Data.Text.Encoding qualified as Text
import Data.ByteString qualified as B
import Data.Serialize.Get qualified as Cereal

data MagicUTF8 (str :: Symbol) = MagicUTF8 deriving Show

symVal :: forall str. KnownSymbol str => String
symVal = symbolVal' (proxy# :: Proxy# str)

instance KnownSymbol str => BLen (MagicUTF8 str) where
    blen MagicUTF8 = unsafePosIntToNat $ B.length $ encodeStringUtf8 $ symVal @str

instance KnownSymbol str => Put  (MagicUTF8 str) where
    put  MagicUTF8 = put $ encodeStringUtf8 $ symVal @str

instance KnownSymbol str => Get  (MagicUTF8 str) where
    get = do
        let expected = encodeStringUtf8 $ symVal @str
        actual <- Cereal.getBytes $ B.length expected
        if   actual == expected
        then return MagicUTF8
        else fail $ "bad magic: expected "<>show expected<>", got "<>show actual

encodeStringUtf8 :: String -> B.ByteString
encodeStringUtf8 = Text.encodeUtf8 . Text.pack
