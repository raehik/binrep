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

import GHC.TypeLits
import GHC.Exts ( proxy#, Proxy# )
import Data.Text qualified as Text
import Data.Text.Encoding qualified as Text
import Data.ByteString qualified as B
import FlatParse.Basic qualified as FP
import Data.Store qualified as Store

data MagicUTF8 (str :: Symbol) = MagicUTF8 deriving Show

symVal :: forall str. KnownSymbol str => String
symVal = symbolVal' (proxy# :: Proxy# str)

instance KnownSymbol str => BLen (MagicUTF8 str) where
    blen = Store.VarSize $ \MagicUTF8 ->
        B.length $ encodeStringUtf8 $ symVal @str

instance KnownSymbol str => Put  (MagicUTF8 str) where
    put  MagicUTF8 = put $ encodeStringUtf8 $ symVal @str

instance KnownSymbol str => Get  (MagicUTF8 str) where
    get = do
        let expected = encodeStringUtf8 $ symVal @str
        actual <- FP.take $ B.length expected
        if   actual == expected
        then pure MagicUTF8
        else eBase $ EExpected expected actual

encodeStringUtf8 :: String -> B.ByteString
encodeStringUtf8 = Text.encodeUtf8 . Text.pack
