{-# LANGUAGE OverloadedStrings #-}

module Binrep.Extra.HexByteStringSpec ( spec ) where

import Binrep.Extra.HexByteString
import Test.Hspec

import Data.ByteString qualified as B
import Text.Megaparsec
import Data.Void ( Void )

megaparsecParseFromCharStream :: forall s a. (Stream s, Token s ~ Char) => Parsec Void s a -> s -> Maybe a
megaparsecParseFromCharStream parser text = parseMaybe parser text

spec :: Spec
spec = do
    let bs = B.pack
    describe "parse" $ do
      let p = megaparsecParseFromCharStream @String (parseHexByteString B.pack)
      it "parses valid hex bytestrings" $ do
        p "00" `shouldBe` Just (bs [0x00])
        p "FF" `shouldBe` Just (bs [0xFF])
        p "1234" `shouldBe` Just (bs [0x12, 0x34])
        p "01 9A FE" `shouldBe` Just (bs [0x01, 0x9A, 0xFE])
        p "FFFFFFFF" `shouldBe` Just (B.replicate 4 0xFF)
        p "12 34    AB CD" `shouldBe` Just (bs [0x12, 0x34, 0xAB, 0xCD])
      it "fails to parse invalid hex bytestrings" $ do
        p "-00" `shouldBe` Nothing
        p "FG" `shouldBe` Nothing
      it "fails to parse 0x prefix" $ do
        p   "1234" `shouldBe` Just (bs [0x12, 0x34])
        p "0x1234" `shouldBe` Nothing
    describe "print" $ do
      it "prints pretty hex bytestrings" $ do
        let p = prettyHexByteString B.unpack
        p (bs [0x5a, 0x7d]) `shouldBe` "5A 7D"
      it "prints compact hex bytestrings" $ do
        let pc = prettyHexByteStringCompact B.unpack
        pc (bs [0xab, 0x25]) `shouldBe` "ab25"
