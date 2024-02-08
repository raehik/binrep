{-# LANGUAGE OverloadedStrings #-} -- for easy bytestring literals

module Main where

import Test.Tasty.Bench

import Data.Word
import Data.ByteString qualified as B

import GHC.Generics

import Binrep.BLen qualified as BLen
import Binrep.BLen.Ask qualified as BLenAsk

main :: IO ()
main = defaultMain
  [ bench "blen/plain/w8" $ nf BLen.blen       (255 :: Word8)
  , bench "blen/ask/w8"   $ nf BLenAsk.runBlen (255 :: Word8)
  , bench "blen/plain/words-and-bs" $ nf BLen.blen       dWordsAndBS
  , bench "blen/ask/words-and-bs"   $ nf BLenAsk.runBlen dWordsAndBS
  ]

data DWordsAndBS = DWordsAndBS
  { dWordsAndBSw8  :: Word8
  , dWordsAndBSw16 :: Word16
  , dWordsAndBSw32 :: Word32
  , dWordsAndBSw64 :: Word64
  , dWordsAndBSbs  :: B.ByteString
  } deriving stock Generic

dWordsAndBS :: DWordsAndBS
dWordsAndBS = DWordsAndBS 0 1 2 3 "quick brown fox and shit"

instance BLen.BLen DWordsAndBS where
    blen = BLen.blenGenericNonSum

instance BLenAsk.BLen DWordsAndBS where
    blen = BLenAsk.blenGenericNonSum
