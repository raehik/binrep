{-# LANGUAGE OverloadedStrings #-}

module Binrep.LawsSpec where

import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck.Instances.ByteString()
import Generic.Random
import Test.QuickCheck
import ArbitraryOrphans()

import Binrep
import Binrep.Generic ( nullTermCstrPfxTag )
import Binrep.Util.ByteOrder
import Data.Word
import Data.ByteString qualified as B
import GHC.Generics ( Generic )

import Generic.Data.Function.Traverse

spec :: Spec
spec = do
    prop "put is identity on ByteString" $ do
      \(bs :: B.ByteString) -> runPut bs  `shouldBe` bs
    prop "parse-print roundtrip isomorphism (ByteString)" $ do
      \(bs :: B.ByteString) -> runGet (runPut bs) `shouldBe` Right (bs, "")
    prop "parse-print roundtrip isomorphism (generic, sum tag via nullterm constructor)" $ do
      \(d :: D) -> runGet (runPut d) `shouldBe` Right (d, "")

--------------------------------------------------------------------------------

type W1   = Word8
type W2LE = ByteOrdered LE Word16
type W8BE = ByteOrdered BE Word64

data D
  = D01Bla     Word8 W1 W8BE
  | D23        W2LE  B.ByteString -- dangerous bytestring, must be last
  | DUnicode例 Word8
  | DSymbols_#
    deriving stock (Generic, Eq, Show)
deriving via (GenericArbitraryU `AndShrinking` D) instance Arbitrary D

instance BLen D where blen = blenGenericSum $ blen . nullTermCstrPfxTag
instance Put  D where put  = putGenericSum  $ put . nullTermCstrPfxTag
instance Get  D where get  = getGenericSum  $ eqShowPfxTagCfg nullTermCstrPfxTag

data DNoSum = DNoSum Word8 W1 W2LE W8BE
    deriving stock (Generic, Eq, Show)
deriving via (GenericArbitraryU `AndShrinking` DNoSum) instance Arbitrary DNoSum

instance BLen DNoSum where blen = blenGenericNonSum
instance Put  DNoSum where put  = putGenericNonSum
instance Get  DNoSum where get  = getGenericNonSum
