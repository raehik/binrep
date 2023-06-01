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
import Binrep.BLen.Simple.Generic ( blenGenericNonSum, blenGenericSum )
import Binrep.Type.Int
import Binrep.Type.Common ( Endianness(..) )
import Binrep.Type.NullTerminated
import Data.Word
import Data.ByteString qualified as B
import GHC.Generics ( Generic )

import Senserial.Sequential.Parse.Sum qualified as Senserial

spec :: Spec
spec = do
    prop "put is identity on ByteString" $ do
      \(bs :: B.ByteString) -> runPut bs  `shouldBe` bs
    prop "parse-print roundtrip isomorphism (ByteString)" $ do
      \(bs :: B.ByteString) -> runGet (runPut bs) `shouldBe` Right (bs, "")
    prop "parse-print roundtrip isomorphism (generic, sum tag via nullterm constructor)" $ do
      \(d :: D) -> runGet (runPut d) `shouldBe` Right (d, "")
    {- Previously, I had tests for asserting that using generic binrep instances
       which were configured incorrectly errored and/or gave certain exceptions.
       With the new generic instance design, this is no longer possible, since
       those cases have been promoted to the type level!
    -}

--------------------------------------------------------------------------------

type W1   = Word8
type W2LE = Endian 'LE Word16
type W8BE = Endian 'BE Word64

data D
  = D01Bla     Word8 W1 W8BE
  | D23        W2LE  B.ByteString -- dangerous bytestring, must be last
  | DUnicode例 Word8
  | DSymbols_#
    deriving stock (Generic, Eq, Show)
deriving via (GenericArbitraryU `AndShrinking` D) instance Arbitrary D

instance BLen D where blen = blenGenericSum $ blen . nullTermCstrPfxTag
instance Put  D where put  = putGenericSum  $ put . nullTermCstrPfxTag
instance Get  D where get  = getGenericSum  $ Senserial.eqShowPfxTagCfg nullTermCstrPfxTag

data DNoSum = DNoSum Word8 W1 W2LE W8BE
    deriving stock (Generic, Eq, Show)
deriving via (GenericArbitraryU `AndShrinking` DNoSum) instance Arbitrary DNoSum

instance BLen DNoSum where blen = blenGenericNonSum
instance Put  DNoSum where put  = putGenericNonSum
instance Get  DNoSum where get  = getGenericNonSum
