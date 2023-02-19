{-# LANGUAGE OverloadedStrings #-}

module Binrep.LawsSpec where

import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck.Instances.ByteString()
import Generic.Random
import Test.QuickCheck
import ArbitraryOrphans()

--import Binrep
--import Binrep.Generic
import Binrep.Type.Int
import Binrep.Type.Common ( Endianness(..) )
import Data.Word
import Data.ByteString qualified as B
import GHC.Generics ( Generic )

import Control.Exception ( evaluate )

spec :: Spec
spec = do
    prop "put is identity on ByteString" $ do
      \(bs :: B.ByteString) -> serialize bs  `shouldBe` bs
    prop "parse-print roundtrip isomorphism (ByteString)" $ do
      \(bs :: B.ByteString) -> runGet (serialize bs) `shouldBe` Right (bs, "")
    prop "parse-print roundtrip isomorphism (generic, sum tag via nullterm constructor)" $ do
      \(d :: D) -> runGet (serialize d) `shouldBe` Right (d, "")
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

dCfg :: Cfg (AsByteString 'C)
dCfg = cfg cSumTagNullTerm

instance BLen D where blen = blenGeneric dCfg
instance Put  D where put  = putGeneric  dCfg
instance Get  D where get  = getGeneric  dCfg

data DNoSum = DNoSum Word8 W1 W2LE W8BE
  | DNoSumBad
    deriving stock (Generic, Eq, Show)
deriving via (GenericArbitraryU `AndShrinking` DNoSum) instance Arbitrary DNoSum

instance BLen DNoSum where blen = blenGeneric cNoSum
instance Put  DNoSum where put  = putGeneric  cNoSum
instance Get  DNoSum where get  = getGeneric  cNoSum
