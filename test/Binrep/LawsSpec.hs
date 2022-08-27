{-# LANGUAGE OverloadedStrings #-}

module Binrep.LawsSpec where

import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck.Instances.ByteString()
import Generic.Random
import Test.QuickCheck
import ArbitraryOrphans()

import Binrep
import Binrep.Generic
import Binrep.Type.Int
import Binrep.Type.Common ( Endianness(..) )
import Binrep.Type.ByteString
import Data.Word ( Word8 )
import Data.ByteString qualified as B
import GHC.Generics ( Generic )

import Control.Exception ( evaluate )

spec :: Spec
spec = do
    prop "put is identity on ByteString" $ do
      \(bs :: B.ByteString) -> runPut bs  `shouldBe` bs
    prop "parse-print roundtrip isomorphism (ByteString)" $ do
      \(bs :: B.ByteString) -> runGet (runPut bs) `shouldBe` Right (bs, "")
    prop "parse-print roundtrip isomorphism (generic, sum tag via nullterm constructor)" $ do
      \(d :: D) -> runGet (runPut d) `shouldBe` Right (d, "")
    prop "serializing a type with an incorrect generic derivation throws an exception" $ do
      \(d :: DNoSum) -> do
        let evaluateShouldThrow a = evaluate a `shouldThrow` (\case EDerivedSumInstanceWithNonSumCfg -> True)
        evaluateShouldThrow (blen d)
        evaluateShouldThrow (runPut d)
    prop "parsing a type with an incorrect generic derivation fails" $ do
      \(bs :: B.ByteString) -> do
        let e = EGeneric "DNoSum" $ EGenericSum $ EGenericSumTag $ EBase ENoVoid
        runGet @DNoSum bs `shouldBe` Left e

--------------------------------------------------------------------------------

type W1   = (I 'U 'I1 'LE)
type W2LE = (I 'U 'I2 'LE)
type W8BE = (I 'U 'I8 'BE)

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
