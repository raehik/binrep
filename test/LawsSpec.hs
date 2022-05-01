{-# LANGUAGE OverloadedStrings #-}

module LawsSpec ( spec ) where

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
import Data.Word ( Word8 )
import Data.ByteString qualified as B
import GHC.Generics ( Generic )

spec :: Spec
spec = do
    prop "put is identity on ByteString" $ do
      \(bs :: B.ByteString) -> do
        runPut bs  `shouldBe` bs
    prop "parse-print roundtrip isomorphism (ByteString)" $ do
      \(bs :: B.ByteString) -> do
        runGet (runPut bs) `shouldBe` Right (bs, "")
    prop "parse-print roundtrip isomorphism (generic)" $ do
      \(d :: D) -> do
        runGet (runPut d) `shouldBe` Right (d, "")

--------------------------------------------------------------------------------

type W1   = (I 'U 'I1 'LE)
type W2LE = (I 'U 'I2 'LE)
type W8BE = (I 'U 'I8 'BE)

data D
  = D01Bla Word8 W1 W8BE
  | D23    W2LE B.ByteString
  | DFF1a2b
    deriving (Generic, Eq, Show)

dCfg :: Cfg W1
dCfg = Cfg { cSumTag = cSumTagHex $ take 2 . drop 1 }

instance BLen D where blen = blenGeneric dCfg
instance Put  D where put  = putGeneric  dCfg
instance Get  D where get  = getGeneric  dCfg

deriving via (GenericArbitraryU `AndShrinking` D) instance Arbitrary D
