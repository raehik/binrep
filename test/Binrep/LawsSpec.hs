{-# LANGUAGE OverloadedStrings #-}

module Binrep.LawsSpec ( spec ) where

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

spec :: Spec
spec = do
    prop "put is identity on ByteString" $ do
      \(bs :: B.ByteString) -> runPut bs  `shouldBe` bs
    prop "parse-print roundtrip isomorphism (ByteString)" $ do
      \(bs :: B.ByteString) -> runGet (runPut bs) `shouldBe` Right (bs, "")
    prop "parse-print roundtrip isomorphism (generic, sum tag via nullterm constructor)" $ do
      \(d :: D) -> runGet (runPut d) `shouldBe` Right (d, "")

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
dCfg = Cfg { cSumTag = cSumTagNullTerm }

instance BLen D where blen = blenGeneric dCfg
instance Put  D where put  = putGeneric  dCfg
instance Get  D where get  = getGeneric  dCfg
