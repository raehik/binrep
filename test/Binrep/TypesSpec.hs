{-# LANGUAGE OverloadedStrings #-}

module Binrep.TypesSpec where

import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck.Instances.ByteString()

import Binrep
import Data.ByteString qualified as B

spec :: Spec
spec = do
    prop "put is identity on ByteString" $ do
      \(bs :: B.ByteString) -> runPut bs  `shouldBe` bs
    prop "parse-print roundtrip isomorphism (ByteString)" $ do
      \(bs :: B.ByteString) ->
        runGet (runPut bs) `shouldSatisfy`
          (\case Right (bs', "") -> bs == bs'; _ -> False)
