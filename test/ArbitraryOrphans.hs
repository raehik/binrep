{-# OPTIONS_GHC -fno-warn-orphans #-}

module ArbitraryOrphans() where

import Test.QuickCheck ( Arbitrary )
import Binrep.Type.Int
import Data.Kind

-- TODO 2023-01-26 raehik: why does the following crash GHC
deriving via (a :: Type) instance Arbitrary a => Arbitrary (Endian end a)
--deriving newtype instance Arbitrary a => Arbitrary (Endian end a)
