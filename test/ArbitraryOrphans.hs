{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE UndecidableInstances #-}

module ArbitraryOrphans() where

import Test.QuickCheck ( Arbitrary )
import Binrep.Type.Int ( I(..), IRep )

-- | Machine integers steal their underlying representation's instance.
deriving via (IRep sign size) instance Arbitrary (IRep sign size) => Arbitrary (I sign size e)
