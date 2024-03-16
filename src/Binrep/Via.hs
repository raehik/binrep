module Binrep.Via where

import Binrep.CBLen qualified as BR
import Binrep.BLen.Simple qualified as BR.Simple
import Binrep.Put ( Put )
import Binrep.Get ( Get )

-- | Identity newtype for using with @DerivingVia@.
newtype Binreply a = Binreply { unBinreply :: a }
    deriving stock Show
    deriving (BR.IsCBLen, BR.Simple.BLen, Put, Get) via a
