module Binrep.Via where

import Binrep.CBLen qualified as BR
import Binrep.BLen.Simple qualified as BR.Simple
import Binrep.Put.Mason qualified as BR.Mason
import Binrep.Put.Bytezap qualified as BR.Bytezap
import Binrep.Get.Flatparse qualified as BR.Flatparse

-- | Identity newtype for using with @DerivingVia@.
newtype Binreply a = Binreply { unBinreply :: a }
    deriving stock Show
    deriving (BR.IsCBLen, BR.Simple.BLen, BR.Mason.Put, BR.Bytezap.Put, BR.Flatparse.Get) via a
