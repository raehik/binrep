module Binrep.Via where

import Binrep.CBLen
import Binrep.BLen.Simple
import Binrep.Put.Bytezap qualified as BZ
import Binrep.Get.Flatparse qualified as FP

newtype Binreply a = Binreply { unBinreply :: a }
    deriving stock Show
    deriving (IsCBLen, BLen, BZ.Put, FP.Get) via a
