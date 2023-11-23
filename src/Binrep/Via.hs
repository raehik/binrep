module Binrep.Via where

import Binrep

-- | Identity newtype for using with @DerivingVia@.
newtype Binreply a = Binreply { unBinreply :: a }
    deriving stock Show
    deriving (IsCBLen, BLen, Put, Get) via a
