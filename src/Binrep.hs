module Binrep
  ( module Binrep.CBLen
  , module Binrep.BLen.Simple
  , module Binrep.Put.Bytezap
  , module Binrep.Get.Flatparse
  ) where

import Binrep.CBLen
import Binrep.BLen.Simple
import Binrep.Put.Bytezap
import Binrep.Get.Flatparse

{- TODO
  * binrep is its own ecosystem where explicitness and correctness wins over
    all. There are no binrep instances for 'Data.Void.Void' or 'GHC.Generics.V1'
    because these can't be binrepped; rather than providing an absurd, possibly
    convenient instance, we emit a type error for their attempted use.
-}
