module Binrep
  ( module Binrep.CBLen
  , module Binrep.BLen.Simple
  , module Binrep.Put
  , module Binrep.Get
  ) where

import Binrep.CBLen
import Binrep.BLen.Simple
import Binrep.Put
import Binrep.Get

{- TODO
  * binrep is its own ecosystem where explicitness and correctness wins over
    all. There are no binrep instances for 'Data.Void.Void' or 'GHC.Generics.V1'
    because these can't be binrepped; rather than providing an absurd, possibly
    convenient instance, we emit a type error for their attempted use.
-}
