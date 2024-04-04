module Binrep
  ( module Binrep.BLen
  , module Binrep.CBLen
  , module Binrep.Put
  , module Binrep.Put.Struct
  , module Binrep.Get
  ) where

import Binrep.BLen
import Binrep.CBLen
import Binrep.Put
import Binrep.Put.Struct
import Binrep.Get

{- TODO
  * binrep is its own ecosystem where explicitness and correctness wins over
    all. There are no binrep instances for 'Data.Void.Void' or 'GHC.Generics.V1'
    because these can't be binrepped; rather than providing an absurd, possibly
    convenient instance, we emit a type error for their attempted use.
-}
