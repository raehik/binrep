module Raehik.Compat.FlatParse.Basic.Prim where

import Raehik.Compat.Data.Primitive.Types
import FlatParse.Basic
import GHC.Exts ( plusAddr# )

anyPrim :: forall a e st. Prim' a => ParserT st e a
anyPrim = withEnsure# size# $ ParserT $ \_fp _eob buf st ->
    OK# st (indexWord8OffAddrAs# buf 0#) (buf `plusAddr#` size#)
  where
    size# = sizeOf# (undefined :: a)
