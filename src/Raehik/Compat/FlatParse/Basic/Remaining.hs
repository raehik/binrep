{-# LANGUAGE PatternSynonyms #-}

module Raehik.Compat.FlatParse.Basic.Remaining where

import FlatParse.Basic.Parser ( ParserT(ParserT), pattern OK# )
import GHC.Exts ( minusAddr#, Int(I#) )

-- | Get the remaining length. May return 0.
remaining :: ParserT st e Int
remaining = ParserT $ \_fp eob s st -> OK# st (I# (minusAddr# eob s)) s
{-# inline remaining #-}
