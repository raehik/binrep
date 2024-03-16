module Raehik.Compat.FlatParse.Basic.WithLength where

import FlatParse.Basic.Parser
import GHC.Exts

-- | Run a parser, and return the result as well as the number of bytes it
--   consumed.
parseWithLength :: ParserT st e a -> ParserT st e (a, Int)
parseWithLength (ParserT f) = ParserT $ \fp eob s st -> do
    case f fp eob s st of
      Fail# st'      -> Fail# st'
      Err#  st' e    -> Err#  st' e
      OK#   st' a s' -> OK#   st' (a, I# (s' `minusAddr#` s)) s'
{-# inline parseWithLength #-}
