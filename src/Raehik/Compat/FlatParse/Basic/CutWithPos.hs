module Raehik.Compat.FlatParse.Basic.CutWithPos where

import FlatParse.Basic ( ParserT, Pos, getPos, cut, err )

-- | Convert a parsing failure to an error, which also receives the parser
--   position (as a 'Pos', from the end of input).
cut' :: ParserT st e a -> (Pos -> e) -> ParserT st e a
cut' p e = getPos >>= \pos -> cut p (e pos)
{-# inline cut' #-}

-- | Throw a parsing error, which also receives the parser position (as a 'Pos',
--   from the end of input).
err' :: (Pos -> e) -> ParserT st e a
err' e = getPos >>= \pos -> err (e pos)
{-# inline err' #-}
