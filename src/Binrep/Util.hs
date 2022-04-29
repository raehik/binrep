module Binrep.Util where

import Data.Text qualified as Text
import Data.Text ( Text )

import GHC.Exts
import GHC.Num.Natural

tshow :: Show a => a -> Text
tshow = Text.pack . show

-- | Convert some 'Int' @i@ where @i >= 0@ to a 'Natural'.
--
-- This is intended for wrapping the output of 'length' functions.
--
-- underflows if you call it with a negative 'Int' :)
naturalFromPosInt :: Int -> Natural
naturalFromPosInt (I# i#) = NS (int2Word# i#)
