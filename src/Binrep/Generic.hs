module Binrep.Generic where

import Binrep.Type.NullTerminated
import Data.ByteString qualified as B
import Data.Text qualified as Text
import Data.Text.Encoding qualified as Text
import Rerefined.Refine ( unsafeRefine )

-- | Turn a constructor name into a prefix tag by adding a null terminator.
--
-- Not common in binary data representations, but safe and useful for debugging.
nullTermCstrPfxTag :: String -> NullTerminated B.ByteString
nullTermCstrPfxTag = unsafeRefine . Text.encodeUtf8 . Text.pack
-- ^ reallyUnsafeRefine : safe assuming Haskell constructor names are UTF-8 with
-- no null bytes allowed
