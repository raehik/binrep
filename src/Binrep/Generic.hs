module Binrep.Generic where

import Binrep.Type.NullTerminated
import Data.ByteString qualified as B
import Data.Text qualified as Text
import Data.Text.Encoding qualified as Text
import Refined.Unsafe

-- | Turn a constructor name into a prefix tag by adding a null terminator.
--
-- Not common in binary data representations, but safe and useful for debugging.
--
-- The refine force is safe under the assumption that Haskell constructor names
-- are UTF-8 with no null bytes allowed. Fairly certain that's true.
nullTermCstrPfxTag :: String -> NullTerminated B.ByteString
nullTermCstrPfxTag = reallyUnsafeRefine . Text.encodeUtf8 . Text.pack
