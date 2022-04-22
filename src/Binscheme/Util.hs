module Binscheme.Util where

import Data.Text qualified as Text
import Data.Text ( Text )

tshow :: Show a => a -> Text
tshow = Text.pack . show
