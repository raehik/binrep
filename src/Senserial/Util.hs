-- | Assorted utilities for senserial.

module Senserial.Util where

import Senserial.Sequential.Parse.Sum ( PfxTagCfg(..) )

import Numeric ( readHex )
import Data.Text qualified as Text

-- | Construct a prefix tag config using existing 'Eq' and 'Show' instances.
--
-- The user only needs to provide the constructor name parser.
eqShowPfxTagCfg :: (Eq a, Show a) => (String -> a) -> PfxTagCfg a
eqShowPfxTagCfg f = PfxTagCfg
    { pfxTagCfgFromCstr = f
    , pfxTagCfgEq = (==)
    , pfxTagCfgShow = Text.pack . show
    }

-- | Turn a constructor name into a prefix tag by applying a function to the
--   constructor name, and reading the result as a hexadecimal number.
--
-- e.g. drop all but final 2 chars, read as single byte
hexCstrPfxTag :: forall a. Integral a => (String -> String) -> String -> a
hexCstrPfxTag f = forceRead . readHex . f
  where
    -- | Successfully parse exactly one result, or runtime error.
    --
    -- TODO I hate 'Read' and this shit.
    forceRead :: [(a, String)] -> a
    forceRead = \case []        -> error "no parse"
                      [(x, "")] -> x
                      [(_x, _)] -> error "incomplete parse"
                      (_:_)     -> error "too many parses (how??)"

