{-# LANGUAGE OverloadedStrings #-} -- for easy error building

-- | Common parser error definitions.

module Binrep.Get.Error where

import Data.Text.Builder.Linear qualified as TBL
import Data.Text qualified as Text
import Numeric.Natural ( Natural )

-- | Top-level parse error.
--
-- The final element is the concrete error. Prior elements should "contain" the
-- error (i.e. be the larger part that the error occurred in).
--
-- Really should be non-empty-- but by using List, we can use the empty list for
-- Fail. Bit of a cute cheat.
type ParseError pos text = [ParseErrorSingle pos text]

-- | A single indexed parse error.
data ParseErrorSingle pos text = ParseErrorSingle
  { parseErrorSinglePos  :: pos
  , parseErrorSingleText :: [text]
  } deriving stock Show

-- | Map over the @pos@ index type of a 'ParseErrorSingle'.
mapParseErrorSinglePos
    :: (pos1 -> pos2)
    -> ParseErrorSingle pos1 text
    -> ParseErrorSingle pos2 text
mapParseErrorSinglePos f (ParseErrorSingle pos text) =
    ParseErrorSingle (f pos) text

-- | Shorthand for one parse error.
parseError1 :: [text] -> pos -> ParseError pos text
parseError1 texts pos = [ParseErrorSingle pos texts]

-- | Construct a parse error message for a generic field failure.
parseErrorTextGenericFieldBld
    :: String -> String -> Maybe String -> Natural
    -> [TBL.Builder]
parseErrorTextGenericFieldBld dtName cstrName (Just fieldName) _fieldIdx =
  [    "in " <> TBL.fromText (Text.pack dtName)
    <>   "." <> TBL.fromText (Text.pack cstrName)
    <>   "." <> TBL.fromText (Text.pack fieldName) ]
parseErrorTextGenericFieldBld dtName cstrName Nothing           fieldIdx =
  [    "in " <> TBL.fromText (Text.pack dtName)
    <>   "." <> TBL.fromText (Text.pack cstrName)
    <>   "." <> TBL.fromUnboundedDec fieldIdx ]

-- | Construct a parse error message for a generic sum tag no-match.
parseErrorTextGenericNoCstrMatchBld :: String -> [TBL.Builder]
parseErrorTextGenericNoCstrMatchBld dtName =
  [    "sum tag did not match any constructors in "
    <> TBL.fromText (Text.pack dtName) ]

-- | Construct a parse error message for a generic sum tag parse error.
parseErrorTextGenericSumTagBld :: String -> [TBL.Builder]
parseErrorTextGenericSumTagBld dtName =
  [    "while parsing sum tag in " <> TBL.fromText (Text.pack dtName) ]
