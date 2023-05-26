module Senserial.Sequential.Parse.Internal.Parser where

import Data.Kind ( type Type, type Constraint )
import Numeric.Natural ( Natural )

{- | Sequential parsers.

A type may be used as a sequential parser provided it is a 'Monad' and has a
class for parsing from compatible types.

This is an sort of "enumeration" type class, which enables selecting a class
to use in a generic instance @S1@ base case.
-}
class Monad prs => SeqParser prs where
    -- | Parser type class.
    type SeqParserC prs :: Type -> Constraint

    {- | Parse using the selected parser's type class.

    The arguments are metadata regarding the parsing type @a@. In order, they
    are

      * datatype name
      * constructor name
      * field name (if present)
      * field index

    This should be defined using the appropriate function in the parser type
    class.
    -}
    seqParse :: SeqParserC prs a => String -> String -> Maybe String -> Natural -> prs a
