-- | Error data type definitions (shared between parsers).

module Binrep.Get.Error where

import GHC.Generics ( Generic )
import Data.Text ( Text )
import Numeric.Natural ( Natural )
import Data.Word ( Word8 )
import Data.ByteString ( ByteString )

-- | Structured parse error.
data E
  = E Int EMiddle

  -- | Unhandled parse error.
  --
  -- You get this if you don't change a flatparse fail to an error.
  --
  -- Should not be set except by library code.
  | EFail

    deriving stock (Eq, Show, Generic)

data EMiddle

  -- | Parse error with no further context.
  = EBase EBase

  -- | Somehow, we got two parse errors.
  --
  -- I have a feeling that seeing this indicates a problem in your code.
  | EAnd E EBase

  -- | Parse error decorated with generic info.
  --
  -- Should not be set except by library code.
  | EGeneric String {- ^ data type name -} (EGeneric E)

    deriving stock (Eq, Show, Generic)

data EBase
  = EExpectedByte Word8 Word8
  -- ^ expected first, got second

  | EOverlong Int Int
  -- ^ expected first, got second

  | EExpected ByteString ByteString
  -- ^ expected first, got second

  | EFailNamed String
  -- ^ known fail

  | EFailParse String ByteString Word8
  -- ^ parse fail (where you parse a larger object, then a smaller one in it)

  | ERanOut Int
  -- ^ ran out of input, needed precisely @n@ bytes for this part (n > 0)
  --
  -- Actually a 'Natural', but we use 'Int' because that's what flatparse uses
  -- internally.

    deriving stock (Eq, Show, Generic)

-- | A generic context layer for a parse error of type @e@.
--
-- Recursive: parse errors occurring in fields are wrapped up here. (Those
-- errors may also have a generic context layer.)
--
-- Making this explicitly recursive may seem strange, but it clarifies that this
-- data type is to be seen as a layer over a top-level type.
data EGeneric e
  -- | Parse error relating to sum types (constructors).
  = EGenericSum (EGenericSum e)

  -- | Parse error in a constructor field.
  | EGenericField
        String          -- ^ constructor name
        (Maybe String)  -- ^ field record name (if present)
        Natural         -- ^ field index in constructor
        e               -- ^ field parse error
    deriving stock (Eq, Show, Generic)

data EGenericSum e
  -- | Parse error parsing prefix tag.
  = EGenericSumTag e

  -- | Unable to match a constructor to the parsed prefix tag.
  | EGenericSumTagNoMatch
        [String] -- ^ constructors tested
        Text     -- ^ prettified prefix tag
    deriving stock (Eq, Show, Generic)
