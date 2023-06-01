module Binrep.Get.Error where

-- | Structured parse error.
--
-- Top level.
data ETop e os
  -- | Parse error with offset information.
  = ETop os (e (ETop e os))

  -- | Empty parse error.
  | EEmpty

-- | Middle level: recursion, structure.
data EMiddle e
  -- | Somehow, we got two parse errors.
  --
  -- I have a feeling that seeing this indicates a problem in your code. We can
  -- only handle "multiple" parse errors when we're prepared to e.g. wrapping an
  -- error with generic info.
  = EAnd e e

  -- | Parse error decorated with generic info.
  --
  -- Should not be set except by library code.
  | EGeneric String {- ^ data type name -} (EGeneric e)

  | EBottom EBase

    deriving stock (Eq, Show, Generic)

-- | Bottom level, plain sum type.
data EBase
  = EExpectedByte Word8 Word8
  -- ^ expected first, got second

  | EOverlong Int Int
  -- ^ expected first, got second

  | EExpected B.ByteString B.ByteString
  -- ^ expected first, got second

  | EFailNamed String
  -- ^ known fail

  | EFailParse String B.ByteString Word8
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

{-

eBase :: EBase -> Getter a
eBase = FP.err . EBase 0

-- TODO rename. getWrapBase?
getEBase :: Get a => EBase -> Getter a
getEBase = getWrapBase' get

getWrapBase' :: Getter a -> EBase -> Getter a
getWrapBase' (FP.ParserT f) eb =
    FP.ParserT \fp eob s st ->
        let os = I# (minusAddr# eob s)
         in case f fp eob s st of
              FP.Fail# st'   -> FP.Err# st' (EBase os eb)
              FP.Err#  st' e -> FP.Err# st' (EAnd os e eb)
              x -> x

--getEBase' :: Get a => EBase -> Getter a
--getEBase' = FP.cut get . EBase 0

-- | Parse. On parse error, coat it in a generic context layer.
getWrapGeneric :: Get a => String -> (E -> EGeneric E) -> Getter a
getWrapGeneric = getWrapGeneric' get

getWrapGeneric' :: Getter a -> String -> (E -> EGeneric E) -> Getter a
getWrapGeneric' (FP.ParserT f) cd fe =
    FP.ParserT \fp eob s st ->
        let os = I# (minusAddr# eob s)
         in case f fp eob s st of
              FP.Fail# st'   -> FP.Err# st' (EGeneric os cd $ fe EFail)
              FP.Err#  st' e -> FP.Err# st' (EGeneric os cd $ fe e)
              x -> x

{-
-- | Parse. On parse error, coat it in a generic context layer.
getWrapGeneric'
    :: Get a
    => String {- ^ data type name -}
    -> (E -> EGeneric E)
    -> Getter a
getWrapGeneric' cd f =
    FP.cutting get (EGeneric 0 cd $ f EFail) (\e _ -> EGeneric 0 cd $ f e)
-}

--Getter a -> Getter 

--offset :: FP.ParserT st e Int
--offset = ParserT \fp eob s st -> 


cutEBase :: Getter a -> EBase -> Getter a
cutEBase f e = FP.cut f $ EBase 0 e

-}
