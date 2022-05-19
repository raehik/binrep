module Binrep.Get
  ( Get(..), runGet
  , GetWith(..), runGetWith
  ) where

import FlatParse.Basic qualified as FP
import FlatParse.Basic.Int qualified as FP
import FlatParse.Basic ( Parser )
import Data.ByteString qualified as B
import Data.Word
import Data.Int

class Get a where
    -- | Parse from binary.
    get :: Parser String a

runGet :: Get a => B.ByteString -> Either String (a, B.ByteString)
runGet bs = runFlatParser get bs

runFlatParser :: Parser String a -> B.ByteString -> Either String (a, B.ByteString)
runFlatParser p bs = case FP.runParser p bs of
                       FP.OK a bs' -> Right (a, bs')
                       FP.Fail     -> Left "TODO fail"
                       FP.Err e    -> Left e

-- | Parse heterogeneous lists in order. No length indicator, so either fails or
--   succeeds by reaching EOF. Probably not what you usually want, but sometimes
--   used at the "top" of binary formats.
instance Get a => Get [a] where
    get = error "TODO"

instance (Get a, Get b) => Get (a, b) where
    get = do
        a <- get
        b <- get
        return (a, b)

instance Get B.ByteString where
    get = FP.takeRest

instance Get Word8 where get = FP.anyWord8
instance Get  Int8 where get = FP.anyInt8

-- | Types that can be coded between binary and a Haskell type given some
--   runtime information.
--
-- We can't prove something related to a value and pass that proof along without
-- dependent types, meaning we can't split validation from encoding like in
-- 'BinRep', so encoding can fail. However, by allowing an arbitrary
-- environment, we can define many more convenient instances.
--
-- For example, you can't write a 'BinRep' instance for 'Word16' because it
-- doesn't specify its endianness. But you can define 'BinRepWith Endianness
-- Word16'! This was, you can decide how much of the binary schema you want to
-- place directly in the types, and how much to configure dynamically.
--
-- This class defaults to the free implementation provided by 'BinRep', which
-- ignores the environment and wraps serializing with 'Right'.
class GetWith r a where
    -- | Parse from binary with the given environment.
    getWith :: r -> Parser String a
    default getWith :: Get a => r -> Parser String a
    getWith = const get

deriving anyclass instance Get a => GetWith r [a]

runGetWith :: GetWith r a => r -> B.ByteString -> Either String (a, B.ByteString)
runGetWith r bs = runFlatParser (getWith r) bs
