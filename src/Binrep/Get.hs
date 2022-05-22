module Binrep.Get
  ( Getter, Get(..), runGet, runGetter
  , GetWith(..), runGetWith
  ) where

import FlatParse.Basic qualified as FP
import FlatParse.Basic ( Parser )
import Data.ByteString qualified as B
import Data.Word
import Data.Int
import GHC.Exts

type Getter a = Parser String a

class Get a where
    -- | Parse from binary.
    get :: Getter a

runGet :: Get a => B.ByteString -> Either String (a, B.ByteString)
runGet = runGetter get

runGetter :: Getter a -> B.ByteString -> Either String (a, B.ByteString)
runGetter g bs = case FP.runParser g bs of
                   FP.OK a bs' -> Right (a, bs')
                   FP.Fail     -> Left "TODO fail"
                   FP.Err e    -> Left e

-- | Parse heterogeneous lists in order. No length indicator, so either fails or
--   succeeds by reaching EOF. Probably not what you usually want, but sometimes
--   used at the "top" of binary formats.
instance Get a => Get [a] where
    get = go []
      where
        go as = do
            a <- get
            FP.isEof >>= \case
              True  -> return $ reverse $ a : as
              False -> go $ a : as

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
    getWith :: r -> Getter a
    default getWith :: Get a => r -> Getter a
    getWith _ = get

deriving anyclass instance Get a => GetWith r [a]

runGetWith :: GetWith r a => r -> B.ByteString -> Either String (a, B.ByteString)
runGetWith r bs = runGetter (getWith r) bs
