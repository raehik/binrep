module Binrep.Get
  ( Get(..), runGet
  , GetWith(..), runGetWith
  ) where

import Data.ByteString qualified as B
import Data.Serialize.Get qualified as Cereal

import Data.Word
import Data.Int

class Get a where
    -- | Parse from binary.
    get :: Cereal.Get a

-- | Run the parser.
--
-- If parsing succeeds, the remaining unconsumed 'B.ByteString' (potentially
-- 'B.empty') is returned.
runGet :: Get a => B.ByteString -> Either String (a, B.ByteString)
runGet = runGetCereal get

-- | Parse heterogeneous lists in order. No length indicator, so either fails or
--   succeeds by reaching EOF. Probably not what you usually want, but sometimes
--   used at the "top" of binary formats.
instance Get a => Get [a] where
    get = go []
      where
        go as = do
            a <- get
            Cereal.isEmpty >>= \case
              True -> return $ reverse $ a : as
              False -> go $ a : as

instance Get B.ByteString where
    -- This is inefficient (does a ton of work checking things that don't
    -- matter), but safe and in the library.
    get = Cereal.getBytes =<< Cereal.remaining
    -- I have an alternative that just appends buffer and returns -- still does
    -- work, just very little. But it might not be correct, and I'm still
    -- discussing whether they can add it.
    --get = Cereal.getByteStringEOF

instance Get Word8 where get = Cereal.getWord8
instance Get  Int8 where get = Cereal.getInt8

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
    getWith :: r -> Cereal.Get a
    default getWith :: Get a => r -> Cereal.Get a
    getWith = const get

deriving anyclass instance Get a => GetWith r [a]

-- | Run the parser with the given environment.
runGetWith :: GetWith r a => r -> B.ByteString -> Either String (a, B.ByteString)
runGetWith = runGetCereal . getWith

--------------------------------------------------------------------------------

-- | Proper 'Cereal.Get' runner, because the library doesn't come with one.
runGetCereal :: Cereal.Get a -> B.ByteString -> Either String (a, B.ByteString)
runGetCereal f = handleCerealResult . Cereal.runGetPartial f

-- | Helper for unwrapping 'Cereal.Get.Result's.
handleCerealResult :: Cereal.Result a -> Either String (a, B.ByteString)
handleCerealResult = \case
  Cereal.Done a bs'  -> Right (a, bs')
  Cereal.Fail e _bs' -> Left $ "cereal error: "<>e
  Cereal.Partial f   -> handleCerealResult $ f B.empty -- cereal moves to Fail
