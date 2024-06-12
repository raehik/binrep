module Binrep.Example.Sum where

import Binrep
import Data.Word
import FlatParse.Basic qualified as FP
import GHC.Generics ( type Generic )
import Generic.Data.FOnCstr

data SumType = SumType1 Word8 | SumType2 Word8 Word8
    deriving stock (Generic, Show)

instance Get SumType where
    get = do
        get @Word8 >>= \case
          1 -> genericFOnCstr @Get @"SumType1"
          2 -> genericFOnCstr @Get @"SumType2"
          _ -> error "TODO"
