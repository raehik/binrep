{-
  * The 'FileTableHeader' is an internal data type. It only gets used inside
    parsing and serializing, the user never actually sees it. It would be a
    target for optimization, i.e. removing it and working by jumping around with
    addresses. It's obviously nice for clarity, though. It would be nice to
    write examples for both.
-}

module Binrep.Example.FileTable where

import Binrep
import Refined
import Binrep.Type.Common ( Endianness(..) )
import Binrep.Type.Int
import Binrep.Type.LenPfx
import Binrep.Type.ByteString
import FlatParse.Basic qualified as FP
import Data.ByteString qualified as B
--import Strongweak
--import Data.Map ( Map )
import GHC.Generics ( Generic )
import GHC.Exts

type BS = B.ByteString
type W32 = I 'U 'I4 'LE

{-
type FileTableW = Map Text BS
type FileTableS = [(BS, BS)]

type FileTableHeader s a = SW s (LenPfx 'I4 'LE (FileTableHeaderEntry a))
data FileTableHeaderEntry s a = FileTableHeaderEntry
  { fileTableHeaderEntryName   :: a
  , fileTableHeaderEntryOffset :: SW s W32
  , fileTableHeaderEntryLength :: SW s W32
  }

extractFiles :: BS -> FileTableHeader 'Strong BS -> FileTableS
extractFiles :: BS -> FileTableHeader 'Strong BS -> FileTableS
-}

getFileTable :: Getter (LenPfx 'I1 'LE Entry)
getFileTable = FP.withAddr# $ \addr# -> getLenPfx (getEntry addr#)

getEntry :: Addr# -> Getter Entry
getEntry addr# = do
    name   <- unrefine <$> get @(AsByteString 'C)
    dat    <- FP.withAnyWord8# $ \offset# -> FP.withAnyWord8# $ \len# ->
        FP.lenAtOffset# addr# (w8i# offset#) (w8i# len#)
    return $ Entry (name, dat)

w8i# :: Word8# -> Int#
w8i# w# = word2Int# (word8ToWord# w#)

exBs :: BS
exBs = B.pack
  [ 0x01
  , 0x30, 0x31, 0x32, 0x00
  , 0x07 -- <- offset!!
  , 0x01
  , 0xFF
  ]

newtype Entry = Entry { unEntry :: (BS, BS) } deriving stock (Generic, Show, Eq)
