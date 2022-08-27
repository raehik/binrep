module Haskpatch.Format.Bps where

import Binrep.Type.Magic
import Binrep.Type.Sized
import Binrep.Type.Varint
import Binrep.Type.Common
import Strongweak

import Data.ByteString qualified as B

-- | TODO
--   * can't do generic because BPS doesn't store command list length, instead
--     requiring a dynamic check on every command
--     * wonder if this is better or more efficient that using a 'BpsVarint' for
--       the length, same as metadata, or storing the end size as a 'BpsVarint'.
--   * maybe two diff types of varint, +ve and -ve. unclear from spec
--   * perhaps store the varint type(s) as a type var, to allow switching
--     between efficient machine ints and safe 'Integer', 'Natural'!
data Bps (s :: Strength) i a = Bps
  { bpsMagic :: SW s (Magic "BPS1")
  , bpsSourceSize :: SW s (BpsVarint i)
  , bpsTargetSize :: SW s (BpsVarint i)

  , bpsMetadata :: BpsMeta a
  -- ^ Optional metadata. According to the specification, this should
  --   "officially" be XML version 1.0 encoding UTF-8 data, but anything goes.

  , bpsCommands :: [BpsCommand]
  , bpsFooter :: BpsFooter s
  }

type BpsVarint = Varnat 'Bijective 'OffContinues 'LE

data BpsMeta a

data BpsCommand
  = BpsCommandSourceRead
  | BpsCommandTargetRead
  | BpsCommandSourceCopy
  | BpsCommandTargetCopy

data BpsFooter (s :: Strength) = BpsFooter
  { bpsFooterSourceChecksum :: SW s (Sized 4 B.ByteString)
  , bpsFooterTargetChecksum :: SW s (Sized 4 B.ByteString)
  , bpsFooterPatchChecksum  :: SW s (Sized 4 B.ByteString)
  }
