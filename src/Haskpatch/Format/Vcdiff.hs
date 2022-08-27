-- | https://datatracker.ietf.org/doc/html/rfc3284

module Haskpatch.Format.Vcdiff where

import Binrep.Type.Magic
import Binrep.Type.Varint
import Binrep.Type.Common
import Strongweak

import Numeric.Natural
import Data.ByteString ( ByteString )
import Data.Word ( Word8 )

data Vcdiff (s :: Strength) = Vcdiff
  { vcdiffHeader :: Header s
  }

data Header (s :: Strength) = Header
  { headerMagic :: SW s (Magic '[0xD6, 0xC3, 0xC4, 0x00])
  -- ^ First 3 bytes are @VCD@ each with their MSB on.

  , headerIndicator :: SW s (Magic '[0x00])
  -- ^ TODO annoying and impacts rest of format. forcing to 0x00 to simplify
  }

data Window (s :: Strength) = Window
  { windowIndicator :: SW s (Magic '[0x00]) -- TODO
  , windowDelta :: Delta s
  }

data Delta (s :: Strength) = Delta
  { deltaIndicator :: SW s (Magic '[0x00]) -- TODO compression indicators. ignoring
  , deltaAddRun :: ByteString
  , deltaInstrs :: [InstrCode]
  , deltaCopy   :: ByteString
  }

data InstrCode = InstrCode
  { instrCodeTriple1 :: InstrTriple
  , instrCodeTriple2 :: InstrTriple
  }

data InstrTriple = InstrTriple
  { instrTripleInstr :: Instr

  , instrTripleSize  :: Word8

  , instrTripleMode  :: Word8
  -- ^ 0 and meaningless unless instr is a COPY
  }

-- TODO singletons it
data Instr = Instr0Noop | Instr1Add | Instr2Run | Instr3Copy

-- | Apparently from the Sfio library, also similar (but not identical) to BPS's
--   varints.
type VcdiffVarint = Varnat 'Redundant 'OnContinues 'BE Natural
