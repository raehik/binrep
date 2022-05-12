module Binrep.Schema where

import Data.Text ( Text )
import Data.Void ( Void )

{- 2022-05-03 raehik
I don't really know how to approach this. I can't figure out a useful datatype
that I can then generate a schema from later. So I think I should start with a
basic pretty layout. Then try generating a Kaitai schema.

I need to think about both generic deriving and manual instance writing.
-}

data D a = D
  { dName :: Text
  , dCs   :: Cs a
  }

data Cs a = C0 | C1 C | Cs Text [(a, C)] -- length should be >=2

data C = C
  { cName :: Text
  , cFs :: [F]
  }

data F = F
  { fName :: Maybe Text
  , fType :: Either Text Text -- Right is another D, Left is end descriptive
  }

-- product types can be simpler
data DProd = DProd
  { dpName :: Text
  , dpC    :: Maybe C
  }

--------------------------------------------------------------------------------

data Kaitai = Kaitai
  { kaitaiMeta :: Void
  , kaitaiSeq  :: [KaitaiSeqEl]
  }

data KaitaiSeqEl = KaitaiSeqEl
  { kaitaiSeqElId   :: Text
  , kaitaiSeqElType :: Text
  }
