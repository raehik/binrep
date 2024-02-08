{- | Kaitai Struct interoperation.

The Kaitai Struct (KS) project covers two main artifacts:

* a complex YAML schema called KSY for describing file formats
* "compilers" which consume schemas and generate parsers (and serializers, as of
  2023!), for various languages, primarily Python and Java

Their goals are much more grounded than binrep. However, a key overlap between
us is the concept of automatically generating code from schemas: a YAML file for
KS, a Haskell type for binrep. Let's think about how we might move between them.

* Automatically parsing a schema into a Haskell type seems backwards since we
  would have to make many static design decisions, where binrep wants the author
  to write as Haskell-y as they can.
* Converting a schema into a function seems handy. But binrep isn't really about
  that, nor does the required tooling exist.
* Converting a binrep type into a KSY document seems plausible, though we'll be
  limited on the syntax we can use.
-}

module Binrep.Schema where

import Data.Text ( Text )
import Data.Void ( Void )

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

type KSYId = Text
data KSYMeta = KSYMeta -- TODO unclear what should be here

data KSY = KSY
  { ksyMeta  :: KSYMeta
  , ksySeq   :: [KSYAttrs]
  , ksyTypes :: Map KSYId KSYType
  }

data KSYAttrs = KSYAttrs
  { ksyAttrId   :: KSYId
  , ksyAttrType :: KSYId
  }

data KSYType = KSYType
  { ksyTypeMeta :: KSYMeta
  , ksyTypeSeq  :: [KSYAttrs]
  }
