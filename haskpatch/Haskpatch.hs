{-# LANGUAGE OverloadedStrings #-}

{- Implementation notes
* Not sure what representation to use for internal state. We need to be
  appending, and inserting elements in the middle arbitrarily (for when we need
  to split a segment). Could use a reverse list, but the splitting gets
  complicated. 'Seq' seems good, but I'm unhappy with the inserting story.
  I think I'm just gonna do the horrible indexing method.

# Devlog
## 2023-08-05T18:21:15+0100 raehik
OK, I'm tired of this now. I have all the concepts down, but the fiddly bits
really are fiddly. I need to write a ton of zipper-y code that splits segments
when we need to only take part of them.

I created some example patch + output target window pairs in my notebook. I
didn't note the "algorithm" I used, which is where I look at the window sizes
and figure out what segments I need. If I walk through that on paper, I should
get closer.

## 2023-08-07T10:53:33+0100
Note for next time: we don't have to *edit* the existing target window. We may
need to split windows, but only to reuse the relevant parts.
-}

module Haskpatch where

import Data.Sequence qualified as Seq
import Data.Sequence ( Seq, (|>) )
import Data.Text ( Text )

{- VCDIFF notes
* s: source
* t: target
* u: s+t end to end
-}

data VCDDeltaInstr i bs b
  = Add i bs -- ^ @i@ must be equal to length of @bs@
  | Copy i i
  | Run i b

{-
-- TODO backwards, because it lets us use a list efficiently.
-- Stateful thing, not useful outside algorithm.
data VCDTargetWindow i bs = VCDTargetWindow
  { twLength :: i
    -- ^ Current window length. Needed for efficient COPY handling for the
    --   reverse list implementation. Should be the same as if you went through
    --   the segments.

  , twSegments :: [VCDTargetWindowSegment i bs]
    -- ^ Current segments in reverse order. The VCDIFF algorithm goes forward,
    --   so if we want to use a list (which seems reasonable), we should build
    --   it in reverse.
  }
-}
type VCDTargetWindow i bs = Seq (VCDTargetWindowSegment i bs)

-- | "Simplified" VCDIFF target window segment, which can only be known stuff or
--   pointing at source window (WITHOUT the s+t indexing stuff).
data VCDTargetWindowSegment i bs
  = Known bs
  | Source
        i {- ^ length. Must not go exceed source window -}
        i {- ^ starting index into source window -}
    deriving Show

--data AbsInstr i bs = AIAdd i bs | AICopy i i i

convert
    :: Num i
    => (i -> b -> bs) -> i -> [VCDDeltaInstr i bs b]
    -> VCDTargetWindow i bs
convert bReplicate sLen = foldr (convertInstr bReplicate sLen) Seq.Empty

-- foldr op :)
-- need to take & return target window because funky COPYs may require us to
-- alter the internal target window representation. (the other two ops just cons
-- on stuff)
convertInstr
    :: Num i => (i -> b -> bs) -> i -> VCDDeltaInstr i bs b
    -> VCDTargetWindow i bs
    -> [VCDTargetWindowSegment i bs]
convertInstr bReplicate sLen instr tw = case instr of
  Add  _bsLen bs -> [Known bs]
  Run   bsLen b  -> [Known (bReplicate bsLen b)]
  Copy  bsLen uIdx -> []
{-
    let xxx =
            if   uIdx - sLen >= 0
            then let tIdx = uIdx - sLen
                 in  twXXX tIdx bsLen tw
            else let sIdx = uIdx - sLen
                 in  []
    in  tw
    -- * figure out u substring that uIdx+bsLen points to
    -- * if it goes past end of t, remainder is repeatLen
-}

{-
twXXX :: i -> i -> VCDTargetWindow i bs -> VCDTargetWindow i bs
twXXX tIdx bsLen tw =
  where
    repeatLen = (twLength tw - tIdx) - bsLen
-}

-- twSegIdx must be valid, thus the target window must be non-empty.
twCopyTInner
    :: i -> Int -> VCDTargetWindow i bs
    -> VCDTargetWindow i bs
twCopyTInner bsLen twSegIdx tw =
  where
    seg = Seq.index tw twSegIdx
    twFromSeg = Seq.drop twSegIdx tw

twCopyTInnerEnd :: i -> VCDTargetWindow i bs -> VCDTargetWindow i bs
twCopyTInnerEnd bsLen = \case
  Seq.Empty -> 

-- segment length must be within given length
segSplit
    :: (bs -> i) -> i -> VCDTargetWindowSegment i bs
    -> (VCDTargetWindowSegment i bs, VCDTargetWindowSegment i bs)
segSplit bsLength upToLen = \case
  Known bs ->
    where
      bsLen = bsLength bs
  Source bsLen sIdx ->

---

exPaperTargetWindow :: [VCDTargetWindowSegment Int Text]
exPaperTargetWindow =
  [ Source 4 0
  , Known "wxyz"
  , Source 4 4
  , Source 4 4
  , Source 4 4
  , Source 4 4
  , Known "zzzz"
  ]

--newtype Target v i a = Target { unTarget :: v (TargetSegment i a) }
data TargetSegment i a
  = TargetSegmentKnown  a
  | TargetSegmentSource i
