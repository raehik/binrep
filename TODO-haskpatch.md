## Overall idea
The essential concept behind all this is to define a super simple patchscript
AST, operations on it (application, optimization passes), and figure out how to
stuff all existing patch formats into it.

## Unified diff (patch+unpatch)
How? Any exist for binary?? Probably (definitely) danger for copyright.

bytepatch supports this due to its simplicity. If you provide exact matches for
all expected source strings in the patchscript, you can reverse just fine. (The
various extensions like binrepping things will break stuff, but there are ways
to support them too.)

## Patch size analysis
Can I, for any patchscript, read *just* the patchscript and determine

  * the minimum size of the input (`Maybe` for weird formats like VCDIFF), and
  * the precise size of the output?

That would be cool. Gives way to efficient, safe patching.

I've focused on in-place patching so far for bytepatch. For that, minimum input
size is easy. Output size depends on a secret implicit instruction in bytepatch,
which is "copy to the end". With that, the output size is always precisely equal
to the input size. Without it, the exact size is at the end of the final
linearized patch.

## Problem (2022-08-18)
The problem with developing this is... the patch formats out there are just
poorly designed. VCDIFF is nice and popular, but the format is overly complex
and it has a weird thing that needs consideration for converting to the unified
AST. BPS is simple, but poorly documented, not well used, and has an irritating
design choice that makes it slightly painful to parse.

For both of them, I'd probably do better by annotating a patch file and using
that , rather than the specifications.
