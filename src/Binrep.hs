{- | Top-level binrep module, exporting all classes, generics & runners.

binrep helps you precisely model binary schemas by combining simple "building
blocks" (e.g. @'Binrep.Type.NullTerminated.NullTerminated' a@) in regular
Haskell types. You can then receive high-performance serializers and parsers for
free via generics.

binrep is /not/ a general-purpose parsing/serializing library. For that, see

  * mason, for fast and flexible serializing
  * flatparse, for extremely performant parsing
  * bytezap, for overly-fast serializing and parsing (but very limited)
-}

module Binrep
  (
  -- * Class and instance design
  -- $class-and-instance-design

  -- * Struct parsing & serializing
  -- $struct-handlers
    module Binrep.BLen
  , module Binrep.CBLen
  , module Binrep.Put
  , module Binrep.Put.Struct
  , module Binrep.Get
  , module Binrep.Get.Struct
  ) where

import Binrep.BLen
import Binrep.CBLen
import Binrep.Put
import Binrep.Put.Struct
import Binrep.Get
import Binrep.Get.Struct

{- $class-and-instance-design
At the core of binrep are a set of classes defining parsers, serializers, and
serialized length checkers on supported types. binrep is its own ecosystem where
explicitness and correctness win over all:

  * there are no binrep instances for 'Data.Void.Void' or 'GHC.Generics.V1'
    because we can't use them; rather than providing an absurd, possibly
    convenient instance, we emit a type error for their attempted use.
  * you can't put/get 'Data.Word.Word32's etc by themselves; you must provide
    endianness information via the 'Binrep.Util.ByteOrder.ByteOrdered' newtype
  * @'Get' 'Data.ByteString.ByteString'@ just consumes the whole input. seem
    weird? it works with the combinators (it's actually rather important)

Here are some important design decisions:

  * Fields in product types are concatenated left-to-right. e.g. @'Put' (l, r)@
    first puts @l@, then @r@. Nothing is placed between them.
  * Sum types must define how to handle the constructor sum.
    Generics are split into sum handlers and non-sum handlers.
    binrep instances are not provided for types such as @'Either' a b@, where we
    can't state how to choose between the 'Left' and 'Right' constructors.
  * @'Refined.Refined' (pl `Refined.And` pr) a@ is re-associated to
    @'Refined.Refined' pr ('Refined.Refined' pl a)@. The single layer of
    refinements is ergonomic, but the way binrep instances work means we need
    the latter. So 'Refined.And' instances essentially rewrite themselves to
    work as if it were a stack of refinements. (See
    'Binrep.Type.Derived.NullTermPadded' for an example.)
-}

{- $struct-handlers

There are experimental "struct" handlers, which only work on data types that
look like C structs. That is,

  * every field must be constant length, and
  * no sums allowed.

The underlying runners for these are even faster-- they shouldn't do much more
work than the code a C compiler would generate for a similar @struct@. But they
are very inflexible (few binrep instances, hard to write by hand) and poorly
tested. Please be warned when using them. (And do consider sending bug reports
to the author!)
-}
