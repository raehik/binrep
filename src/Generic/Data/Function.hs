{- | Functions "lifted" (roughly) to generic Haskell data types.

Haskell data types have a fair amount of structure to them:

  * Multiple constructors (sums)
  * Multiple fields (products)
  * Constructor names must be unique
  * Fields are ordered left-to-right (or top-to-bottom)

We leverage this structure to provide parameterized generic functions, where the
user only handles the base case (individual fields). Such generics are very
relevant for simplistic usages like boring type folds and serializing tasks. No
need to bash out 50 lines of arcane type algebra -- just write a single instance
and you're golden.

Sum types introduce choice, which brings an extra layer of complexity. For this
reason, most functions provide a sum type version and a non-sum type version.
Sum type generic functions will require a bit more information, like some extra
definitions or instances. Using the wrong one will result in a clear type error.
-}

module Generic.Data.Function where
