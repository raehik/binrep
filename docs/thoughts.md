# Assorted thoughts
## Refined & newtypes
Refined is a convenience for when you want to validate some type, but don't want
to store it in a more precise representation. I use it a lot when defining
primitives.

However, I think it comes second to any pre-existing more precise
representations. For example, I originally wrote sized arrays with a refinement
over lists. But such a type already exists in
[vector-sized](https://hackage.haskell.org/package/vector-sized). Swapping out
allowed me to simplify and remove some code.

It is occasionally not inconvenient to encode predicates via type-level
features. This is how `LenPfx` is defined: it stores a sized vector with a
type-level guarantee that the vector's length is well-bounded for the length
prefix type. It could also be represented by a length refinement over a list.
But this way, the compiler has so much information it can essentially write the
code for us. (Check out `LenPfx`'s `Get` instance to see what I mean.)

Note that this complicates using binrep with my `WithRefine` idea. But that's
fine, it's an unfinished convenience idea right now.
