# To-dos
## Simple
  * properly test the default `AsByteString 'C` sum tag handler (!)

## Generate (human-readable) schema from type
I think I do this by writing yet another typeclass, filling it out for my
primitives, then writing yet another generic typeclass. But I'd like to make
this really powerful. Send an `a_generic_info -> b` function along or something,
so we can provide a ton of different schema formats for free. e.g write to plain
pretty string, write to a JSON doc, write to some description language...

Actually, I think you send a bunch of functions along. So that's why people use
a typeclass instead, for this "no middle man" thing. Should be fun.

## Provide "deriving via" generic derivers
Seems to be all the craze lately. See generic-random:
https://hackage.haskell.org/package/generic-random-1.5.0.1/docs/Generic-Random-DerivingVia.html

This would let me do something rather interesting -- instead of use newtypes all
over and forcing the user to endlessly wrap and unwrap, I could perhaps let them
use the regular old type, but derive a specialized instance through a newtype.

Doesn't work with `WithRefine`, since the user needs to handle those themselves
(that's the point). But neat idea.
