# To-dos
## Provide "deriving via" generic derivers
Seems to be all the craze lately. See generic-random:
https://hackage.haskell.org/package/generic-random-1.5.0.1/docs/Generic-Random-DerivingVia.html

This would let me do something rather interesting -- instead of use newtypes all
over and forcing the user to endlessly wrap and unwrap, I could perhaps let them
use the regular old type, but derive a specialized instance through a newtype.

Doesn't work with `WithRefine`, since the user needs to handle those themselves
(that's the point). But neat idea.
