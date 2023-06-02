# bytezap to-dos
## Challenges with unbuffered serialization
### Floats to decimal ASCII
Serializing floats to ASCII efficiently is hard. Most of the recent fast
algorithms `malloc` ~25 bytes at the start then tell you how long it actually
was. I see little in the way of "figure out for cheap how many characters long
this float will be once serialized".

mason wins with its builtin grisu3. I think beating that is Hard. bytestring
actually uses ryu internally! But via a Haskell implementation.
[ryu](https://github.com/ulfjack/ryu/blob/master/ryu/d2s.c) has some stuff.
[Dragonbox](https://github.com/jk-jeon/dragonbox) is better, but it's C++. Most
likely I am on my own here. Perhaps I can figure out some shortcuts in grisu3 to
calculate serialized length without doing *all* the work. If I can avoid
`malloc`ing, it would be a winner.

Perhaps I simply pray to SPJ for good caching behaviour. Running the inner
serializer in `blen` *should* mean it can be reused for `put`, and providing
they occur close to each other (which they do in `runPut`), I could see it
happening.

### Ints to decimal ASCII
This should be OK I think? I wonder how fast we can go, though. Cool C++ lib at
[jeaiii/itoa](https://github.com/jeaiii/itoa). Probably a waste of time, use
bytestring's prims along with a fast `BLen` check (I wrote one before I think).
