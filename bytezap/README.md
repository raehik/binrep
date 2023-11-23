# bytezap
bytezap is a compact, maximal-performance binary serialization library for
Haskell.

Serializers are pointer operations. That's it. They may be sequenced with `<>`.

Serializers are executed by providing an object to serialize, and the length in
bytes which the the object will serialize to. This allocates a single buffer,
then fills it with the serializer's pointer operation.

Obtaining length in bytes prior to serialization is the job of the end-user. If
you can't seem to calculate this for a given type, bytezap probably doesn't want
to work with it. For example, don't try to serialize JSON or map-like
containers.

***It is extremely easy to misuse bytezap and corrupt memory.*** There is not
much room for improving safety without losing performance.

See [store](https://github.com/mgsloan/store) for a similar library, with very
similar gotchas.

## Why?
Because none of the other serialization libraries out there as of 2023-11-23
provide an easily-reusable base for this stuff, which I want for my binrep
library. store is close but too large, mason is close but slow.

## This seems hard to use.
Yes. I don't even provide a `Write` type that wraps a `Poke` with the
appropriate length. I don't because binrep doesn't use it. I haven't decided if
I want to provide one.
