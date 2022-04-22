# binscheme
Haskell data types with their explicit binary schema built into the types.
Intended for simple binary file parsing.

The binary and cereal libraries are for passing Haskell data between other
binary and cereal users. Thus, data representation is largely obscured. For
example, in `cereal`, all data is handled in big-endian format. If you use the
`Serialize` typeclass methods for parsing and serializing, you would never know.

binscheme never makes decisions by itself. You can't parse/serialize a `Word64`
without either providing the endianness to use at runtime, or encoding the
endianness into the type.

See the Hackage documentation for details.
