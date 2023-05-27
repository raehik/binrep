# senserial
senserial is a small library providing reusable generics for parsers and
serializers. No need to muddle through boilerplate generics that look the same
as everyone else's; just provide a few definitions and senserial can give you
powerful generic instances.

## Notes
### Orphan instances
This library is designed to work with and around existing libraries and type
classes. Unless you add a senserial instance to your parser/serializer
definition site, you will be dealing with orphan instances. That's life, Jim.

## License
Provided under the MIT license. See `LICENSE` for license text.
