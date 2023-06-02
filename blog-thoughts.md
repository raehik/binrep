You're a reverse engineer. You stumble across some data that you need to
understand. You quickly figure out that it's serialized in a very simple manner,
probably via a compiler feature that automatically serializes data types. You
write a short, simple parser. Great!

You now want to *write* that same data. Not hard. You write a short, simple
serializer.

You now wish to show the data to an engineer friend. You print out the bytes and
highlight the different fields, and feel very silly. Why can't you just turn it
into some simple format like JSON? Well, you can. You write a JSON serializer.

Your friend wants to edit this data. But she doesn't want to twiddle bytes.
Instead, she edits the JSON and asks that you convert it to the binary format.
You sigh and write a JSON parser, carely checking that values fit into the
binary schema properly.

Look at yourself. You have written the same thing -- a data serializer -- four
separate times! You feel silly. The schema is as simple as it gets, just
concatenated fields. They're even in the same order in your Haskell type.
...Wait a minute.

binrep provides binary *ton* of automated code
