TITLE: The strings in haskell
DATE: 2017-10-02
CATEGORY: reflection
Tags: haskell, programming 
OPTIONS: toc:nil
Status: draft

Strings in haskell are confusing.
There are simply a lot more string types then you'd expect.
most programming languages will have a single string type,
and that's what you use to display text.
For example python has a `str`,
C has a `char*`, and Java has `String`.

In Haskell we have:

+ [`type String = [Char]`](https://hackage.haskell.org/package/base-4.15.0.0/docs/Data-String.html#t:String)
+ [Data.Text](https://hackage.haskell.org/package/text-1.2.4.1/docs/Data-Text.html)
+ [Data.Text.Lazy](https://hackage.haskell.org/package/text-1.2.4.1/docs/Data-Text-Lazy.html)
+ [Data.ByteString](https://hackage.haskell.org/package/bytestring-0.11.1.0/docs/Data-ByteString.html)
+ [Data.ByteString.Lazy](https://hackage.haskell.org/package/bytestring-0.11.1.0/docs/Data-ByteString-Lazy.html#t:ByteString)
+ [Data.Vector.Unboxed Char](https://hackage.haskell.org/package/vector-0.12.3.0/docs/Data-Vector-Unboxed.html)
+ [Symbol](https://hackage.haskell.org/package/base-4.15.0.0/docs/GHC-TypeLits.html#t:Symbol)

what the fuck?
Seriously what the fuck happened?
Why are there so many?
And which one do I choose?

I've been using the following descision process up till now:

1. If some underlying library requires a specific String, such as `String` or `Bytestring`, choose that.
2. Default to Data.Text, except for the case where you deal with binary data (eg no text), then choose bytestring.

In here I pretty much always ignore the lazy variants.
Furthermore, I don't even really think about unboxing.
This advice aligns with anwsers to a stack [overflow post](https://stackoverflow.com/questions/20691463/data-text-vs-data-vector-unboxed-char).
However i"d like to point out, that the arguments
used for advocating `Data.Text` and `Data.ByteString` don't contain
any references, and are mostly done trough appeals of authority.
For example J. Abrahamson says:

> You absolutely should use `Text` if you're dealing with a human language string. You should also be really careful to treat it with care since human language strings are not easily amenable to computer processing. If your string is better thought of as a machine string, you probably should use `ByteString`.

There is no real explanation of *why* to use text.
It's just an appeal to authority: "you really should use text or bytesting".
And I'm not saying he's wrong, I just want to know why.
However in an earlier paragraph he hints to why Text is so much better then string:

> To clarify this distinction take note that there's no guarantee that `unpack . pack` witnesses an isomorphism, that the preferred ways of converting from `Text` to `ByteString` are in `Data.Text.Encoding` and are partial, and that there's a whole sophisticated plug-in module [`text-icu`](https://hackage.haskell.org/package/text-icu) littered with complex ways of handling human language strings.


So to pull this apart, the isomorphism comment says that there maybe data loss
if you convert from a string to a text and back (or a Text and bytestring, it's ambiguous).
He adds fuel to the fire by saying that even if you'd the 'recommended' functions
in `Data.Text.Encoding`, they are partial.
Partiality means that they either crash or return some error on doing this conversion.
And if you need better support use this other package (which binds to icu, a c library).
In other words, its complex to convert between all strings,
and it would've been easy with an isomorphism but there is no such thing.
