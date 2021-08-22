TITLE: Safe conversion in haskell
DATE: 2020-12-24
CATEGORY: reflection
Tags: australia, work
OPTIONS: toc:nil
Status: draft

In haskell we generally like safety from type system.
That is to say, the compiler should give us a type error
if we do something unsafe,
solving the type error is making the program safe.
Surprisingly there are several function in base that aren't
safe.
The best example is [fromIntegral](https://hackage.haskell.org/package/base-4.15.0.0/docs/Prelude.html#v:fromIntegral).
It allows you to make a Int16 into an Int8 with no reprocussions.
The problem here is that `Int16` can represent `2^15` values
(1 bit for a positive/negative sign),
and `Int8` can only represent `2^7`


https://hackage.haskell.org/package/convert
https://hackage.haskell.org/package/witch-0.3.4.0
https://hackage.haskell.org/package/conversion
https://hackage.haskell.org/package/convertible
