Title: Announcement: unwitch
Date: 2026-03-17 18:20
Category: announcement
OPTIONS: toc:nil
Tags: programming, haskell, library

Hello 👋
Announcing [unwitch](https://hackage.haskell.org/package/unwitch).
A primitive conversion library with better safety and error messages.
It can do safety on unboxed types as well.
The idea is inspired by [witch](https://hackage.haskell.org/package/witch)
but removes the type class magic from it, instead it uses functions.
So it's like witchcraft without the magic! ✨

This is an example on how to use the library:
```haskell
import qualified Unwitch.Convert.Double as Double
import qualified Unwitch.Convert.Int32 as Int32

spec = do
  Int32.toInt64 4 `shouldBe` 4
  Int32.toInt16 4 `shouldBe` Just 4

  Double.toInteger 5.0 `shouldBe` Right 5
  Double.toInteger 5.6 `shouldBe` Left $ RationalConversion $ DenomNotOne (5 % 6)
```

The type signatures indicate if a conversion is possible
or needs checking, `Int32.toInt16` may not fit for example.


I've been wanting to make this for years at this point,
because we experience production bugs around simple primitive conversions.
The problem is that writing a conversion library isn't particularly fun.
Thanks to some recent innovations, all I had to do was complain to [other programs](https://jappie.me/haskell-vibes.html)
to write this.
Which made all the difference in the world.
