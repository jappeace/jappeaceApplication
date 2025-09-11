TITLE: Death💀 to typeclasses
DATE: 2025-09-10 23:00
CATEGORY: technique
Tags: haskell, backpack
OPTIONS: toc:nil

<style>
img[src="/images/2025/death.jpg"]{
  height: 20em;
  width:unset;
}
figure {
  float: right;
  margin: 2em;
  margin-top: 0em;
  width: 15em;
}
figcaption{
  font-size: xx-small;
  color: #999;
}
</style>

<figure>
<img  alt="Death (XIII) tarrot card" src="/images/2025/death.jpg" />
<figcaption> Death (XIII) Symbolizes significant change, transformation, and endings, rather than literal physical death. </figcaption>
</figure>

Have you ever seen a Number grazing in the fields?
Or a Functor chirping in the trees?
No?
That's because they're LIES.
LIES told by the bourgeoisie to keep common folk down.
But I say NO, no longer shall we be kept down by deceit!
Come brothers and sisters, come and let us create a system
of values.
Where values are no longer constrained by their type class,
but instead merged as a signature into a module.
Come comrades, let us open the Backpack.


In here we explore an alternative universe where
we neglect the existence of type classes in favor of the backpack module system.
This ends up looking like [OCaml](https://ocaml.org/) in [Haskell](https://www.haskell.org/?uwu=true).
Let us begin with Functor.

```haskell
signature Death.Functor.Signature
  ( Functor
  , map
  )
where

import Prelude ()

data Functor a
map :: (a -> b) -> Functor a -> Functor b
```

This s a [functor😼](https://hackage.haskell.org/package/base-4.21.0.0/docs/Data-Functor.html#t:Functor), and this is also a [functor🐫](https://ocaml.org/docs/functors).
Functor😼 being the categorical functor where we embed one category into another.
The category in this case being that of set's and functions[^not-a-category],
where types are the sets and functions are the uh, functions.
But it's also an ocaml module functor🐫, where the data keyword introduces a hole
into a signature, which we can later fill in with a proper type.[^ocaml-cat-tangent]

[^ocaml-cat-tangent]: Now is the ocaml module functor🐫 a category functor😼.
    It doesn't look like they actually mix modules with types.
    Although I suspect you can unify these, it's not trivial!
    So it's not the category of sets and functions at least.
    It may be another category, there are many, but I've not defined it yet.
    I think the haskell signatures may also be some kind of category,
    because they can merge, it's a monoid. Looks like the module signature is just 
    a set of type introductions. So the merge is a union of those.
    But seriously this post has exploded in scope. So I let all this meandering as an
    excessive to the reader.

[^not-a-category]: Except [Hask is not a cateogry](https://math.andrej.com/2016/08/06/hask-is-not-a-category/), but it is unless you like splitting hairs

[^not-a-category]: 

We've to hide prelude because the Functor typeclass from base gets imported by default.
We can use signatures, like the one just introduced, 
by simply imorting them as if they were modules.
All a signature does is promise to the compiler we'll make a proper module for that
*later*.
We can just start using our functor right now.
For example, in that same impl package I've an auxiliary module for Functor,
providing some utilities:

```haskell
module Death.Functor
  ( module X
  , (<$>)
  , (<$)
  )
where

import Death.Functor.Signature as X
import Prelude (const)

(<$>) :: (a -> b) -> Functor a -> Functor b
(<$>) = map

(<$) :: b -> Functor a -> Functor b
(<$) b = map (const b)
```

As long as a module implements the signature, you get the stuff
that depends on it implemented "for free".
The code depending on the signature is abstract.
You want to keep your signatures small so more code is abstract,
similar to how you want to keep typeclass definitions small
so you can have smaller instances,
and more code depending on the typeclass is abstract.
Let's make an "instance" of our Functor signature,
for the `Maybe` datatype,
with the power of modules:
```haskell
module Death.Functor.Maybe
  ( Functor
  , map
  )
where

import Prelude(Maybe(..), ($))

type Functor = Maybe

map :: (a -> b) -> Functor a -> Functor b
map fab = \case
  Just x -> Just $ fab x
  Nothing -> Nothing
```

Now it's worth pointing out that we've to do a fair bit of cabal work
to make the compiler realize the instance, so in cabal,
our main library with the signatures looks like this:

```cabal
library
  signatures:
      Death.Functor.Signature
  exposed-modules:
      Death.Functor
  hs-source-dirs:
      src/sig
```
Then our implementing code looks like:
```cabal
library impl
  exposed-modules:
      Death.Functor.Maybe
  hs-source-dirs:
      src/impl
```

If cabal misses a module for a signature it'll give you an error like this:
```
cabal build
Resolving dependencies...
Error:
    Non-library component has unfilled requirements: Death.Functor
    In the stanza 'executable exe'
    In the inplace package 'death-1.0.0'
```
This guarantees all signatures have implementations whenever you build a final
executable.
To solve that error, 
to realize it with "some" implementation.
The client doesn't care what implementation:
```cabal
library app
  exposed-modules:
      Death
  hs-source-dirs:
      src/app
  mixins:
       death (Death.Functor.Signature as Death.Functor.Maybe)
```

What we're saying in the mixin field is that the import `Death.Functor.Signature`,
should in fact be called `Death.Functor.Maybe`.
Which makes it use the Maybe implementation whenever it encounters `Death.Functor.Signature`.
Alternatively, we could've just called it by the same name in the impl package as the signature,
I only discovered this later.
However this renaming allows you to implement multiple module signatures
in the same package, so you can use several functors within the
same module as well, for example:

```haskell
{-# LANGUAGE RebindableSyntax #-}

module Death
  ( main
  )
where

import Maybe.Functor
import Maybe.Applicative
import Maybe.Monad
import qualified List.Functor as LF
import qualified IO.Monad as IO
import Death.BusinessLogic(business)

commonFaith :: Maybe String
commonFaith = Just "no longer constrained by deceit"

marchOfValues :: [Int]
marchOfValues = [4, 3, 2]

main :: IO ()
main = (print @(Functor String) $ do
   cryOfUprising <- (\x -> ("rise up" <> x)) <$> pure "brothers and sisters"
   chorusOfTruth <- commonFaith
   pure $ cryOfUprising <> "  against the lies" <> chorusOfTruth 
                        <> show ((+1) LF.<$> marchOfValues)
   ) IO.>> business

```

We use `RebindableSyntax` to inform GHC to use whatever `>>=` is in scope for `do`.
In this case that's the  `>>=` from Maybe.Monad. 
As long as you satisfy the signature, it's happy. 😼
`do` has nothing to do with Monads.
Who lied to you?

Ah right about that business.
Yes, we can't do commercial code like this, this is disgusting.
We need an effect system.[^effect-system] 
Fortunately, we've a versatile one trick pony.
This is our business code:

[^effect-system]: This is sarcasm. I don't think you need an effect system at all! Here I'm just defining a less awfull one.

```haskell
business :: Functor ()
business = do
  writeLine "file name:"
  systemOfValues <- readLine
  writeLine "file content:"
  truthOfTheFields <- readLine

  writeLine "writing file..."
  writeFile systemOfValues truthOfTheFields

  writeLine "reading it again to make sure its iso 42038 compliant"
  uprisingAgainstDeceit <- readFile systemOfValues

  writeLine uprisingAgainstDeceit
```
At this point we don't know what Functor is,
we want it to be IO in our realized impementation,
but for testing we can set it to a state monad for example so we can
make sure it does everything correct in memory.[^good-idea] 
Without having to rely on these unreliable filesystems for example.

[^good-idea]: I'm not sure if this is actually a good idea, seems like a lot of boilerplate for a marginal test speedup. But this is the only use case I can imagine for effect systems. I suppose you could theoretically buy libraries from copmeting vendors for signatures. But this never materialized as a business model (C already had this chance with header files for example!)

Working backwards from our business logic implementation,
We need to define some signatures to support our business logic:
```haskell
signature Death.Effects.FileSystem
  ( readFile
  , writeFile
  )
where

import Prelude(String, FilePath)
import Death.Functor.Signature

readFile  :: FilePath -> Functor String
writeFile :: FilePath -> String -> Functor ()
```

Actually I now realize we could've just renamed prelude on top
of our FileSystem effect to get the realized implementation.
Instead, I made a separate module:
```haskell
module Death.Effects.FileSystem
  ( readFile
  , writeFile
  )
where

import Prelude(IO, readFile, writeFile)
```

When you stare at hammers long enough everything becomes a nail!
Actually I think the state implementation is more interesting:

```haskell
module Death.Effects.FileSystem
  ( readFile
  , writeFile
  )
where

import Prelude(String, FilePath, ($))
import Death.Functor.Signature
import Data.Map qualified as Map
import Data.Maybe(fromMaybe)
import Death.Functor.State

readFile  :: FilePath -> Functor String
readFile path = Functor $ 
    \state -> (state, fromMaybe "" $ Map.lookup path (fileSystem  state))

writeFile :: FilePath -> String -> Functor ()
writeFile path contents = Functor $ 
    \state -> (state {fileSystem = Map.insert path contents (fileSystem state)}, ())
```
It's interesting in that it's boring.
For people out of the loop, this basically a one for one copy of the state monad.
No fancy types at all.
There is nothing going on here.
I feel stupid for pointing out you can do this. [^crazy]

[^crazy]: I'm reasonably sure I'm the only one who ever tried this because I ran into several
          compiler bugs.
          Am I crazy?

Here I used the same name trick to unify the modules with
their respective signatures in the cabal file, which cleans it up a bit:
```cabal

library app
  exposed-modules:
      Death
  hs-source-dirs:
      src/app
  build-depends:
      death:impl,
      death:effects,
      death:effects-io,
      death:effects-app,
```

`death:effects-app` declares our actual "business" logic. and we unifty the `death:effects`
signatures with the modules from `death:effects-io`.
This is a lot nicer to use then having to use that strange
mixin DSL. Which is not hard, the cabal errors are just bad in formatting and output prioritization.
Sometimes the important errors get burried in dozens of other not relevant lines![^example]

[^example]: <details><summary>Cabal hides error example</summary><pre>
$ cabal build
\> Build profile: -w ghc-9.8.4 -O1
\> In order, the following will be built (use -v for more details):
\> - death-1.0.0 (lib) (file src/sig/Death/Base.hsig changed)
\> - death-1.0.0 (lib:effects) (file src/effects/Death/Functor/Signature.hs changed)
\> - death-1.0.0 (lib with Death.Applicative.Signature=death-1.0.0-inplace-impl:Death.Applicative.List, Death.Base=death-1.0.0-inplace-impl:Death.Functor.List, Death.Functor.Signature=death-1.0.0-inplace-impl:Death.Functor.List, Death.Monad.Signature=death-1.0.0-inplace-impl:Death.Monad.List) (first run)
\> - death-1.0.0 (lib with Death.Applicative.Signature=death-1.0.0-inplace-impl:Death.Applicative.Maybe, Death.Base=death-1.0.0-inplace-impl:Death.Functor.Maybe, Death.Functor.Signature=death-1.0.0-inplace-impl:Death.Functor.Maybe, Death.Monad.Signature=death-1.0.0-inplace-impl:Death.Monad.Maybe) (first run)
\> - death-1.0.0 (lib:effects-app) (configuration changed)
\> - death-1.0.0 (lib:app) (configuration changed)
\> - death-1.0.0 (exe:exe) (configuration changed)
\> Preprocessing library 'effects' for death-1.0.0...
\> Preprocessing library for death-1.0.0...
\> Error: [Cabal-7554]
\> can't find source for Death/Functor/Signature in src/effects, dist-newstyle/build/x86_64-linux/ghc-9.8.4/death-1.0.0/l/effects/build/effects/autogen, dist-newstyle/build/x86_64-linux/ghc-9.8.4/death-1.0.0/l/effects/build/global-autogen
\>
\> Building library instantiated with
\>   Death.Applicative.Signature = <Death.Applicative.Signature>
\>   Death.Base = <Death.Base>
\>   Death.Functor.Signature = <Death.Functor.Signature>
\>   Death.Monad.Signature = <Death.Monad.Signature>
\> for death-1.0.0...
\> [1 of 8] Compiling Death.Base[sig]  ( src/sig/Death/Base.hsig, nothing ) [Source file changed]
\> [2 of 8] Compiling Death.Functor.Signature[sig] ( src/sig/Death/Functor/Signature.hsig, nothing )
\> [3 of 8] Compiling Death.Functor    ( src/sig/Death/Functor.hs, nothing ) [Death.Base changed]
\> [4 of 8] Compiling Death.Applicative.Signature[sig] ( src/sig/Death/Applicative/Signature.hsig, nothing )
\> [5 of 8] Compiling Death.Applicative ( src/sig/Death/Applicative.hs, nothing )
\> [6 of 8] Compiling Death.Monad.Signature[sig] ( src/sig/Death/Monad/Signature.hsig, nothing ) [Death.Functor.Signature changed]
\> [7 of 8] Compiling Death.Monad      ( src/sig/Death/Monad.hs, nothing ) [Death.Base changed]
\>
\> src/sig/Death/Functor/Signature.hsig:7:1: warning: [GHC-66111] [-Wunused-imports]
\>     The import of ‘Prelude’ is redundant
\>       except perhaps to import instances from ‘Prelude’
\>     To import instances alone, use: import Prelude()
\>   |
\> 7 | import Prelude (Show(..))
\>   | ^^^^^^^^^^^^^^^^^^^^^^^^^
\> Error: [Cabal-7125]
\> Failed to build lib:effects from death-1.0.0 (which is required by lib:effects-app from death-1.0.0).
</pre></details>

Our test suite uses the state one:
```cabal
test-suite unit
  main-is: Test.hs
  hs-source-dirs:
      test
  build-depends:
      death:effects-app,
      death:effects-state,
```
and it works as expected:
```haskell
unitTests :: TestTree
unitTests = testGroup "Unit tests"
  [
    testCase "run business logic main" $ do
      let (result, ()) = unFunctor Death.business $ State {
        lineInput = "awesemeFile",
        linesOutput = [],
        fileSystem = mempty
        }
      result @?= State {
        lineInput = "awesemeFile",
        linesOutput = ["awesemeFile","reading it again to make sure its iso 42038 compliant","writing file...","file content:","file name:"],
        fileSystem = Map.fromList[("awesemeFile", "awesemeFile")]
        }
```


I don't think it's a good idea to replace standard
typeclasses such as Monad, Applicative and Functor like I've done here.
but I do think you can replace whatever effect system with backpack.
Backpack is more expressive, and very simple to use.
It's more expressive because you can set your base Functor to whatever, including IO.
This same argument goes for runtime performance.[^adoptation]
So you'll get as good as IO by default.


I think it can also compile pretty fast because the module implementations are "orphaned" by design,
so they can be compiled in parralel.
Furthermore there is no instance resolution step.
And finally it's simpler to use,
no fancy types, no need to install anything,
It's already baked in GHC and cabal.
Give it a try! it's not every hard, and let me know what you think in the comment box below.


[^adoptation]: Effect system people always seem so obsessed over this but I don't actually think it matters compared to the time spend on the actual IO part. You probably won't be CPU bound.

## sources

+ This repository has been invaluable: [danidiaz, really-small-backpack-example, Apr 7, 2021](https://github.com/danidiaz/really-small-backpack-example/tree/master/lesson2-signatures)
https://github.com/danidiaz/really-small-backpack-example/tree/master/lesson2-signatures

+ Main backpack thesis, how it all works under the hood: [Edward Z. Yang, BACKPACK: TOWARDS PRACTICAL MIX-IN LINKING IN HASKELL, Oct 10, 2017 ](https://github.com/ezyang/thesis/releases)
  Funny they actually mention typeclasses directly.

+ I made a [reference implementation](https://github.com/jappeace/death) just to make sure I wasn't talking out of my arse and verify it was all possible.

