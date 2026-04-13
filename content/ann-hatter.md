Title: Hatter: Native Haskell mobile apps
Date: 2026-04-13 01:00
Category: announcement
OPTIONS: toc:nil
Tags: haskell, mobile

[Hatter](https://github.com/jappeace/hatter) is like [flutter](https://flutter.dev/) but instead of dart, haskell!

Write native mobile apps in Haskell!
This works similar to react native where we have
tight bindings on the existing UI (user interface) frameworks
provided by android and iOS.

<style>
figure {
  float: right;
  margin: 2em;
  margin-top: 0em;
  width: 15em;
}
@media (max-width: 420px) {
  figure {
    float: none;
  }
}
figcaption{
  font-size: xx-small;
  color: #999;
}
</style>
<figure>
<img  alt="Hatters gonna hat" src="/images/2026/hatter.png" />
<figcaption>Have I gone mad? I'm afraid so. You're entirely bonkers. But I'll tell you a secret. All the best people are.</figcaption>
</figure>

This project cross-compiles a Haskell library to Android (APK) and iOS (static library / IPA),
with a thin platform-native UI layer (Kotlin for Android, Swift for iOS).
There is support for android wear and wearOS as well,
because I personally want to build apps for those. 
iOS and Android support was just a side effect.

Hatter fully controls the UI.
This is different from say [Simplex chat](https://github.com/simplex-chat/simplex-chat) 
where Java or Swift calls into their Haskell library.
Hatter wrote all Swift and Java code you'll ever need,
so you can focus on your sweet Haskell.

Haskell is a fantastic language for UI.
Having strong type safety around callbacks and widgets 
makes it a lot easier to write them.
I basically copied Flutter's approach to encode UI,[^technically-dissimilar]
but in flutter it's a fair bit of guess work, 
what to write where.
It's *very* nice in Haskell however.
I'm annoyed at the languages
they keep shoving into my face for UI.
With [vibes](https://jappie.me/haskell-vibes.html) I put my malice
into crafting something good.
Flutter UI DSL (Domain Specific Language) is already pretty good, 
but the syntax is complex,
and it inherited many foot guns from Java.
Furthermore it's annoying to use android studio,
when there is a perfectly good [emacs](https://www.gnu.org/software/emacs/).
With Hatter I can keep e-maxxing!

[^technically-dissimilar]: Ironically the rendering looks more like react-native. Flutter doesn't use the native widgets but renders its own canvas, which is a lot more work! It does make it consistent accross platforms however. Which is part of their branding I suppose. I considered this option but decided to do a small humble contribution instead.

Example app:

```haskell
{-# LANGUAGE OverloadedStrings #-}
module Main where

import Data.IORef (newIORef, readIORef, modifyIORef')
import Data.Text qualified as Text
import Foreign.Ptr (Ptr)
import Hatter
  ( startMobileApp, MobileApp(..), AppContext
  , loggingMobileContext
  , newActionState, runActionM, createAction, Action
  )
import Hatter.Widget

main :: IO (Ptr AppContext)
main = do
  actionState <- newActionState
  counter <- newIORef (0 :: Int)
  increment <- runActionM actionState $
    createAction (modifyIORef' counter (+ 1))
  startMobileApp MobileApp
    { maContext     = loggingMobileContext
    , maView        = \_userState -> do
        n <- readIORef counter
        pure $ Column
          [ text $ "Count: " <> Text.pack (show n)
          , button "+" increment
          ]
    , maActionState = actionState
    }
```

I had a lot of trouble figuring out initialization.
What we want is to let the UI framework call into
Haskell from Java or Swift.
But we also don't want to force the user to write
their own FFI layer. That's what the library supposed to do.
Finding out that `main` can return whatever
type it wants was a great solution.
`AppContext` now returns all callbacks we need for a functioning app.
Which includes life cycle management.
The `Ptr` is a stable pointer, which allows us to avoid globals completely![^funny]
Pretty neat!
And no, AI will not find solutions like this for you.
You've to torture it in just the right way ;).

## We're All Mad Here: Wrangling Probabilities

The biggest "innovation" here is to just force it to write integration
tests for every feature.
We want it to self validate via code for practically everything.
CI is its code reviewer.
This prevents it from regressing without you noticing it.
At times it'll still try disabling the test suite, or pretend the errors were already existing.
This usually means it got a too 'difficult' job, and you've to break it down in smaller steps.
Or ask it to just do research on one part.
Doing "research" isn't a standard skill like planning mode, but it's very useful
because it allows it to be more creative.
The problem is that it tries placating you and if it thinks it can't do a programming task in reasonable time,
it'll start cheating.
If you tell it to research you sorta let off the time pressure.

Most designs and architectures it comes up with are kinda [bad](https://github.com/jappeace-sloth/haskell-mobile/blob/d6c643f887497c150e8e7a8b0781d31e986a06f1/docs/incremental-rendering-approaches.md).
You've to help it a lot here, 
the entire animation system would've been so bad if I would've let it have it's way.
Firstly it did tree diffing very wrong, and secondly it started out with wanting
the user to register it's own handlers or something?
Now an animation is just a node in the widget tree.
So it becomes easy to use, I guess I value that a lot,
which isn't a right or wrong answer per se, like a Nix build.

It's good at solving Nix builds, but it still does weird things.
it wanted to replace the generic Nix builder with its own scribbles for dependencies, 
which would break the entire hpkgs dsl.
This would for example mean you'd jailbreak a library and then hatter would ignore your instructions.
I had it to tell it to please not do that.
It claimed this wasn't possible.
So I asked it to research it anyway,
And it turns out it was easy to use the standard builders.
It just needed some research time I guess.
It implements stuff for the task at hand and there is no foresight at all.
That's okay though, it's fast. Fast is good.

Another big problem is template Haskell.
We're doing cross compiling and that's very troublesome, 
because we've to execute the cross compiled code,
however this is now the wrong architecture!
This was however mostly solved by the AI.
It'll happily grind away for 3 hours on
a Nix build to get it to work, 
if you tell it that is its entire job.
This kinda kept coming back in various ways and I'm not sure why.
Partly because I don't understand much of the Nix harness it built I guess.
It's so weird getting functioning software you don't fully understand.
I did have it make [some](https://github.com/jappeace/hatter/blob/master/docs/template-haskell-android-crosscompilation.md) 
[reports](https://github.com/jappeace/hatter/blob/master/docs/armv7a-android-wear-crosscompilation.md) on it, which I don't understand.
It says I should [upstream](https://github.com/jappeace/hatter/blob/master/docs/upstream-analysis.md), but I still don't understand.
I'll do nothing for now.

I made [prrrrrrr](https://github.com/jappeace/prrrrrrrrr/) in the hatter framework. 
I'm intending to make at least a couple more apps in that,
I've ideas but I never had a nice framework to work on in.
Around 50k lines of code [spent](https://motleybytes.com/w/Edsger_Wybe_Dijkstra_quotes/Lines_of_Code) 
in 2 weeks or so - Pretty crazy.
Worth it to reach contentment.
Haters gonna hate, hatters gonna hat.

[^funny]: The funny thing is that I was doubting dealing with the globals was even "worth my time", but after I saw the solution,
          I knew this was much better. It just becomes a lot clearer what's going on if you see the data flows in type signatures.
