Title: Haskell is about relentless composition
Date: 2021-03-06 15:44
Category: tools
OPTIONS: toc:nil
Tags: haskell, programming, tools, reflex, frp, servant
subreddit: haskell programming reflexfrp
Status: draft

If you ask people why haskell, you may get a beautiful pallet of answers:
Some people say haskell is about functional programming, the right way to write programs is with immutable values, haskell is pure so if you like functional programming you must *love* haskell [^vague].
Other say it's about abstraction, maths, science and invention.
Haskell has various people doing their phd's in extending the langauge,
therefore if you like being ahead of your peers in programming features, you'll *love* haskell!
Then others may argue the magic lies in types. It has the most advanced typesytem of any commercially used langauge, surely if you like typesystems you must love haskell right?

I'm not saying these perspectives are wrong, just vague.
I don't like vague.

I've been thinking about this subject for a long time,
more then a year now.
In the beginning I wanted to anwser the question "why haskell" over any other langauge,
but that desire has lost me, I no longer care about convincing people.
Mostly becuase if a company is responsible enough to write their entire stack in javascript I think I prefer that.
For one, it'll provide countless of man years of job security for people who don't give a shit about what job they do.
And for two, I strongly believe haskell is such a deflationary force that trying to convince others to use it is like trying to make a coming tornado blow faster with a fan.

Today I'll introduce another perspective which is both simple and very concrete.
Haskell is about relentless compisition.
Composition is about making larger things out of small things.
Being able to compose is incredibly useful in programming,
because you want to decompose a big problem into small ones.
Rather then solving the problem of "automated video editing" in one big chunk,
I'd rather first work on cutting out pointless silences,
then do motion detection,
and then add sentiment detection to figure out the really interesting parts.
The problem of cutting out silences in video itself is also
hard enough to be decomposed, so the process continues
until you end up with problems that are almost trivial,
like calculate the length of a list.
After this we can write individual solutions for each small
problem,
or better yet, find libraries to solve these problems for us,
and then compose them together into a big solution!

Every programmer does this, to some extend or another.
The more aggressive they are with decomposition and recompesition,
the more productive they are.
I claim that Haskell as a programming langauge will help you become relentless
at composition,
you can do this in other languages, but it's the easiest in haskell.
Furthermore, I'll explain this right now.
There is no trick or hidden door.
It's all common sense.

In haskell we can compose small functions into big ones like this.

```haskell
smalFunctionA :: a -> b

smalFunctionB :: a -> c

bigFunction :: a -> c
bigFunction = smalFunctionA . smalFunctionB
```

Take for example python, how do we compose functions in python?


```python
a = TypeVar('a')
b = TypeVar('b')
c = TypeVar('c')
def smalFunctionA (x : a) -> b:
    ...

def smalFunctionB(x : b) -> c:
    ...

bigFunction = compose2(smalFunctionA, smalFunctionB)
```

Not too bad right? How about java:

```java
class X{
  static final Function<A,B> smalFunctionA = ...;
  static final Function<B,C> smalFunctionB = ...;
  static final Function<A,C> bigFunction = smalFunctionA.compose(smalFunctionB)
}
```

I'd argue that the pretty small syntax of python's `compose2`, instead of `.`,
makes it a significantly worse langauge for compesition.
Something like optics would be obnoxious to use in python, yet it has real utility!
Java is hopeless.



[^vague]: Isn't it vague functional programming? Besides plenty of my haskell code is imperative as fuck, and works great like that.
