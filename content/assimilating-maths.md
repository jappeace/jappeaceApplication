TITLE: Assimilating maths
DATE: 2023-05-27
CATEGORY: reflection
Tags: australia, work
OPTIONS: toc:nil
Modified: 2021-08-07 18:17
Status: draft

Category theorycat[^cat], is sometimes mistaken as one of the requirements for learning Haskell.
This is WRONG.
I've been doing Haskell for 5 years professionally now,
and I hardly ever looked at cat 😼.
I started out crafting Haskell programs without investigating
what a monad is.
There was no need, I made programs, they functioned.
I could figure out the type errors, however painfully at first, but I made progress.
what else did I need?

So it's clear you don't need to know category theory at all to write
Haskell.
However I still was curious.
I always wanted to know what it's about, why the maths people are so
excited by it.
So I recently joined a reading group for the joy of abstraction by Eugenia Cheng.
And I totally get now why people mistake it as a prequisite for learning Haskell.
Cat 😼 is everywhere in Haskell.

Let's start with the most obvious.
The category of set's and functions.
Haskell has first class support for function composition:
```haskell
(.) :: (b -> c) -> (a -> b) -> (a -> c)
```
if you take types as set's (like agda does),
this pretty much means you're working
in the category of set's and functions by using this.
`id` can be used as identity:
```haskell
id : a -> a
```
It's a bit flimsy however because haskell lacks nice dependent type [^support]
support so we can't really encode the proofs easily.
I've just been using agda, to encode this:

```agda
```



[^cat]: hencforth refered to as cat 😼
[^support]: One thing I intend to maybe do in the future is to backport some of these
            cat proofs into haskell. However the syntax isn't very illuminating,
            it's just gore. And even tho agda isn't a pretty language, at least it's understandable for proofs.
