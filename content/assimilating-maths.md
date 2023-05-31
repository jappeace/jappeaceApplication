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
support so we can't really encode the properties of a category easily.
In Haskell we call these laws, because you can break them.
In Agda we call them proofs, and we don't give you a program until
you satisfy them. 
Although technically you can get these kind of gaurantees as well
in haskell, for clarity I'll show the properties in Agda:
```agda
record Category {l1 l2 : Prim.Level } {object : Set l1} (arrow : object -> object -> Set l2)  : Set (l1 Prim.⊔ l2) where
  constructor category
  field
    -- structure, things you do with the data.
    identity : {a : object} -> arrow a a
    _∘_ : {a b c : object} ->  arrow b c -> arrow a b -> arrow a c

    -- properties, the rules the structure satisfies
    unitʳ : {a b : object} (f : arrow a b) -> f ∘ identity ≡ f
    unitˡ : {a b : object} (f : arrow a b) -> identity ∘ f ≡ f

    associativity : {a b c d : object} (f : arrow a b) (g : arrow b c) (h : arrow c d) -> (h ∘ g) ∘ f ≡  h ∘ (g ∘ f)
```

we can with this definition, create a cateogry of sets and function:
```agda
setsAndFunctions : {l : Prim.Level } -> Category { l2 = l } (λ a b -> (a -> b))
identity setsAndFunctions {arg} = λ a -> a
_∘_ setsAndFunctions bc ab = λ a → bc (ab a)
-- the proofs are enforced by agda's typesystem.
unitˡ setsAndFunctions a = refl
unitʳ setsAndFunctions a = refl
associativity setsAndFunctions a b c = refl
```

the proofs are all refl (reflexivity, eg this is obvious by definition),
because the type system gaurantees the properties.
This guarantee is also true in haskell however!
so the haskell definition is a type class:
```
class Cateogry cat where
   id :: cat a a
   (.) :: cat b c -> cat a b -> cat a c
```
As you can see only the structure is defined,
the properties are pushed into the comments.
However,
we can write property tests for this,
for some categories.

Before introducing you to the next level of categories
I want to go to a more basic category.




[^cat]: hencforth refered to as cat 😼
[^support]: One thing I intend to maybe do in the future is to backport some of these
            cat proofs into haskell. However the syntax isn't very illuminating,
            it's just gore. And even tho agda isn't a pretty language, at least it's understandable for proofs.
