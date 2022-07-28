Title: Zurich hack 2022
Date: 2022-07-13 15:00 
Category: tools
OPTIONS: toc:nil
Tags: haskell, programming, tools, reflex, frp, servant
subreddit: haskell programming reflexfrp
Status: draft

I participated in [zurich hack 2022](https://zfoh.ch/zurihac2022/).
Zurich hack is a voluntary hackaton[^commercial] organized in
[Rapperswil-Jona](https://www.myswitzerland.com/en/destinations/rapperswil-jona/) [^name], 
with as theme improving the haskell ecosystem and socializing. 
Naturally I chose to work on agda all weekend on the most
researchy [^useless] , project I could find.
Sandy was happy to oblige with his [denotational design](https://zfoh.ch/zurihac2022/projects.html#denotational-design)
project.
It was a lot of fun.

Our presentation was surprisingly good considering we slapped
it together 30 minutes before presentation time,
however,
we didn't explain denotational design well enough,
and we could've elaborated more on why proving matters.
I shall use this post to fill in these gaps.
However for starters it can be seen here:

![ ](https://youtu.be/fCT0uVCe53Q?list=PLOvRW_utVPVnqp24VsF0wiIRa-m9oWrox&t=682)

I helped presenting, however most of the implementation
was done by Sandy and Nathan.
I wish I could've done more but my Agda isn't good enough yet.
My main contribution was cheering on their proving efforts
and coming up with ideas for the design.

## Denotational design
Let's begin on what denotational design is.
You could watch a [video on this](https://youtu.be/bmKYiUOEo2A?t=871),
but in summary[^i-am-not-an-expert]: 

+ We should design based on use, not implementation.
+ Abstractions shouldn't leak.
+ Look for something which is elegant.

[^i-am-not-an-expert]: I'm not really an expert on this at all, I just put it in my own mistaken words. Feel free to correct me.

Consider the first point, 
how often have you extended functionality working
around the existing implementation?
I've surely done this a lot.
I think it's part of the engineers job to decide how "valuable" this
chunck of code is, and if it's valuable,
they should consider redesigning it to be better.
to iterate on the design ignoring the existing implementation
(which likely means some adaptor logic or a partial rewrite).
Zurich hack was special in this regard,
a hackaton with no pressure to come up with result,
of course we can go for the best possible approach!

Abstractions shouldn't leak is quite interesting,
consider for example IP.
how often have you had to know about
the [datagrams](https://www.techopedia.com/definition/6766/datagram)
that make up a packet?
Or had to deal with [MAC](https://en.wikipedia.org/wiki/MAC_address) addresses?
Likely never[^if-you-had].
That's because IP is a good abstraction that doesn't leak
most of the time.
However, if you had to deal with any off low level IP things,
then I apologize, that's some nasty ops.
When I talk about goodness and badness of these abstractions,
I mean the probability of having to deal with the low level machinery.
Consider for example HTTP,
how often did you have to think about the cache behavior of a get request?
Or you probably also know why the User-Agent header is set
to Mozilla [for every browser](https://stackoverflow.com/questions/1114254/why-do-all-browsers-user-agents-start-with-mozilla)
right?
It's because HTTP leaks.
The abstractions don't cover the low level machinery,
furthermore some aspects have become implementation based.
Therefore everyone who uses it,
is forced to know about it's building blocks
and setup conventions to deal with HTTP's lack of structure.

Implied in all of this is that it may take several
iterations to get to the right design.
You may end up with a horror of a proof for example.
Which may indicate you're doing something wrong.
I think this is something we didn't drive home enough in the 
zurich hack presentation,
The proof in our presentation looked impressive,
but this isn't something you necessarily want.

## Proves and programs
Is the design I dreamed up correct?
How do you know this?

We used a technique called a [homomorphism](https://en.wikipedia.org/wiki/Homomorphism)
to prove correctness.
Which is to say,
we brought our chip design back to an interpretation in
natural numbers.
addition or multiplication should be the same for our chip,
as it is in natural numbers.

First of all the most obvious approach is to simply use a
[unit test](https://github.com/isovector/denotational-arithmetic-zurihac/commit/4eb494ad84a1ede2202b036379d8525a391eecbb#diff-201315dac0498e664f0dccffd803e509020bf7d50ce3509d27566a3c26e5cb38R273):
If we map out all possible inputs to all possible outputs we
got a crummy proof:

```agda

mul2x2 : _
mul2x2 = compose add2 add2x2 mul2

_ : (V.map (toℕ ∘ pairμ (pairμ interpretBF) ∘ uncurry (mult mul2x2)) $
        composeTheValues allBools2x2 allBools2x2)
        ≡ (0 ∷ 0 ∷ 0 ∷ 0 ∷
           0 ∷ 1 ∷ 2 ∷ 3 ∷
           0 ∷ 2 ∷ 4 ∷ 6 ∷
           0 ∷ 3 ∷ 6 ∷ 9 ∷ [])
_ = refl
```

Here we're creating a bigger multiplication chip out of an existing one,
by feeding it an add2 chip, an add2x2 chip and a mul2 chip.
the name compose refers to composing a larger chip out of smaller ones.
Although the naming could use some work.

What does this not cover?
For one this only works for the inputs we put in,
in this case those are bits, we've not tested for trits or pentits.
Furthermore this only covers a certain possible size of the chip.

So we've proven these chips behave like we expect for these values and binary circuits.
If you're building a company around chips that only need to be able
to multiply up to 9 and in base 2, this is good enough.
As far unit tests go this is incredibly torough because we're testing against
all possible values in the chip design.

This setup where we test all possible values doesn't happen in practice,
instead the programmer would chose one or two "interesting" values,
and test those,
So a more thorough approach is to sample lots of values and test against these.
This is known as fuzzing or property testing.
Here you would generate a two random inputs on one side
interpret it trough the homomorphism and then see if the addition in natural 
numbers is the same as the test.

There are some issues with property testing however.
For one you're dependent on the quality of the random value
generator for coverage.
Furthermore it's usually difficult to think in terms of poperties.
Finally we can't do polymorphic constructs in property testing
because we can't sample these.


Having a property is already a first step towards a prove.
After all this indicates the first and last statement of the prove.
Now all you've to do is define syntactic transformations to meet
them in the middle.
Unlike property tests, proves are not dependent on random generators
and can do polymorphic types.

Here we introduce the property we want, where `μ` is our interpretation
in natural numbers, and `τ` is the type we put in.

```agda
digitize : ∀ {m} → Fin m × Fin 2 → Fin (m + m)
digitize {m} = cast (trans (sym $ +-assoc m m 0)(+-comm (m + m) 0)) ∘ uncurry combine ∘ swap

record Adder {τ : Set} {size : ℕ} (μ : τ → Fin size) : Set where
  constructor adds
  field
    add : Fin 2 × τ × τ → τ × Fin 2
    zeroA : τ
    proof-add
      : (mnp : Fin 2 × τ × τ)
      → toℕ (digitize (P.map μ id (add mnp))) ≡ toℕ (addF'3 (P.map id (P.map μ μ) mnp))
open Adder
```

With that we can make an interpertation for bits:

```agda
interpret2 : Bool → Fin 2
interpret2 false = zero
interpret2 true  = suc zero
```
This says what size a true and a false are in natural numbers.
Now can define how to add bits:
```agda

add2 : Adder interpret2
add add2 (zero , false , false)     = false , zero
...
add add2 (suc zero , true , true)   = true  , suc zero
zeroA add2 = false
```
The proof is rather obvious:

```agda
proof-add add2 (zero , false , false) = refl
...
proof-add add2 (suc zero , true  , true)  = refl
```
This is still rather simple,
refl means, reflexivity.
In other words, it's so obvious agda can
just look at the definition.

From here the bigger adder can be defined,
which composes one of these adders into a bigger one.
This is the core idea of our DSL, once we got 
a single adder we can grow it trough composition:

```agda
bigger-adder : {σ τ : Set} {σ-size τ-size : ℕ} {μ : σ → Fin σ-size} {ν : τ → Fin τ-size}
               → Adder μ → Adder ν → Adder (uncurry combine ∘ P.map μ ν)
proof-add (bigger-adder {σ-size = σ-size} {τ-size = τ-size} {μ = μ} {ν = ν} x y)
          (cin , (mhi , mlo) , (nhi , nlo))
  with y .add (cin , mlo , nlo) in y-eq
... | (lo , cmid) with x .add (cmid , mhi , nhi) in x-eq
... | (hi , cout) =
  let x-proof = proof-add x (cmid , mhi , nhi)
      y-proof = proof-add y (cin  , mlo , nlo)
      size = σ-size
  in begin
    toℕ (cast _ (combine cout (combine (μ hi) (ν lo))))
    ≡⟨ toℕ-cast _ (combine cout (combine (μ hi) (ν lo))) ⟩
    ...
    toℕ (addF' cin (combine (μ mhi) (ν mlo))) + toℕ (combine (μ nhi) (ν nlo))
    ≡˘⟨ toℕ-addF' (addF' cin (combine (μ mhi) (ν mlo))) (combine (μ nhi) (ν nlo)) ⟩
    toℕ (addF' (addF' cin (combine (μ mhi) (ν mlo))) (combine (μ nhi) (ν nlo)))                             ∎
```

Here `toℕ (cast _ (combine cout (combine (μ hi) (ν lo))))`
is the first step of our prove, or the left hand side of the `≡`,
and `toℕ (addF' (addF' cin (combine (μ mhi) (ν mlo))) (combine (μ nhi) (ν nlo)))`
the right hand side.
What we're doing is using syntactic transformations to arrive
from the initial statement to the final statement.
The statements in `≡⟨ ... ⟩` do these syntactic transformations
Since this proof is rather long and ugly I stripped out most of it.
The full source can be seen
[here](https://github.com/isovector/denotational-arithmetic-zurihac/blob/master/src/Mul.agda).

## Zurich vibes
Aside from our project, 
I think another important part is the chill atmosphere.
For example I asked some people as they'd be working on,
more often then not the answser would be that they're
there mostly to socialize.
In the opening presentation one question that came up
is "where are the showers to swim in the lake?".
I think such a chill vibe sets zurich hack apart from other meetups.

You go from talking to someone who's been using haskell for more
then 10 years and is upset over having to write unit tests,
to some PHD students trying to add subtyping 
to Haskell like languages (but then goodly, somehow),
to a compiler engineer who casually made a debug tool that
can inspect the heap.
That all in the same night.

So naturally, I was quite excited to hear this would be organized
again next year.
Although I'm not going to stay lnoger that time because switzerland
is expensive as fuck.
Even though zurich hack attandence s free, it's still very
expensive to just be there.


[^commercial]:
[^name]: As the name implies. This place is 30 minutes or so driving from zurich.
[^useless]: I guess we had no hope of succeeding,
            which made it all the more worth while trying in my mind.
            After all I spend all year being productive,
            now was a time to do something cool.

