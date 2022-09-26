Title: Zurich hack 2022 Denotational Design
Date: 2022-10-25 15:00 
Category: technique
OPTIONS: toc:nil
Tags: programming, agda, denotational design, zurich hack

<style>
img[alt="zurich hack logo, uwu"]{
  width:40%;
  margin-left: 30%;
}
</style>

![zurich hack logo, uwu](images/2022/zurich-hack.svg)

This blog post is three months overdue, but
I participated in [Zurich hack 2022](https://zfoh.ch/zurihac2022/).
Zurich hack is a voluntary hackaton organized in
[Rapperswil-Jona](https://www.myswitzerland.com/en/destinations/rapperswil-jona/) [^name], 
with as theme improving the Haskell ecosystem and socializing. 
Naturally I chose to work on the most research-y project I could find.
Sandy was happy to oblige with his [denotational design](https://zfoh.ch/zurihac2022/projects.html#denotational-design)
project.
Here we build an "infinite" baseless chip design,
with a [homomorphism](https://en.wikipedia.org/wiki/Homomorphism)
in natural numbers to proof correctness,
more on this in the proofs and programs section.

Our presentation was surprisingly good considering we slapped
it together in 30 minutes.
However,
I think we could've done a better job at explaining
denotational design,
and we could've elaborated more on why proving matters.
I shall use this post to fill in these gaps.
For starters the presentation can be seen here:

<iframe width="560" height="315" src="https://www.youtube.com/embed/fCT0uVCe53Q?start=682" title="YouTube video player" frameborder="0" allow="accelerometer; autoplay; clipboard-write; encrypted-media; gyroscope; picture-in-picture" allowfullscreen></iframe>

I helped presenting[^i'm-on-left], however most of the implementation
was done by Sandy and Nathan.
I wish I could've done more, but my Agda isn't good enough yet.
I helped with cheering on their proving efforts and coming up
with ideas for the design.

[^i'm-on-left]: I'm on the left.

## Denotational design
Let's begin on what denotational design is.
You could watch a [video on this](https://youtu.be/bmKYiUOEo2A?t=871),
but summarized in my own words:

+ We should decompose parts when possible.
+ Abstractions shouldn't leak.
+ We should look for elegance.

[^i-am-not-an-expert]: I'm not really an expert on this at all, I just put it in my own mistaken words. Feel free to correct me.

The first point
is "We should decompose parts when possible".
This means breaking up our design in such a way
we can re-use parts into a larger whole.
For example in Zurich hack,
our first designs was a large record for multiplication
that had everything baked into it.
Then someone had the idea to split that record into
a separate addition and multiplication records
and re-express multiplication into addition.
This allows us to work with the simpler problem
of addition, before tackling multiplication.
Which is what we eventually settled upon as well.
I don't think decomposition has been stated as an explicit
goal of denotational design before,
but it feels implied.
Perhaps in [Conals talk](https://youtu.be/bmKYiUOEo2A?t=871),
"principled construction of correct implementation"
can be interpreted as such.

The point about "Abstractions shouldn't leak" is quite interesting.
We wish to provided a simplified view of the world to the user
through abstraction.
In practice this means we should hide the implementation from the user.
I once suggested for example to add a xor
operation to our record to get rid of the carry bit in certain cases.
After some discussion we settled on not doing this because
xor isn't really a thing you care about when thinking
in terms of semirings[^name].
In other words, when thinking in terms of multiplication
and addition,
you don't want to care about the bit representation.
For more examples of "abstractions shouldn't leak"
I recommend the book 
[algebra driven design](https://algebradriven.design/).

[^name]: The mathmatical name for addition and multiplication

The final point is "We should look for elegance".
Which should serve as a compass upon iteration.
Here again I've an example from just after Zurich hack:
The overflow bit in our addition record bugged me.
It kind off exposes the internals of addition.
So I decided to delete it in favor of doing a full co-product instead.
Doing this would break both multiplication and addition records,
the proofs have to be redone,
I'm not even sure if it's possible.
So there is definitely a cost.  [^real-engineering]
However I like that design, it's more elegant because
this would make multiplication have a product type as input,
and addition a sum type.
Which would have some nice symmetry.
This is something we didn't drive home enough in the 
Zurich hack presentation.
The proof we presented looked impressive,
but this isn't something you necessarily want.
An elegant proof and design is what you want.

[^real-engineering]: In a commercial setting we'd decide if it's worth investing additional
                     in this design.
                     The one presented at Zurich hack works.
                     But if this is intended to be used in a larger system,
                     iterating upon the design may help, if the business can afford it.


## Proofs and programs
I pondered on proofs in software
and how dependent types interplay.
Is the design I dreamed up correct?
How do you know this?
We used a property called a [homomorphism](https://en.wikipedia.org/wiki/Homomorphism)
to prove correctness.
In mortal words,
our chip design was interpreted into [natural numbers](https://en.wikipedia.org/wiki/Natural_number).
Addition or multiplication should be the same for our chip,
as it is in natural numbers.

To start talking about proofs,
we need a design and implementation to proof correctness for.
In our case this was a chip design in Agda.
This Adder[^addition-chip] is something we settled upon after several iterations
of design, but I'm cutting that part out for brevity:[^in-zurich-hack]

[^in-zurich-hack]: In zurich hack we sortoff started out in a classroom with just random ideas.
                   One was quite funny where we somehow ended up with a design that was equivalent
                   to tallying the ones and zeros.
                   But we went in all kinds of directions before settling on using a record.
                   I guess that's the point you've to just try a bunch of stuff
                   and not put to much ego into it.
[^addition-chip]: Addition chip

```agda
record Adder {τ : Set} {size : ℕ} (μ : τ → Fin size) : Set where
  constructor adds
  field
    add : Fin 2 × τ × τ → τ × Fin 2 -- 1
    zeroA : τ -- 2
    proof-add -- 3
      : (mnp : Fin 2 × τ × τ)
      → toℕ (digitize (P.map μ id (add mnp))) 
      ≡ toℕ (addF'3 (P.map id (P.map μ μ) mnp)) 
open Adder
```
Here we're saying, to define an adder you need 3 things.
You need an add operation `1`,
you need a zero `2` [^nathan-zero],
and you need a proof of addition `3`.
The proof of addition is the
homomorphism from our chip to the natural numbers.
Furthermore we don't specify the input type, which is represented by `τ`.
This is done because we want a baseless chip design.
We're fine with arbitrary inputs,
As long as we can interpreted this, represented by `μ`.
A concrete example of `μ` would be an interpertation
into binary values:

[^nathan-zero]: When I asked Nathan to review this post, he mentioned he still hasn't figured out why we needed a zero.
                I'm not really sure either, so it's quite likely this isn't needed at all!
                This maybe an artifact of the time crunch at play.
```agda
interpretBF : Bool → Fin 2
interpretBF false = zero
interpretBF true  = suc zero
```

If we put this into the Adder then `τ = Bool`.
`interpretBF` in this case interprets our code as
a boolean value in natural numbers.
In other words the homomorphism.
This says what value a true and a false are in natural numbers.
Now can define how to add bits:
```agda

add2 : Adder interpretBF -- 1
add add2 (zero , false , false)     = false , zero -- 2
...
add add2 (suc zero , true , true)   = true  , suc zero
zeroA add2 = false -- 3
```

Here we first define the type of `add2` at `1`.
This uses the previously defined `interpretBF` to set `τ = Bool`.
Then we start giving an implementation for `add` at `2`,
which is a simple pattern match into values.
finally we give an implementation of `zeroA` at `3`.

You can do a quick correctness
check before doing a full prove.
For example, in multiplication
we didn't do the full on homorphism prove at first.
It looked daunting,
so we settled on making a [unit test](https://github.com/isovector/denotational-arithmetic-zurihac/commit/4eb494ad84a1ede2202b036379d8525a391eecbb#diff-201315dac0498e664f0dccffd803e509020bf7d50ce3509d27566a3c26e5cb38R273)
instead.
If we map out all possible inputs to all possible outputs we
got a crummy proof:

```agda
_ : (V.map -- 1
        (toℕ ∘ pairμ (pairμ interpretBF) ∘ uncurry (mult mul2x2)) $ 
        composeTheValues allBools2x2 allBools2x2
    )
        ≡ (0 ∷ 0 ∷ 0 ∷ 0 ∷  -- 2
           0 ∷ 1 ∷ 2 ∷ 3 ∷
           0 ∷ 2 ∷ 4 ∷ 6 ∷
           0 ∷ 3 ∷ 6 ∷ 9 ∷ [])
_ = refl
```

At `1` we run our chip design into the interpretation,
and at `2` we expect a multiplication table as result.
Is this test complete?
No, this only works for binary values up to 9,
we've not tested for trits or pentits or higher values.
We did prove however the chip behaves like we expect
for these values.
If you're building a chip company where you only need
multiplication up to 9 in base 2 this is good enough.
As far unit tests go this is incredibly thorough because we're testing against
all possible values in the chip design.
The more common approach is to sample a couple values and call it a day.

Which lead to an alternative approach called property testing.
Here you would generate two random inputs on one side,
interpret it trough the homomorphism
and then see if the multiplication in natural 
numbers is the same as the test.
We didn't do this because
it's sort of difficult to do in Agda.[^agda-noob]
Now you need to figure out how to get your source
of randomness.
Also time was a serious constraint,
and we had a more powerful and interesting technique,
proving!
A proof doesn't have to be hard, for example consider the correctness
prove for our add2 chip:

```agda
  proof-add add2 (zero , false , false) = refl
  ...
  proof-add add2 (suc zero , true  , true)  = refl
```

[^agda-noob]: For me that is, because rember, I'm quite new to this all.

`refl` means, [reflexivity](https://en.wikipedia.org/wiki/Reflexive_relation).
In other words, the statement is simple enough that Agda can
just look at the definition to figure out what it means.
In this case all we do is list out all possible input values,
and tell agda to look at the definition.
`proof-add`'s type signature ensures the implementation is correct.
This is only possible because Agda is dependently typed.
What we proved is that the homorphism is the same under composition for the addition.

This will work for the add2 chip.
However the add2 chip is kindoff useless by itself since it can
only add 2 bits.
Our idea was to compose these adders into bigger adders so that
any size can be represented.
The adder record will then carry the proof for this composition
as well to ensure the homomorphism holds.
To do this we first define the type signature:
```agda
bigger-adder : {σ τ : Set} {σ-size τ-size : ℕ} {μ : σ → Fin σ-size} {ν : τ → Fin τ-size}
  → Adder μ -- 1
  → Adder ν -- 2
  → Adder (uncurry combine ∘ P.map μ ν) -- 3
```
Here we're putting in a low adder `1`, a high adder `2`
which results into a combined adder `3`.
`μ` and `ν` are placeholders for different adders.
This allows us to for example add trits to bits.
The resulting adder `3` maps over both sides of the resulting
tuple[^product] with the interpertation,
and then multiplies them with `combine`
get once again an interpertation into natural numbers.

If you squint a little, the implementation looks like a circuit:
```agda
add (bigger-adder x y) -- 1
    (cin , (mhi , mlo) , (nhi , nlo)) -- 2
    =
      let (lo , cmid) = y .add (cin , mlo ,  nlo) -- 3
          (hi , cout) = x .add (cmid , mhi , nhi)
      in ((hi , lo) , cout) -- 4
```
At `1` we're copattern matching on bigger-adder so we can 
get the underlying `μ` and `ν` adders as `x` and `y` respectively.
At `2` we're getting the actual arguments into the bigger adder.
The type of this is $ Fin _2 \times (\sigma \times \tau) \times (\sigma \times \tau)$
this is where the carry comes into play as argument since
an adder needs to be able to tell when it overflows [^unnesscary].
The actual addition in `3` we pawn off to the underlying adders `x` and `y`,
all we do is hook in the carry[^jappie-ventures].
In `4` we emit the results.

Once we were reasonably confident of our implementation,
we want to prove this correctness.
This is a bit more involved than the boolean interpretation:
```agda
proof-add (bigger-adder {σ-size = σ-size} {τ-size = τ-size} {μ = μ} {ν = ν} x y)
          (cin , (mhi , mlo) , (nhi , nlo))
  with y .add (cin , mlo , nlo) in y-eq
... | (lo , cmid) with x .add (cmid , mhi , nhi) in x-eq
... | (hi , cout) =
  let x-proof = proof-add x (cmid , mhi , nhi) -- 3
      y-proof = proof-add y (cin  , mlo , nlo)
      size = σ-size
  in begin
  begin
    toℕ (cast _ (combine cout (combine (μ hi) (μ lo)))) -- 1
  ≡⟨ toℕ-cast _ (combine cout (combine (μ hi) (μ lo))) ⟩
    toℕ (combine cout (combine (μ hi) (μ lo)))
  ≡⟨ toℕ-combine cout _ ⟩
    size * size * toℕ cout + toℕ (combine (μ hi) (μ lo))
  ≡⟨ cong (\ φ → size * size * toℕ cout + φ) (toℕ-combine (μ hi) (μ lo)) ⟩
    size * size * toℕ cout + (size * toℕ (μ hi) + toℕ (μ lo))
  ≡⟨ {! taneb !} ⟩
    toℕ (addF' cin (combine (μ mhi) (μ mlo))) + toℕ (combine (μ nhi) (μ nlo))
  ≡⟨ sym $ toℕ-addF' (addF' cin (combine (μ mhi) (μ mlo))) (combine (μ nhi) (μ nlo)) ⟩
    toℕ (addF' (addF' cin (combine (μ mhi) (μ mlo))) (combine (μ nhi) (μ nlo))) -- 2
  ∎
```

Note I drastically shortened this proof to make it fit [^full-proof].
What we do is make the first line (indicated by `1`)
be the same as the last line (indicated by `2`).
through steps with equational reasoning.
Every step is small,
and the process is almost fully mechanical pattern matching.
A step is anything within `≡⟨ ⟩`,
which does some small syntax transformation.
The `≡⟨ {! taneb !} ⟩` is a missing step, called a hole.
In this case we request taneb[^ring-solver], to figure out what goes here.
In `3`, we're summoning the proves from `x` and `y` adders
to be used in the composition proof later.
We're making a bigger proof out of smaller ones.

[^ring-solver]: Nathan, a magical ring solver, or flesh and blood person, whichever interpretation suits you better.

If this proof is incorrect, you'll get a compile error.[^incorrectness]
Note that this is similar to property tests,
although it doesn't use randomness and shrinking,
but rather the structure of the implementation
through dependent types.
This is a big step in terms off correctness compared to property tests.
No longer can you have stochastic issues like insufficient sampling,
or biased distributions.
Furthermore smaller proofs compose into larger ones (with the right design).
We can see that for example with `x-proof` in the above block.
but in fact every step between `≡⟨ ⟩` is a prove being re-used.
which comes straight from the implementation.
Property tests however aren't as composable as proofs.
A value generator may be re-used, however care must 
be taken the sampling and bias doesn't become unacceptable.
Finally we're able to prove on polymorphic type variables,
which property tests can't do.
If you have software that /needs/ to be correct,
I think this dependently typed prove approach is a very good option to consider.
I also think Agda is an good choice for a language that supports that.

[^incorrectness]: So what if you're stuck on a proof?
                  This either means you can't think of a function,
                  or it means the thing you're trying to do is impossible.
                  Here you'd need to think really hard if the thing you're designing
                  is correct.
                  You could also try to discuss the issue with a friend or ask
                  on [the internet](https://wiki.portal.chalmers.se/agda/Main/Community).
                  Plenty of people eager to help.
[^full-proof]: The full proof can be seen in the [github repository](https://github.com/isovector/denotational-arithmetic-zurihac),
               although we made some additional changes to the project after the presentation as well.

[^name]: As the name implies. This place is 30 minutes or so driving from zurich.
[^useless]: I guess we had no hope of succeeding,
            which made it all the more worth while trying in my mind.
            After all I spend all year being productive,
            now was a time to do something cool.


## Parting words

Denotational design is an excellent topic of study if you're struggling with questions like
"how do I make my code be more pretty?",
or "how do I design nice and easy to understand libraries?".
Furthermore, even
for commercial code bases we can have correctness proofs.
This is a much more powerful technique than mere property tests,
and puts all that hype around dependent types to work.
We don't need to rely on hand wavy laws asserted merely by
stochastic approximations of proofs,
we can do the real deal!
Please reach out if you're in a domain where correctness
like this is important.
I'd love to chat :).

Special thanks to both Nathan and Sandy for giving useful feedback
on my humble writings.
And of course thanks to all other volunteers who participated,
I had a great time.

[^name]: As the name implies. This place is 30 minutes or so driving from Zurich.
[^unnesscary]: As I mentioned in the design section,
               I don't believe this is necessary,
               but this is an open research question.

[^product]: P stands for product in this case.
            So it's a bimap over a tuple (due to uncurry).


[^jappie-ventures]: Someone may or may not have opened [nandgame](https://nandgame.com/)
                    during zurich hack to show a schema off an adder when we were struggling
                    with correctness.
