Title: Zurich hack 2022 Denotational Design
Date: 2022-07-13 15:00 
Category: technique
OPTIONS: toc:nil
Status: draft
Tags: programming, agda, denotational design, zurich hack

<style>
img[alt="zurich hack logo, uwu"]{
  width:40%;
  margin-left: 30%;
}
</style>

![zurich hack logo, uwu](images/2022/zurich-hack.svg)

I participated in [zurich hack 2022](https://zfoh.ch/zurihac2022/).
This is a voluntary hackaton organized in
[Rapperswil-Jona](https://www.myswitzerland.com/en/destinations/rapperswil-jona/) [^name].
Naturally I chose to work on the most researchy [^useless]  project I could find.
Sandy was happy to oblige with his fun [denotational design](https://zfoh.ch/zurihac2022/projects.html#denotational-design)
project.
Here we build an "infinite" baseless chip design,
with a [homomorphism](https://en.wikipedia.org/wiki/Homomorphism)
in natural numbers to proof correctness
, more on this in the proves and programs section.

Our presentation was surprisingly good considering we slapped
it together in 30 minutes.
However,
we skimmed over the denotational design part,
and we could've elaborated more on why this proving stuff matters.
I shall use this post to fill in the gaps.
For starters the presentation can be seen here:

<iframe width="560" height="315" src="https://www.youtube.com/embed/fCT0uVCe53Q?start=682" title="YouTube video player" frameborder="0" allow="accelerometer; autoplay; clipboard-write; encrypted-media; gyroscope; picture-in-picture" allowfullscreen></iframe>

I helped presenting[^i'm-on-left], however most of the implementation
was done by Sandy and Nathan.
I wish I could've done more, but my Agda isn't good enough yet.
I helped with cheering on their proving efforts and coming up
with ideas for the design.

[^i'm-on-left]: I'm on the left.

## Denotational design
So let's begin on what denotational design is.
You could watch a [video on this](https://youtu.be/bmKYiUOEo2A?t=871),
but in summary[^i-am-not-an-expert].
I'll try to re-explain this in my own words,
so in summary: 

+ We should decompose parts when possible.
+ Abstractions shouldn't leak.
+ We should look for something which is elegant.

I'll explain these more in the following paragraphs.

[^i-am-not-an-expert]: I'm not really an expert on this at all, I just put it in my own mistaken words. Feel free to correct me.

The first point
is "We should decompose parts when possible".
We can see this from our chip design in zurich hack,
one of the first designs was just a large record for multiplication
that had eveyrthing slapped into it.
Then someone had the idea to split multiplication record into
addition and multiplication and re-express multiplication into addition.
This is what we eventually settled upon as well.
I don't think this has been stated as an explicit goal of denotational
design before, but it feels implied.
Perhaps in Conals talk "principled construction of correct implementation"
can be interpreted as such.
And the first design we came up with wasn't composable at all,
I remember seeing a large mult record,
so my suggestion was to split it up,
which we eventually did.

Abstractions shouldn't leak is quite interesting.
We wish to provided a simplified view of the world to the user
through abstraction.
In practice this means we should hide the implementation from the user.
In zurich hack I for example suggested to add a xor
operation to our record to get rid of the carry bit in certain cases.
After some discussion we settled on not doing this because
xor isn't really a thing you care about when thinking
in terms of semirings[^name].
Another example is the IP[^algebra-driven-design] abstractions.
How often have you had to know about
the [datagrams](https://www.techopedia.com/definition/6766/datagram)
that make up a packet?
Or had to deal with [MAC](https://en.wikipedia.org/wiki/MAC_address) addresses?
Rare to never I suppose?
That's because it's a good abstraction that doesn't leak
most of the time.
An example of leaky abstraction is HTTP,
how often did you have to think about the cache behavior of a get request?
Or you probably also know why the User-Agent header is set
to Mozilla [for every browser](https://stackoverflow.com/questions/1114254/why-do-all-browsers-user-agents-start-with-mozilla)
right?
It's because HTTP leaks and doesn't serve well to simplify reality.

[^name]: The mathmatical name for addition and multiplication
[^algebra-driven-design]: I've ripped these examples straight out of Sandy Maguire [alrgebra driven design](https://algebradriven.design/)

The final point is "Look for something which is elegant.".
Which should serve as a compass upon iteration.
Here again I've an example of zurich hack,
where we did our presentations,
everything was done.
However that overflow bit bugged me.
It kind off exposes the internals of addition.
So I decided to delete it in favor of doing a full co-product instead.
This breaks both multiplication and addition recrods,
the proofs have to be redone,
I'm not even sure if it's possible,
but that doesn't matter.
I like that design, it's more elegant.
Keep in mind the dictionary definition of elegance:

1. Graceful and stylish in appearance or manner.
2. (of a scientific theory or solution to a problem) pleasingly ingenious and simple.

Once you've completed a design,
you maybe even proved some properties or wrote some tests already,
now the final step is to ask yourself, "is this elegant?".
Rarely the anwser would be a truthfull yes,
but you can get closer to it by deleting,
factoring out components and simplifying.
It may take several
iterations to get to the right design.
You may end up with a horror of a proof for example.
Which may indicate you're doing something wrong.
I think this is something we didn't drive home enough in the 
zurich hack presentation,
yes that proof looked impressive,
but no, this isn't something you want.
A simple proof is more powerful

## Proves and programs

So on zurich hack I pondered on the role of proves in software
and how dependent types play into this.
This is about program correctness:
Is the design I dreamed up correct?
How do you know this?
We used a technique called a [homomorphism](https://en.wikipedia.org/wiki/Homomorphism)
to prove correctness.
Which is to say,
we brought our chip design back to an interpretation in natural
number to assert addition or multiplication is the same for our chip,
as it is in natural numbers.

First of all the most obvious approach is to simply use a [unit test](https://github.com/isovector/denotational-arithmetic-zurihac/commit/4eb494ad84a1ede2202b036379d8525a391eecbb#diff-201315dac0498e664f0dccffd803e509020bf7d50ce3509d27566a3c26e5cb38R273):

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

so here we're creating a bigger multiplication chip out of an existing one,
by feeding it an add2 chip, an add2x2 chip and a mul2 chip.
`interpretBF` in this case interprets our code as a boolean value in natural numbers.
In other words the homomorphism.
We use `pairμ` to create this single interpretation into an interpretation that
works with a tuple of booleans: 
```agda
pairμ : (Bool -> Nat) -> ((Bool, Bool) -> Nat)
```
Applying this twice allows us to read the result of `(mult mul2x2)`
which returns a `((Bool, Bool), (Bool, Bool))`.
the name compose refers to composing a larger chip out of smaller ones.
Although the naming could use some work.

So we've proven these chips behave like we expect for these values and binary circuits.
If you're building a company around chips that only need to be able
to multiply up to 9 and in base 2, this is good enough.
As far unit tests go this is incredibly thorough because we're testing against
all possible values in the chip design.
The more common approach is to sample a couple values and call it a day.

Which lead to an alternative more torough approach, property testing.
Here you would generate a two random inputs on one side
interpret it trough the homomorphism
and then see if the addition in natural 
numbers is the same as the test.
We didn't do this because our unit tests were better,
and it's sort of difficult to do[^agda-noob] in agda
because now you need to figure out how to get your source
of entropy (randomness).

[^agda-noob]: For me that is, because rember, I'm quite new to this all.

Next proving,
What we proved is that the homorphism is the same under composition for the addition.
So this will work for any size chip.
it looks like this:

```agda
  begin
    toℕ (cast _ (combine cout (combine (μ hi) (μ lo))))
  ≡⟨ toℕ-cast _ (combine cout (combine (μ hi) (μ lo))) ⟩
    toℕ (combine cout (combine (μ hi) (μ lo)))
  ≡⟨ toℕ-combine cout _ ⟩
    size * size * toℕ cout + toℕ (combine (μ hi) (μ lo))
  ≡⟨ cong (\ φ → size * size * toℕ cout + φ) (toℕ-combine (μ hi) (μ lo)) ⟩
    size * size * toℕ cout + (size * toℕ (μ hi) + toℕ (μ lo))
  ≡⟨ {! taneb !} ⟩
    toℕ (addF' cin (combine (μ mhi) (μ mlo))) + toℕ (combine (μ nhi) (μ nlo))
  ≡⟨ sym $ toℕ-addF' (addF' cin (combine (μ mhi) (μ mlo))) (combine (μ nhi) (μ nlo)) ⟩
    toℕ (addF' (addF' cin (combine (μ mhi) (μ mlo))) (combine (μ nhi) (μ nlo)))
  ∎
```
Note I drastically shortened this proof to make it fit [^full-proof].
What we do is make the first line: `toℕ (cast _ (combine cout (combine (μ hi) (μ lo))))`
be the same as the last line `toℕ (addF' (addF' cin (combine (μ mhi) (μ mlo))) (combine (μ nhi) (μ nlo)))`.
trough steps with equational reasoning.
So a step is anything within `≡⟨ ⟩`,
which does some small syntax transformation.
The `≡⟨ {! taneb !} ⟩` is a missing step, called a hole.
In this case we request taneb (Natan), to figure out what goes here.

If this proof is incorrect, you'll get a compile error.
Note that this is similar to property tests,
although it doesn't use randomness and shrinking,
but rather the structure of the implementation
trough dependent types.
So if you've ever had issues with the arbitrary instances
of quick check,
you maybe interested in this whole proving stuff as well,
and by extension dependent types.

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
Furthermore, I really want to promote the idea out there that even
for commercial code bases we can have proves.
We don't need to rely on hand wavy laws asserted merely by
stochastic approximations of proves (property tests),
we can do the real deal!
It's not that hard.


