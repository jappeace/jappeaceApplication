Title: Zurich hack 2022
Date: 2022-07-13 15:00 
Category: tools
OPTIONS: toc:nil
Tags: haskell, programming, tools, reflex, frp, servant
subreddit: haskell programming reflexfrp
Status: draft

<style>
img[alt="zurich hack logo, uwu"]{
  width:40%;
  margin-left: 30%;
}
</style>

![zurich hack logo, uwu](images/2022/zurich-hack.svg)

I participated in [zurich hack 2022](https://zfoh.ch/zurihac2022/).
This is a voluntary hackaton[^commercial] organized in
[Rapperswil-Jona](https://www.myswitzerland.com/en/destinations/rapperswil-jona/) [^name].
Naturally I chose to work on researchy [^useless]  project I could find.
Sandy was happy to oblige with his fun [denotational design](https://zfoh.ch/zurihac2022/projects.html#denotational-design)
project.
Here we build an "infinite" baseless chip design,
with a homomorphism in natural numbers to proof correctness.

Our presentation was surprisingly good considering we slapped
it together 30 minutes before presentation time.
However,
we kind off skimmed over explaining denotational design,
and we could've elaborated more on why this proving stuff matters.
I shall use this post to fill in the gaps.
For starters the presentation can be seen here:

<iframe width="560" height="315" src="https://www.youtube.com/embed/fCT0uVCe53Q?start=682" title="YouTube video player" frameborder="0" allow="accelerometer; autoplay; clipboard-write; encrypted-media; gyroscope; picture-in-picture" allowfullscreen></iframe>

I helped presenting[^i'm-on-left], however most of the implementation
was done by Sandy and Nathan.
I wish I could've done more but my Agda isn't good enough yet.
I helped with cheering on their proving efforts and coming up
with ideas for the design.

[^i'm-on-left]: I'm on the left.

## Denotational design
So let's begin on what denotational design is.
You could watch a [video on this](https://youtu.be/bmKYiUOEo2A?t=871),
but in summary[^i-am-not-an-expert]: 

+ We should design based on use, not implementation.
+ Abstractions shouldn't leak.
+ Look for something which is elegant.

I'll explain these more in the following paragraphs.

[^i-am-not-an-expert]: I'm not really an expert on this at all, I just put it in my own mistaken words. Feel free to correct me.

Consider the first point, "we should design based on use, not implementation",
how often have you extended a piece of fucntionality where you had
to work around the existing implementation?
I've done this a lot.
And I think it's part of the engineers job to decide how "valuable" this
chunck of code is, and if it's valuable,
they should consider redesigning it to be better.
To iterate on the design ignoring the existing implementation
and do partial if not complete rewrites if necessary.

Abstractions shouldn't leak is quite interesting,
consider for example IP. how often have you had to know about
the [datagrams](https://www.techopedia.com/definition/6766/datagram)
that make up a packet?
Or had to deal with [MAC](https://en.wikipedia.org/wiki/MAC_address) addresses? I bet never[^if-you-had].
That's because it's a good abstraction that doesn't leak
most of the time.
However, if you had to deal with any off low level IP things,
I apologize, that's some nasty ops.
When I talk about goodness and badness of these abstractions,
I mean the probability of having to deal with the low level machinery.
Consider for example HTTP,
how often did you have to think about the cache behavior of a get request?
Or you probably also know why the User-Agent header is set
to Mozilla [for every browser](https://stackoverflow.com/questions/1114254/why-do-all-browsers-user-agents-start-with-mozilla)
right?
It's because HTTP is a disaster of an abstraction which leaks.
HTTP is ill defined and completely implementation based.
Therefore everyone who uses it,
is forced to know about it's building blocks
and setup conventions to deal with it's lack of structure.

I guess implied in all of this is that it may take several
iterations to get to the right design.
You may end up with a horror of a proof for example.
Which may indicate you're doing something wrong.
I think this is something we didn't drive home enough in the 
zurich hack presentation, yes that proof looked impressive,
but no this isn't something you want.

## Proves and programs

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
the name compose refers to composing a larger chip out of smaller ones.
Although the naming could use some work.

So we've proven these chips behave like we expect for these values and binary circuits.
If you're building a company around chips that only need to be able
to multiply up to 9 and in base 2, this is good enough.
As far unit tests go this is incredibly torough because we're testing against
all possible values in the chip design.
The more common approach is to sample a couple values and call it a day.
Which lead to an alternative more torough approach, fuzzing.
Also known as property testing.

Here you would generate a two random inputs on one side
interpret it trough the homomorphism and then see if the addition in natural 
numbers is the same as the test.

Next proving,
which looks like this:

```agda

```

What we're doing is using syntactic transformations to arrive
from the initial statement to the final statement.
For example, `cong`
ignores a part of a prove so you can apply only on part of the syntax.

## Zurich vibes

But aside from our project, 
another very part is the chill atmosphere.
You go from talking to someone who's been using haskell for more
then 10 years and is upset over having to write unit tests,
to some PHD students trying to add subtyping (but then goodly, somehow)
to Haskell like languages,
to a compiler engineer who casually made a debug tool that
can inspect the heap.
That all in the same night.

It just keeps going like this,
however the nice aspect is that you learn that all these amazing
people, are just people.
They've to eat, drink coffee and get drunk like any other nerd.
They all have some ambition or another,
altough I met quite a few who were there to just socialize.
I guess I'm one of the odd-ones out in that regard,
I just wanted to do something useless, but socializing was a step to far.

[^commercial]:
[^name]: As the name implies. This place is 30 minutes or so driving from zurich.
[^useless]: I guess we had no hope of succeeding,
            which made it all the more worth while trying in my mind.
            After all I spend all year being productive,
            now was a time to do something cool.

