TITLE: Why haskell?
DATE: 2024-04-07 17:10
CATEGORY: reflection
Tags: blog
OPTIONS: toc:nil
Status: draft

During a reflection of the past year haskell came up as a point of contention,
some business folk were skeptical of this technology [^replacement],
whereas I was the opinion that it's the best choice for webservers.
On their request I gave a "lunch and learn" [presentation](https://jappie.me/talks/why-haskell.html#/title-slide)
about why people should use Haskell.
I thought this was an excellent idea since it has been a while since I thought about this.
This is a transcription of the ideas I presented.
Primarily for my own record[^years], however maybe some of y'all find use in these words as well.

[^years]: I've used haskell for years now. 
          I think I originally started with it because I wanted to use the best tools, 
          solving the hardest problems.
          I primarly focussed on concurrency, I found rust interesting. 
          But Haskell was just better at it.

## Productivity 
In short, I think Haskell gives a large boost to productivity trough it's typesystem.
Sometimes my dear colleagues like to speak in terms of capabilities
of a language,
but due to a property called turing completeness this doesn't make a lot of sense.
Turing completeness means that any programming language with this property
has the same capabilities.
Therefore I think it's more constructive to talk about *restrictions*.
Haskell has many restrictions due to it's typesystem.
Which is an automated sanity checker for software, which ensures internal consistency.
This rejects many nonsensical or buggy programs before you're even allowed to run them!

=== TODO DRAGON EXAMPLE

This is an example of introducing types:
```haskell
data Nationality = Dutch | English | Other
data Person = Person Int Text Nationality 
```
Two types are introduced, a `Person` and a `Nationality`, a `Nationality` is either `Dutch`, `English` or `Other`. In this program `Nationliaty` can be nothing else.
So a way to think of this is a set of possibilities.
A `Person` consists of an `Int`[^short], a `Text` and a `Nationality`.
so `|` stands for `or`, posibility, wheras a space between the constructors stand for `and`.
This is sometimes reffered to as product (and), and sums (or). 
In this case `Text` could stand for a persons' name for example, `Int` a persons' age
and natonality for their nationality.
Both `Text` and `Int` are already built in or provided by libraries, so we can just use those types.

To give meaning to these definitions we need to use them.
Now we've some types to work with we can define functions:

```haskell
getName :: Person -> Text  -- type signature
getName (Person _ _ field) = field -- implementation
```
The first line here is the type signature.
The second line the implementation of the function.
In the implementation, all we do is pattern match, or destruct the person, and pull out a field.
The pattern match happens before the `=`, and the result of the function is after the `=`.
We've some more functions to introduce to see the typesystem in action:
```haskell
getName2 :: Person -> Text
getName2 (Person _ field _) = field

getName3 :: Person -> Text
getName3 (Person field _ _ ) = field
```
Here I asked the audience the question, which of these functions is correct?
In other words, which field do we need to pull out to get a `Text` back so we 
satisfy the typesignature?

In fact, we don't need to think, we can just run the typechecker:

```haskell
Main.hs:10:29: error:
    + Couldn't match expected type 'Text'
                  with actual type 'Nationality'
    + In the expression: field
      In an equation for 'getName': getName (Person _ _ field) = field
   |
10 | getName (Person _ _ field) = field
   |                              ^^^^

Main.hs:16:31: error:
    + Couldn't match expected type 'Text' with actual type 'Int'
    + In the expression: field
      In an equation for 'getName3': getName3 (Person field _ _) = field
   |
16 | getName3 (Person field _ _ ) = field
   |                                ^^^^
```
So clearly gatName2 is correct.
This is a type error. The only reason this works is because we setup
the types, or our expectations, like that.
If we delete the types all these functions would compile because the
types would be inferred.
In essence what you do most of the day as a haskell error is solve these type errors.
Every error solved is akin to solving a bug in a lesser typed language[^other].
In haskell all statements are always typed and have to be internally 
consistent.

[^other]: There are other commercially used languages with typesystems, 
          but these typesystems are either weaker, or the language doesn't reduce
          to a single expression or usually both[^weaker].
          Typescript in many cases gives up on inference (using any). 
          You got your jvm langauges (java, scala and kotlin) with subtyping issues.
          Or Rust, which doesn't reduce to a single expression, 
          or lacks features such as higher kinded polymorphism.
           

Another example is around pattern matching, here we want
to figure out if someone is Dutch:
```haskell
data Nationality = Dutch | English | Other -- previously defined nationality
isDutch :: Person -> Bool -- typesignature
isDutch (Person _ _ nationality) = case nationality of -- implementation
  Dutch -> True
  English -> False
  Other -> False
```

the `isDutch` function takes a person and returns a boolean,
indicating whether they're dutch by True, or not, by returning False.
We pattern match to figure out what has to happen.
In fact there are two pattern matches, one destructs the person and gets the
nationality, the second one decide based on nationality with `case` `of` 
if they're dutch or not.
In Haskell, all value level computation is driven by pattern matches[^reduction][^most-of-language].

Here I asked the audience, what happens if we add another alternative to Nationality,
say we wish to introduce a German nationality.
In fact what happens is that we get a warning at the pattern match:
```haskell
Main.hs:21:36: warning: [-Wincomplete-patterns]
    Pattern match(es) are non-exhaustive
    In a case alternative:
        Patterns of type 'Nationality' not matched: German
   |
21 | isDutch (Person _ _ nationality) = case nationality of
```
The audience mentioned you'd get an exception[^exceptions] if you'd use this function,
which is true if you don't enable and solve these warnings.
In practice what you do for commercial project is turn these to 
warning into errors as well[^w-error].
So this would be effectively a compile error.

All these type errors introduce a short feedback cycle.[^addictive]
You setup the compiler in filewatch, and on any change it'll tell
you if it's right or wrong.
There is no need to write any tests asside the typesignatures.
I dubbed this "technique" **Type error driven development**.

Before the presentation I asked the colleague who I'm teaching haskell
as part of this assignment to comment on her experience:

> Can be difficult and challenging at times,
  but much easier when you've got someone experienced in haskell who really knows their stuff to help you and I like being able to just try run it and see what the compiler says.
  So you can incrementally get closer and closer to getting it right.

My colleague also uses the compiler as an
assistant to help you write correct programs.
**Type error driven development** is real.
And unlike an AI asisstent, it's reliable! [^haskell-and-ai]

## Expressiveness
Okay so that were all my slides on the typesystem, magnificient as it is
let's move on to some other advantages.
Haskell is a conscise language, we don't need to write a lot
of code to express a lot.[^information-density]

```haskell
determineLoadState :: LocationStatus -> LoadStatus -> LoadStatus
determineLoadState unitStatus currentState = case unitStatus of
  -- we should ignore unregisterd units for load status
  StatusNotRegistered -> currentState
  StatusUnknown UnitNotAvailable -> currentState -- also unregistered
  -- if the state is unkown make sure to track it
  StatusUnknown reason -> setUnknown reason currentState
  -- if a state is located here, we mostly want to map it to located here,
  -- except if a state was unkown
  StatusLocatedHere _ -> locateHere currentState
  StatusLoaded _ -> locateHere currentState
  -- dispatched and delivered are current state because we start
  -- of with the assumption it was dispatched
  StatusDispatched _ -> currentState
  StatusDeliveredToSite _ -> currentState
  StatusInstalled _ -> currentState
```

For context, this is probably the most complicated piece of logic the code base.
It's deriving a load (of concrete units), 
which is derived from the individual unit status (named `LocationStatus` in this case),
which is derived from gps location and the state of the sensor fleet.
All this is reduced to a simple pattern match.

Actually, I cheated a little in preperation of this presentation, 
the original code was a double pattern match like this:
```haskell
determineLoadState :: LocationStatus -> LoadStatus -> LoadStatus
determineLoadState unitStatus currentState = case unitStatus of
  StatusLocatedHere _ -> case currentState of
    LoadUnknown x -> LoadUnknown x
    LoadDispatched _ -> LoadLocatedHere
    LoadLocatedHere -> LoadLocatedHere
    LoadNoTimeDetermined -> LoadLocatedHer
```
However this was factored out in a seperate function like so,
```haskell
locateHere :: LoadStatus -> LoadStatus
locateHere currentState = case currentState of
  LoadUnknown x -> LoadUnknown x
  LoadDispatched _ -> LoadLocatedHere
  LoadLocatedHere -> LoadLocatedHere
  LoadNoTimeDetermined -> LoadLocatedHere
```
Which brings us back to the original branch definition:
```haskell
determineLoadState :: LocationStatus -> LoadStatus -> LoadStatus
determineLoadState unitStatus currentState = case unitStatus of
  StatusLocatedHere _ -> locateHere currentState 
```
we simply swapped out the resulting type with a function.
I called this "logical substitution" in the presentation,
but I was reminded this is also known as equational reasoning.
The fact you can do this is reliably[^try-other-languages] is extremly powerfull for refactoring.

## Search
Suppose we want to read a file from the filesystem. 
We know we need a `FilePath` as input to this function.
Do some side effect `IO` to access the filesystem,
and the filetext would be text.
So this is the signature we're looking for:
```haskell
FilePath -> IO Text
We can in fact use [hoogle](https://hoogle.haskell.org/?hoogle=FilePath%20-%3E%20IO%20Text) 
to search for that:


[[https://jappie.me/talks/img/hoogle-read-file.jpg]]

As far as I'm aware no other commercial language has 
this kind of capability.
You can search trough google of course as well,
but searching just on types is a Haskell concept.

## Petri dish
Another advantage of haskell is that it's used as a petri dish
of research.
In fact one of the original motivations of haskell was to
allow more efficient communication between fp researchers.
If you learn haskell, reading papers that use haskell instantly become accessible
for free.
Furthermore the langauge is packed full of interesting and novel libraries.
A taste of interesting areas are:
+ dependent types
  + [singletons](https://hackage.haskell.org/package/singletons)
  + [agda](https://hackage.haskell.org/package/Agda) 
  + [idris](https://hackage.haskell.org/package/idris)
+ effect systems
  + [polysemi](https://hackage.haskell.org/package/polysemy)
  + [freer-simple](https://hackage.haskell.org/package/freer-simple)
  + [effectfull](https://hackage.haskell.org/package/effectful)
+ property tests
  + [QuickCheck](https://hackage.haskell.org/package/QuickCheck)
  + [hedgehog](https://hackage.haskell.org/package/hedgehog)

This is just a sample, all of them have competing implementations! 
Which is great for hashing out a good design,
naturally it's not so good if you want to choose and are not sure which one is best 
for your situation.
Here asking people with experience is invaluable.

## Learning curve
One major disadvantage of using haskell is the learning curve.
Assuming someone can already program,
we've seen that it'll take at least 2 weeks of just grinding
trough an issue before someone even starts to produce anything.
After these 2 weeks they are somewhat independent and
can start working on issues on their own.
Implementing a full backend feature would take about 6 weeks.
During this time it's recommended you have access to an experienced
developer to deal with issues.

We also limited the scope of learning. 
So all those petri dish projects
I mentioned won't actually appear in commercial production code.
Production code is relatively simple haskell.
This is so we can more easily onboard non haskell programmers.
We need the majority of the code to be in this simple form 
because it allows new developers to become productive first,
before exploring more advanced concepts.

Developers can explore these more advanced concepts in either self
improvement time or art projects.
In the past I've also experimented with allowing lesser used parts
of the codebase be used as petri dish area.
However I think this can go wrong as well if commercial
suddenly decides the features implemented by those code 
parts are in fact important.
Now you've a chunck of inaccassile code which needs rapid modification.

This learning curve is a one time investment.
Once you've leanred how to be productive in haskell,
you'll gain access to all this research for the rest of your carreer.
Furthermore you'll be able to use a programming langauge
which is decades ahead of anything else.

## Tooling
We found haskell language server (HLS) unreliable
for comerrcial development.
What happened was that people used this as the only source of truth
for compiler errors.
While developers had to ship features this
this program at times would randomly crash.
Causing a large amount of frustration!

One of the first things I did was depracate HLS
and switch back to normal caba/stack based development.
The features that HLS provides are neat,
but if it's unreliable I can't recommend it.

However I don't think we've had other issues besides this.

## Commercial adoptation
So using haskell sometimes gives you the feelng you're
the only one using it.
There are quite often unmaintained libraries, 
or sometimes even non-existent libraries.

You got your github, meta, google using haskell, or at least 
they're [sponsors](https://haskell.foundation/donations/) 
of the haskell foundation.
You've various consultancies in Haskell of course, 
and then random successfull companies such as scrive or juspay.
And then you got various smaller startps using haskell,
such as artificial, supercede and textql.

## Other languages
I also briefly did a comparison between other languages.
During the presentation I mentioned at times the main other language
used typescript,
but people already wanted to move away from that,
so this section was specifcally about showcasing how haskell
was better then these choices.

### Kotlin
Here I mentioned how kotlin has mutable state,
wheras haskell doesn't by defalt.
So in haskell you retain this logical substition property
or equational reasoning.

Also haskell's typesystem is far more advanced.
It can do no typesearch, nor type error driven development.
After all, if it compiles you've no idea if it'll work.

In fact, kotlin inherits subtyping from java.
This is the concept of dog and cat both extending animal.
It's a strange featre because even OOP advocates
would [favor composition over inheritence](https://en.wikipedia.org/wiki/Composition_over_inheritance#cite_note-2).


### Go
Go shares the same issue as kotlin with mutable state.

It has a very explicit way of doing error handling.
However you can do the exact same thing in haskell with
and `Either` type, in fact my blogpost goes into [that]({filename}/failing-in-haskell.md).

### Rust
I didn't mention rust in the original presentation.
But in an earlier blogpost, 
certain commenters thought it "fun" to compare haskell to rust.
So I shall have som "fun" at your expense.

Don't [forget](https://doc.rust-lang.org/std/mem/fn.forget.html), rust leaks memory safely.
Rust borrow system is kindoff annoying for most tasks I want to do.
Having a garbage collector is a huge boost to producitivy.
Rust implemented the wrong syntax, opting for async/await instead of do notation.
Rust just doesn't look nice.

## Questions

From your dragon example I get that this language allows you to put belts and
braces in place such that silly mistakes don't make it into programs which would
happen in other languages?

Yes! Indeed, many large financial orginisations chooose haskell due to it's correctness
properties.
For example before I worked for a reinsurance tech company, now re-insurance
deals are about large sums of money, so correctness is paramount.

Normally when big players contribute to the language, they also contribute to the tooling.
So why hasn't this happened for haskell?

Would we use haskell more in future projects after seeing the benefits?

+ talked about depending on if the resources are availalbe
+ which situations is it good.
   + eg embedded is a no go
   + good for webservers and compilers
   + frontend, maybe (reflex)
      + alternative tech such as purescript.


[^try-other-languages]: You can try doing this in other languages, but if you've a notion of mutable state, you'll quickly learn this isn't easy.

[^information-density]: I think a lot of people who first start with haskell get kindoff tripped over how conscise everything is.

[^haskell-and-ai]: I've encouraged her to use AI in her code writiing. The compiler will catch most bullshit anyway. It's a really nice combination!

[^addictive]: I suspect most people get hooked into haskell because of this, I even suspect it's mildly psychologically addictive. Mike Kasberg strongly recommends short feedback cycles for [software development](https://www.mikekasberg.com/blog/2019/02/24/feedback-cycles.html#what-does-the-data-say). Kasberg M. "Why Short Feedback Cycles Lead to Great Software" 24 february 2019.

[^exceptions]: Haskell indeed supports the notion of exceptions. Which is often a matter of debate when to use them. See my blogpost [failing in haskell]({filename}/failing-in-haskell.md) for details.

[^w-error]: there is a [-Werror option](https://downloads.haskell.org/ghc/latest/docs/users_guide/using-warnings.html#treating-warnings-as-fatal-errors). If you're interested in how to configure all these stuff see my [haskell template](https://github.com/jappeace/haskell-template-project/blob/master/template.cabal#L50). Which is good for commercial projects assuming you want to use nix (which you may not want to do depending on the institutional knowledge in your orginisation, I decided against nix for example).

[^most-of-language]: We can even reduce the entire langauge to function application, pattern matching, and a type system to keep it internally consistent. We've not seen function application at  this poin in the blogpost.

[^reduction]: Also known as reduction. It's not specified explicitly in the report. See this [paper](https://citeseerx.ist.psu.edu/document?repid=rep1&type=pdf&doi=392c7408723ee8d14e3f38ff22aa475b98a50704): Harrison, William L., and Richard B. Kieburtz. "Pattern-driven reduction in Haskell." 2nd International Workshop on Reduction Strategies in Rewriting and Programming (WRS02), Copenhagen, Denmark. 2002. 


[^short]: short for integer, not to be confused with the `Integer` type which is a big Integer, and can be any size, whereas `Int` can only be a certain size, due to hardware constraints.


[^replacement]: For context, the CTO absolutely loves Haskell. However, the team lead of this project unfortunatly grew discontent with it. Mostly due to relying on [HLS](https://github.com/haskell/haskell-language-server) for type errors, which is (currently) to unreliable for serious development. Furthermore the project adopted several advanced libraries and techniques causing a lot of overhead for a beginner which impaired the feeling of productivity and caused frustration. They got the feeling the learning never stopped! The "fix" for this project was to make the code easier to modify with less learning, and we used the compiler now directly instead of HLS.
