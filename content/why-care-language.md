TITLE: Why do programmers care about langauge?
DATE: 2018-07-20
CATEGORY: opinion
Tags: Haskell
OPTIONS: toc:nil
status: draft


+ Langauge is turing complete, they all do the same thing
+ Langauge selection as a 'technical' choice
   + Is a ridiciolous statement. They all do the same
   + Selecting one is zero sum: all others lose
+ What do I want to do with a langauge?
   + Build something fast
   + But everyone does?
   + Why is there conflict
+ Enter multi agent systems
   + Believes are local to an agent.
   + I may know x does not mean you know x
   + I think Haskell is fast and efficicient, but there is no way for me to
      tell you that, without you having to actual go and study it.
      Which is big time investment to ask.
+ There are 10 programmers in a room 8 know JS, 6 know Java and python, 1 knows haskell
  Which langauge will they use?





At Daisee colleagues who are not familiar with programming often ask why using
Haskell is important.
Explaining this to people with little technical background is incredibly hard.
What's more important however is that we attempt to construct a well cited
sourced opinion rather than "it's just better" or "it gives more quality".
This post will not attempt to convince programers, as they should [know why](https://wiki.haskell.org/Why_Haskell_matters),
and are most surely familiar with the [price to pay](https://metarabbit.wordpress.com/2017/05/02/i-tried-haskell-for-5-years-and-heres-how-it-was/).
Instead it will attempt to inform 'normal' people,
those who have not sacrificied to the machine god.
Note that in this post I may use technical jargon for completeness in parenthesis.
These sentences can be ignored.

## Why do programmers care about their langauge
Isn't langauge just a tool to get meaning accross to the computer?
Yes, but if one needs to work with this for the bigger part of a day,
having a ten percent increase in productivity is rather nice.
What's more is that it's not just being send to the computer, to contribute to
a code base the programer has to understand what already exist.
So communication is to both the computer and fellow programmers.
Consensus has to be reached on this.
Programming langauges in princpile do not mix
(unless significant effort is undertaken trough [FFI](https://en.wikipedia.org/wiki/Foreign_function_interface), or [libraries](https://en.wikipedia.org/wiki/Library_(computing)) using that).

Note that choosing a programming langauge is a zero sum game. If one is chosen,
that one wins, and all other programming langauges are not chosen and therefore
loose.
Also note that we have mathmatical proofs showing that all programming
langauges do the same thing (a property called [turing completeness](https://en.wikipedia.org/wiki/Turing_completeness)).
Finally do note that if one langauge is chosen, it's hard to change:
Not only does all existing code need to be converted to another langauge,
the other langauge also needs to be thought to everyone in the team.
Although it has to be pointed out that programming langauges do not differ
as much from each other as natural langauges,
they all try to do the same thing after all: Tell the computer what to do.
They influence each other [a lot](https://exploring-data.com/vis/programming-languages-influence-network/) in design.

As an added bonus, the recruiting industry somehow cought on to the discussion
of programming langauges.
Somehow they associated them as being similar to natural langauges
(which is ridicoulus,
it takes years to get going in a natural langauge,
a programming langauge takes maybe a week or two
if you've already learned to program before that).
Every job you accept is now branded with the programming langauge that is used.
Getting rid of it is getting rid of experience.

## What makes a langauge popular
Only langauges that are easy to learn will ever become popular.
This is a self feeding cycle.
Because if a langauge is easier, more people will learn it more quickly,
write more code, create more libraries and make it more attrictive for
buisnesses to pick it up.
Creating more jobs creating more interest.
If 4 programmers in the room know python and 1 knows haskell, the team will
democratically decide to choose python everytime.
Well that's fair right? May the best langauge win.

Is that so? Is easyness to learn the thing a buisness should optimze for?
Because if so it does mean other factors are ignored, such as quality of
deployment (less bugs), and easyness of change or agility.
