Title: Cabal exact print
Date: 2024-05-05 16:00 
Category: tools
OPTIONS: toc:nil
Tags: haskell, programming, tools, reflex, frp, servant
subreddit: haskell programming reflexfrp
Status: draft

For the longest time I've been annoyed with [cabal](https://www.haskell.org/cabal/) telling you
to add modules to your cabal file.
It's capable of detecting they're missing, 
but won't go the extra mile and just add them.
This is not a big deal for a single module, 
but if you like splitting code into many small modules,
for example because you like [fast compile times](https://www.parsonsmatt.org/2019/11/27/keeping_compilation_fast.html#the-projecttypes-megamodule),
or because you're of the radical opinion modules
are [namespaces](https://www.cs.auckland.ac.nz/references/haskell/haskell-intro-html/modules.html),
then any refactor may cause many of these modules to appear.
Which becomes a chore to manage.
Currently if you build a project with an extra module not listed in your cabal file,
GHC emits a warning:
```
<no location info>: error: [-Wmissing-home-modules, -Werror=missing-home-modules]
    These modules are needed for compilation but not listed in your .cabal file's other-modules: 
        X
```

You'd say, why doesn't cabal just add this module to the cabal file?
Well, it can't.
Cabal is currently only able to parse Cabal files,
and print them back out in a mangled[^mangled] form.
There are other programs providing this functionality[^other-programs], but nothing integrated.
Anyway one day I was lamenting this problem
on the internet and I was told a forum user[^self-identified] that
this is in fact the [cabal exact print](https://github.com/haskell/cabal/issues/7544) issue.
In any case, after some exploritary discussion in that thread,
I got a maintainer to [endorse](https://github.com/haskell/cabal/pull/9436#issuecomment-1809209581) 
an [approach I made](https://github.com/haskell/cabal/pull/9436#issue-1989616367).

[^mangled]: you can see this mangling by running `cabal format` on a cabal file,
            the issues I saw were:
            <ol>
            <li> Delete all comments</li>
            <li> Merges any `common` stanza into wherever it was imported.</li>
            <li> Change line ordering </li>
            <li> Change spacing (although perhaps to be expected from a formatter)</li>
            </ol>

[^other-programs]: We have a whole host of projects that re-implement this specific functionality.
                   for example have: 
                   <ul>
                    <li> [hpack](https://github.com/sol/hpack),              </li>
                    <li> [autopack](https://github.com/kowainik/autopack)    </li>
                    <li> [cabal-fmt](https://github.com/phadej/cabal-fmt)    </li>
                    <li> [gild](https://taylor.fausak.me/2024/02/17/gild/)   </li>
                   </ul>
                    Of course many of these projects do more then just module expension.
                    hpack provides a completly different cabal file layout for exampe,
                    cabal-fmt and gild are formatters for cabal file.
                    Only auto-pack just does this one feature.
                    However, since all these programs implement this functionality,
                    there is clearly demand for it.
                    no-one has attempted to centralize it yet however.

Previous attempts were [abandoned](https://github.com/haskell/cabal/pull/7626),
or revolved around creating a seperate AST, which was against maintainer recommendation, 
and then [abandoned](https://github.com/haskell/cabal/pull/9385).

In essence I'm doing a fully integrated design around `parseGenericPackageDescription`.
To make sure the changes I make are helping, I setup several round trip test concerning
the various properties we want see exact printed.
For example, I'll add a cabal [file](https://github.com/haskell/cabal/blob/a75d51b8921f30ec24414f7a3413afc0e0fac111/Cabal-tests/tests/ParserTests/exactPrint/comments.cabal) with comments:
```cabal
cabal-version:           3.0
name:                    two-sections 
version:                 0
synopsis:                The -any none demo
build-type:              Simple

                         -- my awesome
-- library
library 
  default-language:  Haskell2010
  exposed-modules:   AnyNone
  build-depends:     base <5
                    , containers

```

We put that file trough `parseGenericPackageDescription`, 
put the result of that function `GenericPackageDescription` into the `exactPrint` function,
and it should be the [*exact* same](https://github.com/haskell/cabal/pull/9436/files#diff-81e14d1d71534933570bc079db1bbd5795b7b88ec79da5462d586bd8ea637c31R82).
By doing this we should theoretically be allowed to modify
`GenericPackageDescription` however we please,
and get the result printed.
Solving our initial module not listed problem (and [several others](https://github.com/haskell/cabal/labels/exact-print)).

The implementation itself is simple.
We add a [field](https://github.com/haskell/cabal/pull/9436/files#diff-73c00fc0bacfac2e46beb6b5fafba1886f0e32e8678b5173347acfd7ec8aef05R127) to `GenericPackageDescription` with all the exact
print data.
Which we access trough `Map`'s. 
The key is a new concept called a 'NameSpace'.
Values are ExactPostions for example.
I modified the pretty printer to first try and get
an exact position, before falling back to pretty printing.
This instantly solved the tests for sections.

Then there is a long list of features we need to support,
such as comments, common stanzas, 
better comma support, and conditional branches.
Exact printing comments are hard, because cabal just filters them now instead of parsing them. 
so the parser needs to be modified as well.
Fortunately someone has already done some [preliminary work](https://github.com/haskell/cabal/pull/9436/commits/d752e49e526a377f1ec96a37660e0fd9b88cb5e0)
 on that,
unfortunately it breaks my existing tests.
Common stanzas allow users to declare some shared stanza, such as ghc-options
or build depends fields,
and re-use them across various targets, the library or executable.
This is also hard because the common stanzas are currently
just absorb into whatever [they're imported](https://github.com/haskell/cabal/pull/9436/files#diff-39a353df50e7eed47b5958c6025b67b06fac735a8b5b994c1464d6fd84df745eR696),
they don't exist in the `GenericPackageDescription` at all!
Exact printing comma support seems a bit strange, 
but once you think about it for a bit,
you'll see you can put commas in many different places and still have a valid
cabal file.
These locations need to be parsed and tracked somehow.
I've not even spoken about conditional branches yet,
which I've not thought about yet,
however the point is that there is a lot to support!

In general the issue why this exact printing is so hard is because
cabal has a lot of features, all needing to be supported.
And every new feature being added will by default not have exact printing 
support until *some* solution gets merged.
So overtime we can expect exact printing to become even harder to implement.

Why I'm writing this instead of just implementing exact print?
Back in November, I implemented some initial solution in anger,
and the draft PR created, my anger had all but dissipated. 
With clarity of mind, 
I realized spending
all my freetime implementing this feature isn't sustainable.
I'd burn out at that rate.
Essentially I'm looking for a way to make this more sustainable.

A possibility is to create a haskell [tech proposal](https://github.com/haskellfoundation/tech-proposals/blob/main/proposals/templates/CommunityProject.md).
This is a funding mechanism for long standing issues in the haskell community.
With funding I could trade hours in my day job for hours working on this particular issue,
without losing anything on lifestyle.
Currently the issue is that these proposals look like [a lot of work](https://github.com/haskellfoundation/tech-proposals/blob/main/proposals/052-cryptography-leg-1.md).
And I'm not sure how much budget is even available for a project like this.
So this in essence this blogpost is a preproposal,
to see what people think of it without going all the way.

Another thing that came to mind is that I wanted to do is recruit volunteers.
In general, however they'd run into the same issue I had!
But one possibility would be to pitch this as a zurich hack idea,
and work on the exact printer as a group.
The tests can be solved in parrelel, a single person could work on
comments, and another on common stanzas for example.
Which I still may do. 

If I managed to unlock funds trough a tech proposal,
I need to discuss that with the powers that be to make sure
subcontracting is allowed.
After all If I'm payed for the work, 
I would also want to reward successful contributions.

[^self-identified]: They're a self identified milk enthusiast, whatever that means.
