Title: Fun with stack
Date: 2018-03-21 13:30
Date: 2017-03-06 22:00
Category: tools
OPTIONS: toc:nil
Tags: stack, haskell, programming
status: draft

Working at Daisee, Jappie uses a lot of Haskell programming.
Although Haskell is obviously as amazing as the [stereotype asserts](https://www.reddit.com/r/haskell/comments/zpff3/larry_wall_you_should_probably_know_about_it/),
the tooling can be a bit challenging.

One's understanding start with the fact that there is not one unified package
manager. Well there is, but only experts use
[cabal](https://www.haskell.org/cabal/) directly.
The issue is that often dependencies don't quite match from the
[Hackage](https://hackage.haskell.org/) repository,
which cabal uses for the source code.
So installing one extra dependency may cause you hours of trying to fit them
all together.

This is were [stack](https://www.stackage.org/) stepped in.
Stack guarantees that at some point in time all the provided packages in their
repository will build together, by essentially hosting their alternative to
[Hackage](https://hackage.haskell.org/), called
[Stackage](https://www.stackage.org/).

Okay so everything is good up till this point. However, 

- ssl
- nix
- git
