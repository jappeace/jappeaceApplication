Title: Announcing mysql pure fork
Date: 2023-08-13 13:37
Category: tools
OPTIONS: toc:nil
Tags: programming, mysql, haskell, database

I've forked [mysql-haskell](https://hackage.haskell.org/package/mysql-haskell) and [persistent-mysql-haskell](https://hackage.haskell.org/package/persistent-mysql-haskell)
into [mysql-pure](https://hackage.haskell.org/package/mysql-pure)
and [persistent-mysql-pure](https://hackage.haskell.org/package/persistent-mysql-pure).
The original packages were no longer maintained and frequently caused me issues 
during GHC upgrades,
so I decided to take over maintainership. 
For example, bounds were outdated, or I needed minor patches found on
[obscure branches](https://github.com/chordify/persistent/tree/persistent-mysql-haskell-9.2)
in [unrelated repositories](https://github.com/naushadh/word24/tree/ci).
I've set up various CI automations.
Specifically, I've integrated automated version 
bumping from [nomeata](https://github.com/nomeata/haskell-bounds-bump-action) and 
incorporated [haskell nightly](https://github.com/jappeace/haskell-nightly) builds[^issue].

I forked because I didn't want to go through the [process](https://wiki.haskell.org/Taking_over_a_package) of 
obtaining the Hackage namespace,
which I found discouraging.
Furthermore, mysql-pure merges several dependencies
of mysql-haskell into one package.
[word24](https://hackage.haskell.org/package/word24),
[binary-parsers](https://hackage.haskell.org/package/binary-parsers),
and [wirestreams](https://hackage.haskell.org/package/wire-streams)
are all now part of this one package.
This presents a somewhat different perspective on package organization,
which might warrant a new package *anyway*.
However, practically no third-party packages depend on the packages
I merged,
So having them separated results in more work nobody appears to benefit from.

[^issue]: This instantly found an issue in GHC on Windows for the linker: https://gitlab.haskell.org/ghc/ghc/-/issues/23835.
