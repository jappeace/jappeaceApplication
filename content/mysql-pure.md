Title: Announcing mysql pure
Date: 2023-07-01 15:44
Category: tools
OPTIONS: toc:nil
Tags: programming, mysql, haskell

I've forked [mysql-haskell](https://hackage.haskell.org/package/mysql-haskell) and [persistent-mysql-haskell](https://hackage.haskell.org/package/persistent-mysql-haskell)
into [mysql-pure](https://hackage.haskell.org/package/mysql-pure)
and [persistent-mysql-pure](https://hackage.haskell.org/package/persistent-mysql-pure).
Neither of those packages seem maintained.
I've done this because they cause me constant issues
for one of my clients on GHC upgrades.
Bounds are out of date and or I need minor patches.
I'm saving myself time by just taking over maintainership,
and setting up various form of CI automation.
For example I've added the automated version
bumping of [nomeata](https://github.com/nomeata/haskell-bounds-bump-action),
and [haskell nightly](https://github.com/jappeace/haskell-nightly) builds[^issue].
I forked because I don't want to go trough the [process](https://wiki.haskell.org/Taking_over_a_package) of 
getting the hackage name space,
that demotivates me.
Furthermore mysql-pure merges several dependencies
of mysql-haskell into one package,
[word24](https://hackage.haskell.org/package/word24),
[binary-parsers](https://hackage.haskell.org/package/binary-parsers),
and [wirestreams](https://hackage.haskell.org/package/wire-streams) are all
now part of this one package.
This is a somewhat different view of package organization,
which may warrant a new package *anyway*.
However, practically no third party packages dependent on the packages
I merged anyway.
Having them split incurred more work.

[^issue]: Which instantly found an issue in ghc on windows for the linker https://gitlab.haskell.org/ghc/ghc/-/issues/23835
