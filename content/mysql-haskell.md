Title: BONUS Announcement mysql pure unfork
Date: 2023-08-14 17:37
Category: tools
OPTIONS: toc:nil
Tags: programming, mysql, haskell

Good news! I've come to an agreement with the maintainer of
[mysql-haskell](https://hackage.haskell.org/package/mysql-haskell), [winterland](https://github.com/winterland1989).
I'll become a co-maintainer.

What this means in practice is that I'll deprecate [mysql-pure](https://hackage.haskell.org/package/mysql-pure)
and merge the changes back into [mysql-haskell](https://hackage.haskell.org/package/mysql-haskell).
This will make upgrades far more convenient for users.
That's you!
It isn't a big deal for me either way, as I simply want the project to build reliably.
There's no other agenda than that,
so convenience for users is paramount.
[persistent-mysql-pure](https://hackage.haskell.org/package/persistent-mysql-pure) will, from now on, depend on mysql-haskell again.
This fork is still necessary for the persistent users out there,
because my [announcement](https://discourse.haskell.org/t/ann-mysql-pure-fork-of-mysql-haskell/7297)
and [issue](https://github.com/naushadh/persistent-mysql-haskell/issues/2) failed to reach that maintainer.

When winterland becomes active again in the future,
as they intend to,
I hope to hand off maintainership back to them.
