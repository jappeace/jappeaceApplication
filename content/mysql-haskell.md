Title: (BONUS) Announcement mysql pure unfork
Date: 2023-08-14 17:37
Category: tools
OPTIONS: toc:nil
Tags: programming, mysql, haskell

Good news! I've come to an agreement with the maintainer of
mysql-haskell, [winterland](https://github.com/winterland1989).
I'll become co-maintainer.

What this means in practice is that I'll deprecate [mysql-pure](https://hackage.haskell.org/package/mysql-haskell)
and merge the changes back into [mysql-haskell](https://hackage.haskell.org/package/mysql-haskell).
This will make upgrading far more convenient for users.
That's you!
It is not a big deal for me either way as I just want the project to build reliably.
There is no other agenda then that,
so convenience for users is better.
persistent-mysql-pure will from now on also again depend on mysql-haskell.
This fork still remains true for the persistent users out there,
as my [announcement](https://discourse.haskell.org/t/ann-mysql-pure-fork-of-mysql-haskell/7297),
and [issue](https://github.com/naushadh/persistent-mysql-haskell/issues/2), has failed to reach that maintainer.

When winterland becomes active in the future again,
which they intend to do,
I hope to hand off maintainership to them again.

