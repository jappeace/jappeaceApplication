Title: The worst monad tutorial
Date: 2018-05-13 16:00
Category: tools
OPTIONS: toc:nil
Tags: stack, haskell, programming, build-tools
subreddit: haskell programming
Status: draft

This blogpost will not make you *any* better at 
haskell or give you any understanding on what a monad is.
In fact it's a waste of bytes, time and you should close the page.
Close it now, I'm serious, this is trash.
Close it.
Still here?
Wow you must be really wanting to understand
monad's if you're willing to give an abysmall source
like this a chance.
Well okay then buckle up buckeruu because
you're in for a ride.

Let's simplify the problem, 
rather then understanding perfection and magnificience
of 'Monad', we'll go to something we all now.
A semiring.
Wait what? I hear you think,
what the hell is a semiring.
unless you didn't finish elementry,
I promise you
that you know this structure.
Because anything that has a `+` and `*`
operation is a semiring.
Like for example natural numbers
(that is numbers bigger then 0),
weight, or lines.
So you see,
since you know how to do multiplication and addition,
you know what a semiring is.
Even though it may be less obvious for a line,
consider multiplying a line of 20cm by 5cm.
it's 100cm.
Easy.
As long as the laws of semigroup are followed,
we can consider it a semigroup.
Often this involves recognition by a practictioner.
But they turn up in unexpected places,
like in haskell, the `Either a b` and tuple form
a semigroup.
Because we can pretend Either is + and tuple is *.

