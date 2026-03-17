Title: Announcement: memory / ram fork
Date: 2026-03-17 18:19
Category: announcements
OPTIONS: toc:nil
Tags: programming, haskell, crypto

Hello 👋
Recently there was some [discussion](https://discourse.haskell.org/t/fork-basement-as-baseplate/12415) around forking basement as baseplate.
After some investigation I found out the basement library isn't all that useful,
so I decided to fork memory instead, as [ram](https://hackage.haskell.org/package/ram),
which no longer depends on basement.

This follows the recent [crypton/cryptonite](https://www.reddit.com/r/haskell/comments/14245q8/crypton_is_forked_from_cryptonite_with_the/) fork.
[Crypton](https://hackage.haskell.org/package/crypton) now also depends on ram.

*Migration note:* You may get weird incompatibilities between memory and ram when building with the latest packages.
The trick is to move over to ram, which is a drop-in replacement.

## Why fork?
We're doing this because the original maintainer doesn't want to appoint a new maintainer, however they'll respond to inquiries.
So according to Hackage policy we can't take over their package.[^background]
I opened [an issue](https://github.com/haskell/cabal/issues/11629) on Cabal 
to hopefully make it easier in the future to fork drop in replacements.

Please note that this is not an endorsement of ram or memory packages.
I'm only providing a stable but maintained way of transitioning away from this package.
They could use improvement, and alternatives are welcome!
See [this discussion](https://discourse.haskell.org/t/improving-memory-with-better-abstractions/12350) for example.


[^background]: There also doesn't seem to be a lot of clarity on how we can discuss these policies, but people are working on shedding some light on this.
