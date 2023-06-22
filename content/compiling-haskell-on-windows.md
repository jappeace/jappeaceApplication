Title: Mysql support for haskell on windows
Date: 2023-06-22 15:44
Category: tools
OPTIONS: toc:nil
Tags: haskell, programming, tools, reflex, frp, servant
subreddit: haskell programming reflexfrp
Status: draft

This is one of those blogpost I'm making because it sounds trivial,
but once you get into this you'll learn the depths of the abys.

Normally I use nix to build haskell.
Nix is almost a perfect solution for haskell.
It provides a functioning binary cache and it allows patching
of libraries where naughty maintainers didn't bother updating
them for whatever reason.

Windows doesn't support nix at all.
Only on WSL[^wsl] there is nix support, but that isn't windows.
Furthermroe the target platform i'm deploying for, windows server 2012,
flatout has no WSL support anyway.
So we've to use cabal (or stack).
So I decided to drink the coolaid of mutable world, and go for ghcup.

[^wsl]: Windows subsystem for linux, eg a neatly integrated linux vm.

Without nix, or stackage, the situation becomes different.
All of hacakge is now available for solving,
meaning cabal will try to use the highest possible version wherever possible.
You've to sortoff jerry-rig cabal with upper version bounds to get a working build.
But it's not just you, it's every library that's transiently dependent as well
that does this.
So if tls 
I didn't know this at the time.

I've to say, the first time I tried doing this,
it wasn't just frustrating, at points I even felt real despair.
I had this fear that there simply was no way of building mysql
support for windows.




So the next best thing is to cross compile.
And although there are claims you can cross compile.
I don't have the time to figure out how.
