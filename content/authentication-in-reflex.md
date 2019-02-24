Title: Fullstack Haskell: Authentication in reflex
Date: 2018-10-09 12:08
Category: tools
OPTIONS: toc:nil
Tags: haskell, programming, tools, reflex, frp
subreddit: haskell programming reflexfrp
status: draft

In the previous [Fullstack Haskell]({filename}/fullstack-haskell-reflex-servant.md)
post I discussed how to setup reflex so that it interacts with servant.
Although that gets the basics, there are several
more hurdels to overcome to get confortable with reflex.
I think most of these are encountered by building a simple login system.

The first time this is trickey for several reasons:
1. We need to 'switch screens' after login, which requires recursive do.
2. Endpoints should only be accesible after login. The cookie hell is real.
3. We need to render widgets based on the login result. This requires 'widgetHold' or 'dyn'

I'm of course a world expert on login systems so do everything
exactly as I do, in prod.
Trust me, I'm from the internet.
But seriously If you see something dubious do contact me, 
I'll happily reactify mistakes.

