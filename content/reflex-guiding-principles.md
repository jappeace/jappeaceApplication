Title: Reflex guiding principles
Date: 2018-10-09 12:08
Category: tools
OPTIONS: toc:nil
Tags: haskell, programming, tools, reflex, frp, servant
subreddit: haskell programming reflexfrp
Status: draft

This is an attempt to start a discussion on how to make sensible reflex architecture.
There have been reddit threads about this before [here](https://www.reddit.com/r/haskell/comments/7nxni9/reflexdom_vs_miso/).


# Minimize the amount of events
Browser is chuck full of events, you want to hide as much as possible.
If you can, get rid of them.

# Put of merging events untill the last moment
Because it's unclear when a event will fire from just the type system,
it maybe be prudent to avoid merging them to much.

# Minimize monadic do
This can cause weird runtime errors because of the recursive nature.


# minimize monadic functions
You want to keep everything as pure as possible.
First try to keep you functions on just values,
if you can't try using just the timeline (Reflex t)
only after that put stuff in wdigets.


# Avoid using large scope dyn and widgethold
I used to grab for these when dealing with collections, however
they tend to have ill effect on dynamic values inside them.
They'll just keep on resetting state.
Which is kindoff ugly.
In most cases I've regretted using them as they have unexpected effects on state.
In essence they'll swoop away whatever is going on on getting an update.

There are build in functions for dealing with collections that are much better suited.
Such as listwithkey (for maps) or simplelist (for normal lists).
These will give inidividual dynamics.
