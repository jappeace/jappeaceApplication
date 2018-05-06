Title: Comment fetcher
Date: 2018-03-21 13:30
Date: 2017-03-06 22:00
Category: reflection
OPTIONS: toc:nil
Tags: test, reddit, project, comment
status: draft

So I'm trying to tie in reddit into this website as a comment system.
Basically I want to specify in pelican which subreddits this post should be on.
Then use javascript to find the relevant thread and display the comments of
that thread below this post.

# Basic idea

So only stuff would happen on compile time (ie transforming these loosly organized
\*.org files into html), where the treads would be created.
And at runtime on the user client (ie in the browser).
I got this idea from the youtube plugin that replaces youtubes ~~horrible~~
interesting commenting system with reddit posts if available.
I don't want to even bother with an own comment system, because I'm not making
a dynamic website.

# The journey begins

I'm updating this post while I'm working on the system and just using it as the
about URI of the reddit app.
I guess the eventual project will consist of 2 apps really, 1 is the comment
fetching system as a browser agent.
The second is the publisher agent that I will tie into pelican.
After reading [this](https://github.com/reddit/reddit/wiki/OAuth2) it appears that the app type of OAuth forces me to do this
anyway.

The first step I did was make ann "app" on the [OAuth api](https://www.reddit.com/prefs/apps).
This has nothing to do with programming but just making the service aware that I
may be doing some requests. (they shouldn't have called this an "app" but an ear
or channel would be a better name).

Ok so after closer inspection of the [snoowrap api](https://not-an-aardvark.github.io/snoowrap/snoowrap.html#.getAuthUrl),
it turns out that even for reading requests on an app you don't control you
need to require users to authenticate the app.
I don't really believe that will be a good first implementation
(it will put a huge burden on the user, if they even have a reddit account).

An alternative approach of implementing this would be using screenscraping,
but I don't want to do that right now in javascript.

Therefore I think I will just move on to the compile time idea.
This 

# Picking up
A year later and jappie started to look into this idea again.
Because he is looking for a job he has quite a bit of spare time on his hands.
This project however would make his website much more attractive,
as people now could interact and these interactions would be integrated within
the whole.

The compile time idea is that we setup a connection with reddit at compile time.
Then we check on reddit if we already made a post per post.
If this is the case we see if there are any comments.
If so we fetch these.
Then we expose these to pelican so a template can be generated from this comment
structure.

It's true that there are two phases, a publish phase, and a fetch phase,
however, I'm not sure why I'd split these up in seperate plugins.
What may be done is making this configurable, but for now I'd just make a simple
plugin that could do this.
