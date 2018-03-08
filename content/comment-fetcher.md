Title: Comment fetcher
Date: 2017-03-06 22:00
Category: reflection
OPTIONS: toc:nil
Tags: test, first, comments
subreddit: netherlands
status: draft

So I'm trying to tie in reddit into this website as a comment system.
Basically I want to specify in pelican which subreddits this post should be on.
Then use javascript to find the relevant thread and display the comments of
that thread below this post.

# Basic idea

So only stuff would happen on compile time (ie transforming these loosly organized
\*.org files into html), where the treads would be created.
And at runtime on the user client (ie in the browser).
I got this idea from the youtube plugin that replaces youtubes horrible 
commenting system with reddit posts if available.
I don't want to even bother with an own comment system.

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

Ok so after closer inspection of the [snoowrap](https://not-an-aardvark.github.io/snoowrap/snoowrap.html#.getAuthUrl) api, it turns out that even for
reading requests on an app you don't control you need to require users to
authenticate the app.
So I don't really believe that will be a good first implementation
(it will put a huge burden on the user, if they even have a reddit account).
An alternative approach of implementing this would be using screenscraping,
but I don't want to do that right now in javascript.
Therefore I think I will just move on to the compile time idea.