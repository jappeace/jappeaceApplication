Title: Reddit poster
Date: 2018-05-07 9:00
Category: tools
Tags: reddit, project, comment
subreddit: programming
status: draft

Yesterday the [Reddit Poster](https://github.com/getpelican/pelican-plugins/pull/1026)
plugin for [pelican](http://docs.getpelican.com/en/stable/) was finished.
This is an initial step towards providing Reddit integration with pelican.
Another goal in the future may be the ability to fetch comments from Reddit 
at compile time and display them below posts.

What this plugin does is look up the list of subreddits in an article,
and post the article to each of them as an URL.
There is also a default 'collection' where everything gets posted too.

However the decision was reached to reduce the scope of this project to just
posting first, as there was not enough time available to make fetching work,
and it was also uncertain if people would even bother to comment on this website.
Although it is assumed people will do as reddit really entices people to get
engaged.

The idea of this project has been lingering in Jappie's todo list for years now.
It is a great relief to finally make some progress in it.
To advertise this website now the only thing that needs to be done is
specifying the list of subs in the relevant article.
