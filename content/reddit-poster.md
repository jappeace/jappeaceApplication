Title: Reddit poster plugin for pelican
Date: 2018-05-07 9:00
Category: tools
Tags: reddit, project, comment
subreddit: programming python

Yesterday the [Reddit Poster](https://github.com/getpelican/pelican-plugins/pull/1026)
plugin for [pelican](http://docs.getpelican.com/en/stable/) was finished.
This is an initial step towards providing Reddit integration with pelican.

What this plugin does is look a predefined list of subreddits names in [an article](https://raw.githubusercontent.com/jappeace/jappeaceApplication/master/content/reddit-poster.md),
then it posts the article to all those subreddits.
Aside from the subreddits in the list, there is also a 'collection' subreddit
where all articles posted too.

Another goal in the future may be the ability to fetch comments from reddit 
at compile time and display them below posts.
However the decision was reached to reduce the scope of this project to just
posting first, as there was not enough time available to make fetching work.
It is hoped however that many people now will start reacting to these posts
so that it may be useful to implement this ability too.
That will however be done in a separate plugin.

This project has been lingering in Jappie's TODO list for years now.
It is a great relief to finally make some progress in it.
To advertise this website now the only thing that needs to be done is
specifying the list of subs in the relevant article.
