Title: Website Launch
Date: 2016-12-25 12:04
Category: Meta
Tags: css, opinion, monetise, theme

So this is the post where I'm launching this website.
I finished the css mostly as a beautifull CGA theme.
In this post I'll discuss my decision making process.

# Why make a site?
I want to be able to express some opinions, mainly trough videos actually but
some times having text is better.
For example when I want to write a guide on what tools I use,
or if I need to do something well referenced.

The main purpose of this is to make money.
I want to adopt a digital nomad like lifestyle (or something similar),
and maintaining a website seems like a good addition.
Currently I'm running this at a loss of course.
Therefore I included advertisements on release and setup a patreon page.
I need to make this work as soon as possible.
However I also hate advertisements,
therefore I make a promise to delete them as soon as patreon can cover the
costs of running the website.
By which I mean hosting costs (vps in my case) and domain name costs.

# Site setup
I use pelican to create this site.
It is a script for parsing text based content formats and generate html from
them, so that you don't have to redefine the menu every blog post for example.
I basically stole this idea from [eev.ee](eev.ee).
So what happens it that you write your content in markdown,
then you run the script which generates the static html for you.
I tried setting up org, but had some problems so I'll use markdown first,
I think I can mix formats anyway.

You can specify the structure of the html in the theme and of course the css.
Note that even if the html is static you can still embed JavaScript,
so you're not committed to just static on each page.
This is why I chose this tool.
No commitments for the future and a relative simple setup.
I'm thinking of later embedding a comment system (probably reddit), and
macgyvering together a vote system, but this has to wait.

## The theme
The CGA theme was an intentional choice.
My target audience is mainly people with interests in technical things,
and I'm assuming that these kind of people will like such a theme either out
of nostalgia or pragmatism.

I got the inspiration from the Gentoo website they launched I think 2 years
ago as an April fools joke, here is a screenshot:

![gentoo cga](/images/2016/gentoo-cga.png)

<!-- screenshot -->
(btw, if anyone has the original html implementation
please contact me, I want to re-host it).
I really loved the idea of just hosting a website like that.
I had started working on a theme, using the kernel config as a style guide:

![kernel config](/images/2016/linux-config.png)
<!-- screenshot -->

However I've been remarkably busy since that time
(grad school, China internship etc),
so I hadn't come around finishing it.
Untill now since I started writing my thesis,
I'm realizing that soon I need to work,
and I don't really want to work for someone else.

I like making CSS designs based of images, and I'm one of those oldtimers that
doesn't use anything fancy like less.js or bootstrap.
So you can see here the result of my work.
I may add some more art work in the future but I'm intending to use this theme
for quite a while.

Which brings us to another advantage of this theme, it will age very well.
Unlike the newest fashion websites where people put huge images at the beginning
of their posts and where you have to scroll endlessly to read something is this
kind of theme tried and tested.
I see the modern website industry becoming more like the fashion industry
where practicality has to make way for beauty.
I refuse to partake in such an "arms race",
redesigning my website every year or having to choose a picture every post
seems just like a chore to me.
If I want to add pictures to a post I'll do it because I want to,
not because its fashionable.

## No CDN's FB links or google analytics
External requests to other domains significantly decrease page loading times.
Don't believe me? Well you can measure it yourself with your browser, it has 
an build-in network monitor (right click -> inspect element -> network, now press
ctr+f5 and see what takes the longest to load, you can see most things go in parallel,
but you should pay special attention to the things that go in sequence).
Many page load speed improvement website actually recommend a CDN,
which is stupid because laying the http connection to a CDN is much slower
then just using the existing connection.

But CDN's aren't meant to increase pageload,
they're meant to decrease serverload by moving parts of the
static content delivery from the dynamic server to dedicated servers,
freeing up server time and thus decreasing server load.
But my entire website is static so why would I bother with that?
server load is not going to be my bottleneck any time soon.

When I browse the internet I use umatrix to block things such as google
analytics and whatever the facebook requests do.
Mainly out of privacy concerns but also because this speeds every website
significantly, especially if you have a poor internet connection.
Besides I don't imagine my target audience uses facebook that much,
correct me if I'm wrong.
