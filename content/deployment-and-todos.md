Title: Deployment and todo's
Date: 2016-12-27 12:00
Category: meta
Tags: css, opinion, todo, administration


Yesterday I deployed the website, to my terror it wasn't responsive
 on mobile devices (adapt to screensize).
So with this post I fixed that, by replacing the
basic font-size specifications from small to 1vw (everything else was em).
The rest of the site already used percentages.
The site needs to be responsive because its ironic to have a CGA based theme
being responsive.

So to deploy I found a vps partner in [Galaxy Host Plus](https://galaxyhostplus.com/).
They offer a vps package for about 2 euro 50 in the month I get 512MB of ram,
50GB of disk space 500GB of bandwidth and an IP address.
This is a pretty good deal I think, took me about 2 hours to find (you can get a
lot more expensive for less good specs, it should become cheaper every year).
The domain name I had already registered at [Starhosting](https://www.starthosting.nl/).
Its 11 euro per year.
So to make this site (self) sustainable I need to make at least
$$11/12+2.5=3.42$$ euro per month.
Although this is not completely fair since I'll be using the vps for other stuff
as well, such as Syncthing and private git (not for source code but just personal
stuff, such as undeveloped ideas).

To setup this website it took about 2 days to figure out both pelican and styling
the theme.
I could have done this a lot faster with wordpress or something but it
wouldn't have made me as happy as this theme and going full static website mode.
Full static website mode scales a lot better than wordpress and is cheaper in
maintenance (less band with, less memory used because no PHP or Apache).
Besides, I already knew how to CSS, HTML, and more pure JS than would be
considered healthy (yes I've done some native JS development, no libraries).
Actually now I think about it I would've probably gotten lost in wordpress and
for me it wouldn't have been faster since I really wanted this theme.

Lets talk about why I wanted this theme.
First of all it fits me, I'm the kind of person who doesn't care much for
designs.
Secondly I suspect its more memorable then a standard fashionable website with
bootstrap.
Thirdly I made it from the ground up (not the html structure,
I sort off borrowed that, since I didn't know how to pelican),
which means its quite easy for me to change it.
Fourthly I think there is a select group of people who like this theme,
and those people probably also like what I have to say.
Therefore the theme is practical.

# Todo list
* +Scale mobile+
  This is done now.
* HTTPS
  With lets encrypt around I probably should do this to please the security
* Add advertisements
  This is until my Patreon finally takes off
* Comment system. I have an idea for this, basically to make a reddit bot to
  auto create a thread for me and then link this that per post in this website
  with javascript. Shouldn't be to hard, but probably a weekend of work
  (If anyone wants to help please contact me)

