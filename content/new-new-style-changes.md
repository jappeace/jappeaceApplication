TITLE: New new website style changes
DATE: 2018-03-08
CATEGORY: meta
Tags: thesis, tips, technique 

Due to circumstances in his life, Jappie Klooster decided it's
time for some more website changes!
These are all intended to simply reduce the amount of time it costs maintain
this website.
This should allow Jappie Klooster to update it more regularly as the friction
for updates is decreased.

# Brutalist style
Jappie Klooster became unhappy with the style, although it entertains him endlessly,
it may give the wrong idea to a third party.
To untrained eyes it may look like he attempted to make it look good
(which wasn't the intention at all).
So rather than trying to make it beautiful, Jappie decided to go in full brutalism mode.
In essence all style except the most practical ones will be stripped.
For reference:

### old
![old](/images/2018/old-theme-reference.jpg)
As the reader see the old style was rather dark themed.
It used the linux kernel compilation program as a reference, as discussed
[in another post]({filename}/website-launch.md).

### new
![new](/images/2018/new-theme-reference.jpg)
In the new style Jappie just started deleting html and CSS untill he came
accross this, it's not as brutalist as initially promised,
but looks a lot more attractive and was a lot more simple to accomplish.

Jappie intentionally put the image of the new style in here, even though it's the
current style. It is assumed that this style is not permanent either.

# Getting rid of org mode
The believe is still held that org-mode is a superior standard compared to markdown.
It can do so many things, such as inline code execution and pipe the result to
the document itself, as having full reference support.
These are just some of the many great features of org-mode.
However org-mode has issues.
The fact that it's entirely confined to Emacs for HTML rendering is a massive problem.
Emacs needs to be booted for each article just to do the HTML rendering.
There is no easy way Jappie Klooster knows of to keep this process active as a daemon somehow.
Technically Emacs can do this, but it is meant to do communication with XORG,
not to act as a script execution platform.

In the future Jappie Klooster may do investigation to make this work fast,
however for now the easiest way of increasing building times for this website is to just
use the markdown export feature of org-mode.
The reason he chose org-mode in the first place as a viable alternative is that it
doesn't try to lock you into it.
He now executes this option, because there is simply no time to investigate the
preferred route.
