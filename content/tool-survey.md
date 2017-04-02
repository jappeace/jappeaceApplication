TITLE: Tool survey
DATE: 2017-04-01
CATEGORY: tools
Tags: time, efficiency, tweakability, power

Some time ago I made a blog post about thesis [writing tips](thesis-writing-tips).
However while writing that a large part started to be about text editing tools
and version control.
To keep the thesis writing tips post more focused I postponed writing about
that. This post treats my tools of choice.

All people will only ever care about a niche of tools.
The reason is that in our world, we
have too many choice to form an opinion about each of them.
Tell me for example about your favorite pick axe or transport ship,
you probably don't have an opinion about these tools.
Which just shows how more complex our society became compared to for example the
stone age,
where everyone had a favorite type of stone.

Note that isn't enough to form an opinion about a tool to care about it,
you actually have to have used it for a while,
and then switch to another quite different tool to start caring.
Only once you switch you will form a deeper 'bond' with the new one or old one.
I think I switch quite often.
Which brings us to another reason for this post,
I want to compare my current tools of choice with those of future me.

# Operating System
This is where things will get interesting.
The main theme of my selection has always been tweakability.
I want to be able to change things I don't like.
This is seen most clearly from my OS choice:
[Gentoo/Linux](https://gentoo.org/),
this is a source based rolling release distribution.
Most distributions work with pre-compiled software.
However software provides often at compile time, flags that enable or disable
features.
Gentoo standardizes this and allows you to specify per software package
which options you want or don't want.

The reason I chose Gentoo initially is more petty, a class mate of my was using
it, and I didn't even understand what "source based" or "rolling release" meant.
I wanted to be at least at the same level as my classmates,
so in my stubbornness I spent about 2 weeks installing it on my own the first
time, with help of the handbook.
It took so long because I didn't know anything really about the shell and unix,
Gentoo really forces you to learn.

Now I stick with Gentoo because it allows me to do customizations,
while updates won't break these.
Gentoo's packagemanager is very careful in preserving configuration files.
Unlike Ubuntu,
which upgraded me out of my custom alternatively installed desktop environment.
Gentoo has to be more careful, because of its inherent configurable-ness.
Whereas Ubuntu provides ease of use, which goes hand in hand with,
knowing it better than the user.

# Control everything with keyboard
During my software engineering classes I got introduced to the idea that the
mouse is pretty slow to work with.
The key difference between mouse usage and keyboard only usage is that mouse
usage often involves searching the right place to click, wheareas using the
keyboard is just doing what you want to do.
By systematically making the mouse less important,
working with the computer becomes more like playing an instrument.
I use several programs that minimize mouse usage:

1. [i3-wm](https://i3wm.org/) or its wayland successor [sway](http://swaywm.org/)
   (as soon as wayland takes off, it'll probably be some years later)
   This is the window manager that fills the screen with active window by
   default, or splits it if there are multiple windows open.
   It also has work spaces (basically groups of windows under a global tabbing
   system).
2. [vim](http://www.vim.org/). This is a text editor with "modes".
   normal mode is navigation, and insert mode is for typing text.
   If you work a lot with text its definitely worth learning this.
   But beware, expect the first two weeks or so of using this program to
   have lower productivity. (and basically no productivity the first day ;) )
3. [Spacemacs](http://spacemacs.org/). This is an evil combination of Emacs and
   vim. I use this for software development and writing larger texts.
   whereas I use vim for quick and dirty edits (besides vim is available on
   every Unix system whereas Emacs and especially Spacemacs is not).
   Oh I also run this in daemon mode so that I can use i3 to move windows rather
   than the pretty bad buffer navigation of Emacs.
4. [Ranger](https://github.com/ranger/ranger). This is a file browser that works
    with vim like bindings, I also have a Spacemacs plugin that does the same
    inside Spacemacs.
    Actually if you combine i3 + vim + ranger you have a really solid IDE.

I haven't found a good way to eliminate mouse usage from the browsing experience.
There are plugins that give vim like navigation,
but I never really found them compelling to use.
This maybe just personal taste,
but I think it also has to do that browsing,
is usually quite similar to searching.

# Write everything down
I've slowly over the years come to realize how unreliable my memory is and 
how liberating it is to write stuff down.
No need to think about something which you've already written down.
I have a little idea project in which I write down random ideas
I have for programs or systems, even political once.
The reason for doing this is that the idea can then exit my mind without the
fear of forgetting it,
often I will later extend it or realize its unfeasible or impractical.

To this project I also added a planning file which contains some stuff I
probably should work on.
This really helps focusing and narrowing down a direction I want to go in.
They also provide a dialogue between passed me, current me, and eventually
future me.
It was a classmate who brought me up on this idea, but CGPGrey pointed out
the dialogue aspect.
The dialogue aspect also holds true for ideas,
but ideas are generally not plans yet.
Plans have some form of commitment too it, you just need to find the time.

## Version control
I mentioned "projects" before, by this I mean I have the files under version
control.
I use [git](https://git-scm.com/) for version control.

Wait, I'll clarify the concept of version control first.
Think of it as Ctrl+Z on steroids managed by a dedicated program.
It can track changes over multiple files and directories, and with each
change you want to save you can add a message.
It can also send these changes to other machines or 'locations'.
There are actually a bunch of options for this
[svn](https://subversion.apache.org/),
[cvs](http://www.nongnu.org/cvs/),
[bazaar](http://bazaar.canonical.com/en/) and many more.
At this point its safe to say however that git crushed all competition,
the reason for this is that the architecture of git compared to other systems
is dead simple.
Also [github](https://github.com/) helped a lot.

However I don't just use git for just  programming projects, no.
I use also it for important configuration files, my todo list, my idea project
and of course my thesis.
Basically most real information I put into my computer that can be represented
as plain text without to much trouble, I put in git.
The reason for this is that it has a much nicer history than for example dropbox,
and provides advanced merge mechanisms.
Besides with git I don't need to have to trust some external service,
since its decentralized.

Actually git has slowly become a diary of mine.
I think future me, or historians if I ever become that important,
can track most of my life at this point with help of git.
In the back of my mind I've actually developed an opinion that git usage
is maybe a basic computer skill and should be thought on schools,
since it could make collaborative work on documents much easier. 
Although I know this won't happen in the near future.
Since it would also require the rejection of binary formats for text processing
all together. Which is something harder to ask than an open format.


# Discovery
The source of finding new tools is the most important thing to discuss.
I think I had three major sources over the years:

1. School, mandatory content
2. School, classmates
3. Reddit

The first two sources have essentially become defunct since I started writing
the thesis. However I'm expecting a fourth source in the form of colleagues
once I start working.

I think mandatory school  content is generally the worst quality.
First of all it lags behind the inovation curve.
Secondly most people already know how to use that since its mandatory so you'll
have most competition on that area if you're an expert.
Finally it doesn't really give you much of opinions on the matter at all.
Giving dumb reasons for learning the tool "you have to",
or "this is industry standard" rather than giving a comparison between
alternatives.

Classmates are a much better source, in fact one of the best I think because
they can tell you their opinion, and opinions are *so* important.
Even if you disagree with them, it forces you to defend your position
and reconsider, which makes your choice stronger.
Or if you fail you may flip and gain a new experience.

I have mixed feelings about reddit, its a major source of distraction,
but it has a lot of gems (not even hidden, I mean reddit has search functionality!).
I often subscribe to sub-reddits of tools I'm interested in using to learn more
about them and to lurk on those valuable opinions.
But the hivemind, seriously fuck that politically correct piece of shit...
<sup><sup>I love you</sup></sup>.
Our relation is complicated.

# Paranoia (security)
I want to say, if you don't know much about computer security, stop reading,
because its a lot of work and you may never get rewarded for your work.
Anyway there are some issues I recently addressed to make identity theft harder,
and my online accounts more difficult to "hack".

I'll start with the online accounts.
I use a password manager called [Keepass](http://keepass.info/),
actually I use the [qt based one](https://keepassxc.org)
(works better with Linux).
The reason for this is complicated and explained very well
[here](https://www.youtube.com/watch?v=3NjQ9b3pgIg)
but basically its more safe than memorizing several.
I use [syncthing](https://syncthing.net/) to share the database with my phone
and my server, I don't wanna lose that.
I just have a huge password, I didn't want to do the keyfile, because I'm afraid
to lose that.

I recommend everyone to use a password manager.
Its much easier to use,
it will autofill passwords in your browser and you just have to remember the
master password.
It allows you to have much less "stuff" in your mind and its more secure.
I even put WIFI passwords in there.
[Network manager](https://wiki.gnome.org/Projects/NetworkManager)
also allows viewing of WIFI passwords,
at least in [plasma](https://www.kde.org/plasma-desktop),
but since I'm almost always in i3 its just easier to have in Keepass.
Besides now I have the WIFI passwords on my phone too.

For authentication (making it difficult to steal my identity),
I went trough the effort of setting up
[pgp](https://www.gnupg.org/) for email and git.
For email I use the [Thunderbird](https://www.mozilla.org/en-US/thunderbird/)
extension
[enigmail](https://www.enigmail.net/index.php/en/).
All my git commits and emails are signed now.
To configure signing in git I followed
[this](https://git-scm.com/book/en/v2/Git-Tools-Signing-Your-Work).
[This](https://stackoverflow.com/questions/10161198/is-there-a-way-to-autosign-commits-in-git-with-a-gpg-key)
stackoverflow question explains how to sign automatically.

Linus Torvalds
[believes](http://git.661346.n2.nabble.com/GPG-signing-for-git-commit-td2582986.html)
auto signing is unnecessary.
I disagree, it makes it much harder to steal my identity,
without me needing to think about it.

For email I know its a good thing to sign,
spoofing email is incredibly easy and by signing it,
spoofing my email becomes difficult.
Assuming the recipient checks it, which most people don't,
but if nobody sets this up nobody will ever start using it.
So I setup email signing more as a matter of principle I guess.
Oh and btw, if everyone actually did this,
spam would've not been a problem at all.
So shame on you if you don't do this.

I also setup a VPN on the server with [openvpn](https://openvpn.net/),
mainly in case I want to go to china again,
I'll just have that around.
It allows you to visit google there.
The VPN client is basically a use flag for network manager.

Using a VPN by default won't help you with security much.
Maybe if you use not encrypted WIFI, but even then if you visit https websites
the connection is encrypted anyway.

# Other random stuff
I don't know what to say about most of these.

+ [Hostblock](https://github.com/cgag/hostblock)
  for disabling reddit, youtube and facebook while working on stuff.
  This is a huge productivity boost
+ [Firefox](https://www.mozilla.org/en-US/firefox/new/?scene=2) for browsing.
  I think google has enough opurtinity to spy upon me.
  They won't get my browser.
+ [gnome terminal](https://help.gnome.org/users/gnome-terminal/stable/)
  as terminal emulator, wait I have a reason for this.
  It'll rewrap text on screenresizing. [Konsole](https://konsole.kde.org/)
  won't do that, but konsole renders
  [firacode](https://github.com/tonsky/FiraCode) litagures correctly.
+ [You complete me](https://github.com/Valloric/YouCompleteMe) to give both
  Spacemacs and vim better contextual awareness for various languages
+ [org mode](http://orgmode.org/) to write any document in.
+ [Fbreader](https://fbreader.org/) for reading ebooks.
+ [qpdf](http://qpdf.sourceforge.net/) for pdf files.
