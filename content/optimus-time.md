TITLE: Optimus time!
DATE: 2017-01-29
CATEGORY: tools
Tags: gentoo, optimus, time, wasted, frustration

Using gentoo is a bliss most of the time.
The package manager portage is one of the most advanced managers that I've ever
used.
Last week I encountered how much better it is than for example apt, when my old
laptops power supply burned out and I had to resort to a xubuntu machine.
Apt has no capability for example to run multiple install processes at the
same time, whereas portage just goes with that.
Apt also has no propper versioning system, which makes python 2 and python 3
seperate packages, but portage can use slots for that.
This has the advantage that the search tool results become a lot less cluttered.

However using gentoo has also its disadvantages.
It can demand a lot of time once you start tweaking with it.
Yesterday I spent about 2 hours figuring out why my graphics card was slow.
3 hours trying to use the card in a dedicated manner (
rather than hot switching which the optimus technology does, promises to do, but
doesn't do).
Which failed because documentation is often to technical and its hard to debug,
because they sort of expect you know how to debug.
I ended up with the galium driver which upon kvm from vmware??
That didn't sound right, and indeed this was the software render.

Then I tried using the bumlbebee and primus driver from the main portage
tree and non masked primus variant instead of the bumblebee overlays ones.
This maked primus run work again. (it was borke for about one year, and I used
optirun -b primus instead which worked).
using the mainline gentoo tree also revealed that apperanlty primus requires
nvidia to have a use flag enabled.
Its the thing I don't like about useflags, how poorly they are documentated
I think the idea is that normally portage will tell you what use flags you need,
but not for edge cases (such as using an overlay).

So at this time I was being hope full again because I got some results,
therefore I did one last ditch attempt to try and use a dedicated one.
I rebooted and X11 ended up in a black screen with a nonworking keyboard.
Thanks X for taking con troll over everything (wayland still worked).

If your keyboard fails you're in big trouble,
X is sort of optional to fix things, but without a keyboard you can't even
access the backup tty's linux comes with (alt+{f1,f2,&#x2026;}).

So now I had to make a new live usb, because this new laptop has no cd player,
and I only had live CD's.
With the live [usb|cd] the plan is to fix your system so you can boot at least
into a tty.
So in my situation it just involved disabling the display manager
(which disables the keyboard).
In case of systemd you just remove the symlink
\`/etc/systemd/system/display-manager.service\`.
So after I did taht I spend over 4 hours figuring out what was wrong with the
display manager.
Apparently this new bumbleed clashes with X11, 
I used the \`/var/log/emerge.log\` to figure this out
(I kindoff forgot all the stuff I changed and for some reason journalctl
recorded nothing of sddm.
Also deleting the xorg.conf was a bad idea. it'll also produce a black screen.
However at this point I had about 5 different configuration of xorg so I just
coppied the working one in there.
I also found out that if you specify the wrong name in auto login of sddm
you will also get a black screen but with active keyboard so you can just
login to another tty to do more debugging.

So this time the great optimus technlogy (which I really start to hate), 
has taking 9 hours.
This is on top of the first time when I learned about bumblebee in my early
gentoo/steam experience (I guess another 8 hours at least).
And that one time optimus suddenly broke (perhaps 4 hours untill I found the
optirun -b PRIME trick, prime is required btw otherwise steam won't accept it).

I don't know, I'm salty at both optimus and gentoo to a lesser extend.
I mean all these problems were just configuration problems I would've
encountered too on other distro's I guess.
But gentoo makes it so easy to go out of your way and play with the
configurations.

Perhaps next time I should just go with an AMD laptop, I hear they are making
tons of progress with their opensource drivers.
