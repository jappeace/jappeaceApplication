TITLE: Making money with foss
OPTIONS: toc:nil
DATE: 2017-05-22
Modified: 2019-03-10 14:33
CATEGORY: technique
Tags: free, money, software, programming

In this blog post I will discuss how to make money with open source software.
Why do you care?
Making money of this model is hard,
yet you got to pay the bills.
Even though the free software variant is [consumer friendly](https://www.gnu.org/philosophy/free-sw.en.html).
(note that these consumers maybe businesses too).
This is a big problem.
Lot's of projects are maintained in people's unpaid free time,
next to their day jobs.
So they don't really get the love they deserve.
Can we fix this?

I would like to do this myself full time.
Lest I became [financially independent](https://www.reddit.com/r/financialindependence/), I couldn't.
So in here I will list the options I found trough researching the subject.
As both an overview to myself and to the interested reader.

# Google results

In my naive approach I searched for ["Making money with foss"](https://www.google.nl/search?q=making+money+with+foss&ie=utf-8&oe=utf-8&client=firefox-b&gfe_rd=cr&ei=50YjWYn_CdHU8geKob64BQ).
The first site that came up was [info world](http://www.infoworld.com/article/2612393/open-source-software/greed-is-good--9-open-source-secrets-to-making-money.html).
This site was full of warning flags,
for example having a
click bait title and using a pagnation.
After skimming the article it wasn't about making money of an open source project,
but potential reasons for open sourcing (part of) your code.
It was aimed at mid to high level management,
non technical texts are of no interest to me,
mainly because they usually have an agenda.

A [second article](http://www.fosslc.org/drupal/node/131) linked to [another page](http://carlodaffara.conecta.it/?p=90&cpage=1#comment-50)
with a better list, but I couldn't find it.
The third [article](http://www.cio.com/article/3178621/open-source-tools/how-to-make-money-from-open-source-software.html) was just a summery of a video and wasn't very focused.
Okay so my search terms were bad.
Time to be more specific.

I searched for ["foss business models"](https://www.google.nl/search?q=making+money+with+foss&ie=utf-8&oe=utf-8&client=firefox-b&gfe_rd=cr&ei=50YjWYn_CdHU8geKob64BQ#q=foss+business+models), you know you did something right when
scholarly articles start being presented.
There was an entire [Wikipedia page](https://en.wikipedia.org/wiki/Business_models_for_open-source_software) on the subject.
I guess more people are really interested in this.
So does this blog post die in the cradle?
I say no, I can still give my opinions on that page.
Another thing is that discussing this page on my blog will force me to closely
look at the possibilities.
Besides the fact that there is a (quite big) wiki on it only shows how much
people care about this subject.

# The Business models

The core problem open source software faces is not having a monopoly on
distribution that copyright provides.
Therefore we can't sell it directly:
Anyone can start distributing the software without asking anything for it,
without running a loss because distribution is
(practically) free for [software on the internet](https://stratechery.com/2015/aggregation-theory/).
Giving up this monopoly is a necessary requirement for open source,
called the [second and thirth freedoms](https://www.gnu.org/philosophy/free-sw.en.html).
This is also done with the BSD like licenses,
so we can consider it a part of open source.
Therefore we need to find a way of making money by working around this
restriction.

## Sell stuff besides software

One of the primary strategies is selling stuff besides the software.
Red Hat does this trough [providing support contracts](https://en.wikipedia.org/wiki/Red_Hat#Business_model) for example.
This can be extended by providing certificates of expertise
(which Red Hat also does).

I don't like this approach as it gives the incentive to make bad software.
If your software is hard to use, then only you can sell support.
You should make your software easy to use.

Another strategy is selling of merchandise, such as fan [T-shirts or coffee cups](https://store.wikimedia.org/collections/accessories).
Which the wikimedia foundation does.
Selling merchandise is quite clever because its much more easy for someone to
buy 'something', rather than just donating.
Besides its free advertisement for your project,
*and* your users now have something to identify themselves with.
So I think if you have an opensource project you should seriously consider doing this.

Finally a programmer can rent himself out to a project to add features or do
bug fixes.
These include [Bountysource](https://www.Bountysource.com/) or [kickstarter](https://en.wikipedia.org/wiki/Kickstarter), where Bountysource is
usually fix an issue first and then get paid whereas kickstarter works on
promises.
In essence you're offering up your own time to work on a project.
In both cases there is of course the persistent issue that its [hard to decide](https://softwareengineering.stackexchange.com/questions/648/how-to-respond-when-you-are-asked-for-an-estimate)
how much time this will take.
With kickstarter however you can estimate it better because you're probably
starting your own bigger project, so the overhead of estimation is smaller.
With Bountysource each bounty requires its own estimate and these are often
smaller, think of about \\$100, its hard to decide to even bother with that.
Another issue with Bountysource is that you have to switch from project to
project often, which requires a lot of getting familiar with the code base
overhead.
The model of Bountysource is less then perfect to live off.
Although for existing developers of a project its a great motivation to work a
little more.
But don't expect strangers to 'join in', just because of the bounties
unless they're highly valued.
In fact you can see that few people 'live' off Bountysource just by the
overview of bounty hunters. Only three people at the time of writing got over
\\$2000 in the last 90 days and only 9 over \\$1000.

In the strategy of renting yourself out is going to interested parties yourself
and offer to write software.
I've attempted this at times with for example [offertex](https://github.com/jappeace/offertex) and [schijt je rijk](https://github.com/jappeace/schijt-je-rijk).
The issue that always gets me is negotiating about price.
And of course once the work is finished, you need to find *more* work.
Therefore you won't get better overtime.
Imitating red hat with support contracts could do that,
but starting with that on your own is hard.
Although I had other factors working against me,
living in a remote village didn't help.
So you mileage may vary.

## Donations
Another powerful trick of trying to become self sustained on software is using
donations.
The big issue with this is of course getting people to donate in the first place.
A famous example of a donation scheme as that of [mozzilla firefox 1.0 release](http://www-archive.mozilla.org/press/mozilla-2004-12-15.html).

Recently another option has popped up in the form of [patreon](https://www.patreon.com/).
The idea is pretty simple, subscribe to a creator and every time he puts
something out (or every month) donate a small amount decided by you.
[Some](https://www.patreon.com/landley)
[developers](https://www.patreon.com/kozec)
[have](https://www.patreon.com/bcachefs)
[embraced](https://www.patreon.com/pippin)
[this idea](https://www.reddit.com/r/linux/comments/5omtvg/patreons_to_support_open_source_projects_please/),
although I haven't seen anyone that earns self sustaining amounts.
It does provide a reliable income stream and is aimed at *individuals*,
and since there are [other](https://www.patreon.com/cgpgrey) [creators](https://www.patreon.com/avasdemon)
who have managed to get to a sustainable level.
We can expect this to happen eventually for software developers too.

The big advantage patreon offers over Bountysource is that,
rather than having to think about how much time creating a feature costs as
with Bountysource,
you can just continue improving the project how you think it should be done.
Of course some trust in the developer is necessary for that,
Bountysource doesn't have that problem, since payment is done afterwards and I
assume regulated by Bountysource.
However apparently Bountysource offers a similar service [for projects](https://salt.bountysource.com/teams/neovim).

Another approach to this is to start a foundation.
Blender is developed like [this](https://www.blender.org/foundation/),
and if your project has any kind of traction,
do consider this.
Trusting a foundation is much more easy because
they're regulated by law.

## Advertisements

A trick often overlooked by most developers is advertising.
In principle free software is not against the idea of advertising.
However a problem with this is that anyone can take your software,
remove the advertisements, and redistribute the add free version.
You can prevent this from happening by offering two versions,
one with adds and one without and then ask your users to support the project
by downloading the one with adds.

[Addblock plus](https://en.wikipedia.org/wiki/Adblock_Plus#Controversy_over_ad_filtering_and_ad_whitelisting)
famously white listed adds as a way of generating revenue.
It may be considered hypocritical,
but remember that as free software anyone can fork it and remove this feature.
Not that you have to since there are already [alternatives](https://github.com/gorhill/uBlock).
Note that although Adblock Plus probably made quite a good load of money
with their white listing program,
it will probably kill the project eventually.
Because now they're essentially running an extortion racket,
becoming nothing more than parasites and providing lower quality user experience.
I imagine the people who go out of their way to install an ad blocker are not
the kind of people who are satisfied with an ever laxer white list.

## Crypto Currency
There is an opportunity in crypto too.
Although this immediately sounds fishy.
I'd say that taking a couple cycles per minute to do some
mining isn't *that* bad.
I mean how is it any different from stealing a users
attention with advertisements?
It's probably even better because the users' time is more valuable
than machine cycles.
No wonder that google [cracked down](https://www.theregister.co.uk/2018/03/14/google_cracks_down_on_crypto_with_ad_ban_from_june/)
on this.
Crypto is a substitute for advertising, 
Google is essentially a massive ad company.
But this has the same problem as advertisment in that forks will
occur which would take it out.

## License tricks
The final category for making money with open source is license trickery.
So this comes from the idea that, if you are the sole copyright holder,
you can put the software under various licenses.
Note that it is a big if to be the sole copyright holder,
you need consent that any contributor is handing over their copyright.
And as we will see in the re-license case, this can be abused.
Which increases contributing barrier significantly.
But in return as project owner you get a lot more possibilities for making money.

Dual licenses are a practice where you offer one open license,
and another business aimed license which promises more support than the open
license or removes restrictions (such as forcing open source).
This is [where AGPLV3 shines](http://lucumr.pocoo.org/2013/7/23/licensing/#the-stricter-gpl), original authors can offer large organizations an
alternative license, however downstream receivers of the code under AGPL cannot
do this.
[Mongodb](https://en.wikipedia.org/wiki/MongoDB) is an example that does this and they can only do this by
asking contributors to [hand over copyright](https://www.mongodb.com/legal/contributor-agreement).
As a contributor you should be wary of doing this,
copy left doesn't work if you hand over copyright.
If you work under BSD or MIT kind of licenses it doesn't matter
(and I assume you already came to terms with this).

However, this does allow a company to thrive upon open source.
A company is still required to hoard in the business deals with other companies,
and to collect the copyright assignments from contributors.
It may be taken over similarly as Oracle did to Sun.
But the AGPL based code was already under license,
so the community can step in and take over development,
as happened with Illumos (Solaris fork) after Oracle went on its rampage.
Although a recent [article](https://stratechery.com/2019/aws-mongodb-and-the-economic-realities-of-open-source/)
by Ben Thompson cast some doubt on the long term efficacy of this strategy.

This has however a darker side in potential license trolling.
Which [Oracle (who else) did](https://lists.debian.org/debian-legal/2013/07/msg00000.html) for example with a database.
Changing from BSD to AGPLv3, which in case of Debian required around 100 other
dependent packages to change to AGPLv3 too.
Which of course is not going to happen.
Oracle probably did this to force users of that database to take a commercial
license instead, taking foss projects (such as Debian) as collateral damage.

## Proprietary extensions

Proprietary extensions involve releasing an open source core and add
(usually business centred) proprietary extensions.
The Wikipedia page lists several example but the one I'm personally
familiar with is the IntelliJ project.
Which is [opensource](https://github.com/JetBrains/intellij-community), has a [proprietary paid extension](https://www.jetbrains.com/idea/features/editions_comparison_matrix.html), and also a [cla](http://www.jetbrains.org/display/IJOS/Contributor+Agreement)
(which doesn't hand over copyright but does a similar thing,
licenses a right to copy).

IntelliJ is kind off open source, but many developers want to pay for things
such as CSS or JavaScript support.
Although I'd say any text editor can do that, such as [spacemacs](http://spacemacs.org/).
where IntelliJ shines is Java and Scala.
As far as I can see are the 'supported' features, just bells and whistles.
However they maybe valuable for a professional developer, or to a software house
to which the license fees are nothing compared to developer time.

Because IntelliJ is open source it allowed Google to create android studio.
This is great for the IntelliJ team because now there is another party
that is dependent on their core of which they hold all copyright.
Google may help developing the IntelliJ Java core, just to get it
to work for android developers.
This is the thing most companies are after with open source,
free programming manpower.

## Delayed open sourcing

This is the thing [John Carmack](https://en.wikipedia.org/wiki/John_Carmack) famously did with ID tech.
After some time selling the games he would release the source of the games.
Which he did for [Doom](https://github.com/id-Software/DOOM), [Quacke](https://github.com/id-Software/Quake) even [Doom 3](https://github.com/id-Software/DOOM-3).
This in turn led the games to be developed upon for a long time after their
release.
For example [ioquake3](https://ioquake3.org/), still actively develops the quake engine,
driving sales of the quake game itself because the assets aren't freely
available.

So he open sourced partly dated tech, because he was confident he could do better now.
And he increased sales of already released games,
because people wanted to hack on it.
I think this is pretty genius, gaining some extra juice out of old projects.
The really nice thing is that the lifespan of these games has tremendously increased.
Creating a core of die-hard followers.
Doom has been ported to essentially [everything](https://en.wikipedia.org/wiki/List_of_Doom_source_ports).
It's a mystery to me why not more companies are doing this.

## Re-license
If you are the sole copyright holder, you can stop distributing under the
open source license and re-license it.
Originally I didn't want to include this option because you're no longer
doing foss at this point.
But if your project is dying, because you can't give it enough love.
This maybe totally valid.
It's your project after all,
if you can make a valid business out of it by stop doing opensource
I'd give you nothing but praise.

Having said that,
this also opens up the opportunity to hate upon
Oracle. So lets hate upon Oracle.

So if we Google: [Why oracle is horrible](https://www.google.nl/search?q=why+oracle+is+horrible&ie=utf-8&oe=utf-8&client=firefox-b&gfe_rd=cr&ei=Hi0sWcTQNOvGXqT5o7gM), we can get some [dumb](https://www.quora.com/Whats-so-bad-about-Oracle) [quora](https://www.quora.com/Why-do-some-people-hate-Oracle) answers.
These are just not the point.
This [reddit thread](https://www.reddit.com/r/linux/comments/2e2c1o/what_do_we_hate_oracle_for/), sums it up nicely.
What is really dog kicking evil were the Solaris issues, which is discussed in
this [this video](https://www.youtube.com/watch?v=-zRN7XLCRhc#t=33m0s).
A little further in [the video](https://www.youtube.com/watch?v=-zRN7XLCRhc&feature=youtu.be&t=2482) its is explained how it happened.
So what happened is that Oracle obtained all copyright from various authors by
buying SUN which required initially handing over copyright,
Open Solaris was closed by Oracle with a re-license.
This was only possible because Sun asked contributors to fork over copyright.
What we can learn from this is that if you contribute to free software and care
about it, **never hand over copyright**.
I'm happy to say however that a fork of Solaris occurred called [Illumos](https://wiki.illumos.org/display/illumos/illumos+Home) that
seems to still be active.

# In conclusion
Because we are interested in making money,
this post will took us all over the place.
On the one hand we have the greedy businesses,
and on the other side the diligent developer.
Licenses were never discussed in university,
which is interesting because this is *the* method for making
money with software.
I think having discussed the overview and shown some concrete examples was a
good exercise.
I was not aware at all for example of the AGPLv3 practices which
are interesting (without passing moral judgment).
