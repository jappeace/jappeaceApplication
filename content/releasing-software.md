TITLE: Releasing software
DATE: 2023-06-09 18:17
CATEGORY: reflection
Tags: work, remote
OPTIONS: toc:nil
Status: draft

I'm rather frustrated at a certain organizaiton,
regarding their ineptitude regarding releases.
Perhaps my frustration would be less if I would write down
my concerns about process.

We started off with feature flags.
They were initially introduced as a way to pace development.
No longer needed changes be done in one go and be correct immediatly,
but QA could test a feature behind a feature flag while part was being 
developed.
This also allowed more smaller PR's,
making reviews much easier.

*However*, the flag count grew, and sales people started to realize
they could enable flags for certain customers.
This is all good, were it not for the fact that these features
are still in development.
So, on one occasion a frantic client success manager got upset
after her feature flag (a fancy file uploader)
had dissapeared because it got merged into another feature
(a gigantic redesign which was swallowing a bunch of other features).
This effectivally meaned certain ccustomers had no longer access
to the file uploader they grew accustomed to.
And also that their entire flow of working was now broken,
since this is how they learned how to use the product.

Note that in this setting, a customer is a large coorporation with
1000's of employees.
So if one of them gets unhappy, this particular tiny startup
would lose a lot, not just money also in reputation
(which is future sales),
and potentially kill it.
Up till now this hasn't happened,
all customers who've signed have always been kept happy.

Anyway, a long story short,
since the feature was being used in production.
I strongly urged releasing this.
Internally this was discussed,
and everyone agreed upon releasing this file uploader feature.
*Everyone*.
However some requests were made,
for example there were some redundant form elements.
Deleting this had some consequences of the rest of the code
however, because of how the internal application was structured,
it required removing this field also from the model[^model].
Actually deleting this form element was quite a bit of work because of that.
So it took one of the devs a while to ready the PR to release the feature,
The manager suggested to also delete the old file uploading button?
This was a pretty good idea, because with this change the old flow would
be quite broken, we already removed error messages.
The manager in charge got tagged,
and ignored the PR for several days,
after which we decided to merge.
We already had decided to release this feature,
so his approval wasn't necessary according to our customs.

Fast forwared to the next week, and our dear client success manager
was in panic once more.
turns out that one of our older customer was still using that button.
So we had to put it back,
I warned them that this old flow is broken, misses a lot of stuff and would
confusion.
They just wanted it back however, so I complied.
I also added back the error messages which were also gone for some reason
(Based on my own testing).
This was a rather minor change, however our dear manager in charge,
did what any good leader would do.
And blame the devs.
*Even though this was his own suggestion*.

I wouldn't be writing this if it's was the only incident.
He was right on the confusion that followed from leaving the button,
QA got confused, another dev got confused, I'm surprised no more customers were complaining about this.
Although they likely just ended up feeling stupid and not complain, which is *worse*.
Seems impossible to delete that darn button however.

Moving forwared a month, dear a large client was prepping for trial.
I just had finished off several month effort of reducing tech debt,
which inovled moving a large unbounded task into a queue based approach,
so we can handle many more clients, which we'll need.
So I moved onto this detection feature which we needed to impress said client.
We had a meeting to figure out what we could get done before some demo.
In here, I warned the people involved I knew nothing about this,
and the amount of features wouldn't be possible to be completed within the alloted time.
So they got rescoped to something possible.
The other dev would implement 4, I would do 1. I ended up doing 4 and he did 1 :(
then again, he did a stellar job,
I mean the reason I could finish so many is because he made the code really well structured.
So I'm not salty.
It just shows how hard it is to estimate time, but we got everything done.
So, I had to cheat on that final task to finish it because it turned out to be
involving the much dreaded elm apps.
These elm apps represent html tables, that open up a modal with more tables.
Why this is implemented in elm is beyond me, there is no interactivity at all.
I guess it does some rendering of graphs as well.
well, I know why this was implemented in elm.
The developers who wrote it were more familiar with elm, and they had deadlines,
so they chose what the knew.
It's like 8 different tables, all looking slightly different,
all piped into the same elm app with flags specifying
how it should behave.
We've had production bugs with these apps.
they just stop functioning and you've no idea why.
Furthermore one of the best features of elm, it's debugger, is unusable because they
overlap, since there are several elm apps displayed on the page.
For tables, y'know the thing HTML could do since version 4.

Anyway the demo goes well, cuz they skoot around those elm apps, don't open the models
(which show the wrong information our new feature detects).
Now 2 weeks later the product manager decides it's a good idea to release this feature.
but I've not finished yet, since i was away for 1 week on vacation, (also there
was an offsite).
But the way I learn about this intent is trough an announcment in #general.
I'm kind off shocked, if clients see those numbers don't align they found
in that new feature with these elm apps, they're gonna freak out.
Because they're gonna think our platform fucked up their data somehow.
So I reply to his announcement, because I know he's well aware of this.
> Not sure if I'd release the "feature" because "our elm tables" are kinda inchoerent right now. but if you're happy with it 🤷
Here I named the specific features interacting instead of "feature" and "our elm tables",
which is retracted for sake of anononymity.
Now is this blunt? Yes, but remember, there is a *a lot of money on the line*.

The same manager who blamed the dev, was responsible for this release,
used his amazing managing talents, and decided to throw a tantrum and get the CTO to scold me.
Yes, I was scolded, and told I had done a good thing at the same time by the CTO?
And we decided to disable the links in those elm apps,
preventing clients from seeing wrong information.
Y'know I got told my communication was to blunt and not precise enough,
but it once again puts all the blame on developers,
even though our beloved manager could've asked in #tech before
releasing,
are there any serious issues left with feature X or Y or Z.
(I just assume he didn't have enough time to actually figure out this himself because our dear management talent is actually legitimately quite busy)
Like you would do in a team.
Or he could've engaged me in my #general response, figure out what's going on.
but no, this is why I am not good fit for management,
I lack talents for throwing tantrums or blame the devs.
I'm even friends with many developers.
I strongly encouraged for example one of the less confident developers to try
and remove that form element from earlier, I knew they could do it, it'd just be a lot of compile errors. I told them to reach me out if they can't figure out an issue, which they did, but with some advice they practically did it themselves.
A true manager would just try to make them feel like a small person I guess.

How would you release?
I've always learned release early release often.
But ever since the introduction of feature flags,
we've just not been releasing,
up till it became a problem.
In typical startup style sledgehammer fashion the problem was solved,
release a bunch of stuff,
and for the unfinished parts we peeled them off puting them behind different flags.
But does the path towards releasing this software have to be so
stressful?
I feel we're doing it wrong.
The lack of some kind off formalized strategy causes some issue here
I think.
We're just doing stuff, inconsistently,
and coordination revolves around people being shocked and in panic.
This clearly isn't working.

Here I was hoping to arrive at some golden solution,
perhaps send a particular manager at some course for release engineering.
However once I googled them they seemed to be focused on learning about
technology, but we don't have a tech problem.
Our problem is entirely focused on process, how /people/ interact.
I hope it's clear from context that I think blaming the devs isn't a right solution.
They'll just leave after a while.

In this article I was particularly harsh on this one manager,
but I've to say he's not all bad.
He has good ideas.
And I'm sure he got some stuff to say about me.
but, this is *my* blog.
and I'll write what *I* want to write about.
I've kept this intentionally anonymous because people can grow
and orginasations can change.
In fact,
I hope people will change after reading my humble perspective.


[^model]: A model being a piece of abstract representation of the world, which is directly represented into database tables for example. Changing this requires a lot of updating of the code, wherever it used. Pretty much all web application have either expicit or implicit representations of models.
