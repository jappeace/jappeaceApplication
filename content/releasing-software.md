TITLE: The Release Rodeo
DATE: 2023-06-09 18:17
CATEGORY: reflection
Tags: work, remote
OPTIONS: toc:nil
Status: draft

We started off with feature flags.
These were initially introduced as a way to pace development.
No longer did changes need to be done in one go and have to be correct immediately,
but QA could test a feature behind a feature flag while a part was being developed.
This also allowed for more smaller PRs,
making reviews much easier.

*However*, the flag count grew,
and salespeople started to realize they could enable flags for certain customers.
This is all well and good,
except for the fact that these features are still in development.
So, on one occasion a client success person got upset after her feature flag
(a fancy file uploader) had disappeared because it got merged into another feature
(a gigantic redesign which was swallowing a bunch of other features).
This effectively meant certain customers no longer had access to the file uploader they had grown accustomed to.
And also that their entire workflow was now broken,
since this is how they had learned to use the product.

Note that in this setting, a customer is a large corporation with 1000's of employees.
So if one of them gets unhappy,
the startup I worked for could lose a lot,
not just money but also reputation (which translates into future sales),
and it could potentially kill the startup.
Up until now this hasn't happened,
all customers who've signed have always been kept happy.

Anyway, long story short, since the feature was being used in production,
I strongly urged its release.
This was discussed internally,
and everyone agreed on releasing this file uploader feature.
*Everyone*. However, some requests were made, for example, there were some redundant form elements.
Deleting this had some consequences for the rest of the code, however, due to how the internal application was structured, it required removing this field also from the model[^model].
Actually deleting this form element was quite a bit of work because of that.
So it took one of the devs a while to ready the PR to release the feature.
The manager suggested also deleting the old file uploading button?
This was a pretty good idea,
because with this change the old flow would be quite broken;
we had already removed error messages.
The manager in charge got tagged and ignored the PR for several days,
after which we decided to merge.
We had already decided to release this feature,
so his approval wasn't necessary according to our customs.

Fast forward to the next week,
and our dear client success person was in panic once more.
It turns out that one of our older customers was still using that button.
So we had to put it back.
I warned them that this old flow was broken, missed a lot of stuff and would cause confusion.
They just wanted it back however, so I complied.
I also added back the error messages which were also gone for some reason (based on my own testing).
This was a rather minor change,
however our dear manager in charge,
did what any good leader would do and blamed the devs.
*Even though this was his own suggestion*.

I wouldn't be writing this if it was the only incident.
There is more.
He was right on the confusion that followed from leaving the button.
QA got confused, another dev got confused, I'm surprised no more customers were complaining about this.
Although they likely just ended up feeling stupid and didn't complain, which is *worse*.
It seems impossible to delete that darn button, however.

Moving forward a month, a large client was prepping for a demo.
I moved onto this detection feature which we needed to impress said client.
We had a meeting to figure out what we could get done before some demo.
From this we learned we had to modify the elm apps.
These Elm apps represent HTML tables that open up a modal with more tables.
Why this is implemented in Elm is beyond me, there is no interactivity at all.
Well, I know why this was implemented in Elm.
The developers who wrote it were more familiar with Elm,
and they had deadlines, so they chose what they knew.
It's like eight different tables, all looking slightly different,
all piped into the same Elm app with flags specifying how it should look.
We've had production bugs with these apps.
They just stop functioning and you've no idea why.
Furthermore, one of the best features of Elm,
its debugger, is unusable because they overlap, since there are several Elm apps displayed on the page.
As you can tell I'm not to happy having to modify these.

Anyway, the demo went well, as they skirted around those Elm apps and didn't open the modals,
which show the wrong information our new feature detects.
Now, 2 weeks later, the manager decides it's a good idea to release this feature.
But I've not finished yet, since I was away for 1 week on vacation, (also there was an offsite).
But the way I learn about this intent is through an announcement in #general.
I'm kind of shocked.
If clients see those numbers don't align with that new feature and our Elm apps, they're gonna freak out.
Because they're gonna think our platform messed up their data somehow.
So I reply to his announcement, because I know he's well aware of this.
> Not sure if I'd release the "feature" because "our Elm tables" are kind of incoherent right now. But if you're happy with it 🤷‍♂️
Here I named the specific features interacting instead of "feature" and "our Elm tables", which is redacted for the sake of anonymity.
Now is this blunt? Yes, but there is *a lot of money on the line*.

The same manager who blamed the dev was responsible for this release,
used his amazing managing talents,
and decided to throw a tantrum and get the CTO to scold me.
Yes, I was scolded, and told I had done a good thing at the same time by the CTO?
And we decided to disable the links in those Elm apps, preventing clients from seeing wrong information.
You know, I got told my communication was too blunt and not precise enough,
but it once again puts all the blame on developers,
even though our beloved manager could've asked in #tech before releasing,
"Are there any serious issues left with feature X or Y or Z?"
I just assume he didn't have enough time to actually figure out this himself because our dear management talent is actually legitimately quite busy.
Like you would do in a team.
Or he could've engaged me in my #general response, figure out what's going on.
Perhaps this is the hidden trick to becoming a manager,
just constantly blame others for your own mistakes.
Never reflect, always project.

How would you release?
I've always learned to release early and release often.
But ever since the introduction of feature flags,
we've just not been releasing, until it became a problem.
In typical startup style sledgehammer fashion,
the problem was solved, release a bunch of stuff,
and for the unfinished parts we peeled them off putting them behind different flags.
But does the path towards releasing this software have to be so stressful?
I feel we're doing it wrong.
The lack of some kind of formalized strategy causes some issues here I think.
We're just doing stuff, inconsistently, and coordination revolves around people being shocked and in panic.
This clearly isn't working.

Here I was hoping to arrive at some golden solution, perhaps send a particular manager to some course for release engineering.
However, once I googled them they seemed to be focused on learning about technology, but we don't have a tech problem.
Our problem is entirely focused on the process, how people interact.
I hope it's clear from the context that I think blaming the devs isn't the right solution.
They'll just leave after a while.

In this article, I was particularly harsh on this one manager, but I've to say he's not all bad.
He has good ideas.
And I'm sure he's got some stuff to say about me.
But, this is *my* blog, and I'll write what *I* want to write about.
I've kept this intentionally anonymous because people can grow and organizations can change.
In fact, I hope people will change after reading my humble perspective.

[^model]: A model being a piece of abstract representation of the world, which is directly represented into database tables for example. Changing this requires a lot of updating of the code, wherever it used. Pretty much all web application have either expicit or implicit representations of models.

