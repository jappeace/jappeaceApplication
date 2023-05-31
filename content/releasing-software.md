TITLE: The Release Rodeo
DATE: 2023-06-09 18:17
CATEGORY: reflection
Tags: work, remote
OPTIONS: toc:nil

<style>
img[src="/images/2023/release-rodeo.png"]{
  height: 20em;
}
</style>

![Release rodeo~](/images/2023/release-rodeo.png)

In the beginning, there was nothing.
We merged completed features after a month or so of development,
leading to large PRs and often unexpected implementations.
Then, feature flags were introduced.
This allowed the merging of code in chunks,
enabling QA to test thoroughly.
Furthermore, it allowed the people in charge to decide *when* to release.
Because this also facilitated more, smaller PRs[^PR],
it also made the reviewing process much easier.
Everything was awesome in paradise,
but not all was as it seemed...

[^PR]: Pull requests, the primary mechanism in which developers align their changes.
       It opens up a moment for questions or comments.

The flag count grew,
and salespeople started to realize they could enable flags for certain customers.
This went well, in the beginning.
Except that these features were still in development.
So, on one occasion, a client success person got upset after her feature flag,
a fancy file uploader,
had disappeared because it was merged into another feature flag[^problem].
This effectively meant certain customers no longer had access to the file uploader
to which they had grown accustomed.
Also, their entire workflow was now broken,
since this was how they had learned to use the product.

[^problem]: This itself was also problematic.
            The issue was that designers began wanting to make changes (as is their job),
            but developers started asking questions about how to implement these changes,
            and realized it was easier to just merge features.

Note that in this setting, a customer is a large corporation with thousands of employees.
So if one of them becomes unhappy,
the startup I worked for could lose a lot.
Not just money but also reputation, which translates into future sales,
potentially threatening the survival of the startup.
Up until now, this hasn't happened;
all customers who've signed up have always been kept happy.

Anyway, long story short, since the feature was being used in production,
I strongly urged its release.
This was discussed internally,
and everyone agreed on releasing this file uploader feature.
*Everyone*. However, some requests were made, for example, there were some redundant form elements.
Deleting these had consequences; due to how the internal application was structured,
it required removing this field also from the model[^model].
Because of that, deleting this form element was quite a bit of work.
So it took one of the developers a while to finish the PR for the release of the feature.
The manager suggested also deleting the old file uploading button.
This was a good idea,
because with this change the old flow would be quite broken;
we had already removed error messages.
The manager in charge was tagged and ignored the PR for several days,
after which we decided to merge.
We had already decided to release this feature,
so his approval wasn't necessary according to our customs.

Fast forward to the next week,
and our dear client success person was once again in a panic.
It turns out that one of our older customers was still using that confusing button.
So we had to put it back.
I warned them that this old flow was broken,
lacked a lot of elements and would cause confusion.
They just wanted it back however, so I complied.
This was a rather minor change,
however the manager in charge of releases,
did what any good leader would do and blamed the developers.
*Even though this was his own suggestion*.

I wouldn't be writing this if it was the only incident.
There is more.
He was right about the confusion that followed from leaving the button.
QA got confused, another dev got confused, I'm surprised more customers weren't complaining about this.
Although they likely just ended up feeling stupid and didn't complain, which is *worse*.
It seems impossible to delete that darn button, however.

Moving forward a month, a large client was preparing for a demo.
I moved onto this detection feature which we needed to impress said client.
We had a meeting to figure out what we could get done before the demo.
From this we learned we had to modify the Elm apps.
These Elm apps represent HTML tables that open up a modal with the specific data comprising those tables.
It's like eight different tables, all looking slightly different,
all piped into the same Elm app with flags specifying how it should look.
We've had production bugs with these apps.
They just stop functioning and you've no idea why.
Furthermore, one of the best features of Elm,
its debugger, is unusable because they overlap,
since there are several Elm apps displayed on the page.
As you can tell, I'm not too happy having to modify these.
It's a lot of pointless complexity.

Anyway, the demo went well,
as they skirted around those Elm apps and didn't open the modals,
which show the wrong information our new feature detects.
Now, 2 weeks later, the manager decides it's a good idea to release this feature.
I learned this through an announcement in #general.
But I wasn't finished yet, since I was away for 1 week on vacation.
I'm kind of shocked.
If clients see those numbers don't align with that new feature and our Elm apps,
they're going to freak out.
Because they're going to think our platform messed up their data somehow.
So I reply to his announcement, because I know he's well aware of this.

> I'm not sure if I'd release the "feature" because "our Elm tables" are kind of incoherent right now. But if you're happy with it 🤷‍♂️

Here I named the specific features interacting instead of "feature" and "our Elm tables", which is redacted for the sake of anonymity.
Now, is this blunt? Yes, but there is *a lot of money on the line*.

The same manager who blamed the developers in the missing button incident,
was also responsible for this release.
Naturally, he used his amazing management talents,
and decided to throw a tantrum and get the CTO to scold me.
Yes, I was scolded, and told I had done a good thing at the same time by the CTO.
I got told my communication was too blunt and not precise enough,
but it once again puts all the blame on developers,
even though our beloved manager could've asked in #tech before releasing,
"Are there any serious issues left with feature X or Y or Z?"
Like you would do in a team.
Or he could've engaged me in my #general response, to figure out what's going on. [^tricks]

[^tricks]: Perhaps this is the hidden trick to becoming a manager,
           just constantly blame others for your own mistakes.
           Never reflect, always project.
           Not that I'm surprised this manager in particular acts this way.
           He had an amazing teacher of incompetence.

How would you release?
I've always learned to release early and release often.
But ever since the introduction of feature flags,
we've just not been releasing, until it became a problem.
In typical startup style sledgehammer fashion,
the problem was solved; release a bunch of stuff,
and for the unfinished parts we peeled them off, putting them behind different flags.
But does the path towards releasing this software have to be so stressful?
I feel we're doing it wrong.
The lack of some kind of formalized strategy causes some issues here, I think.
We're just doing stuff, inconsistently, and coordination revolves around people being shocked and in a panic.
This clearly isn't working.

[^model]: A model is an abstract representation of the world, which is directly represented into database tables, for example. Changing this requires a lot of updating of the code, wherever it's used. Pretty much all web applications have either explicit or implicit representations of models.
