TITLE: The Release Rodeo
DATE: 2023-05-31 20:14
CATEGORY: reflection
Tags: work, remote, programming, release
OPTIONS: toc:nil
Modified: 2023-08-19 12:38

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
The people in charge could now also decide *when* to release.
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
So, on one occasion, a client success person panicked, after 
a fancy file uploader behind a feature flag disappeared.
This happened because it was merged into another feature flag[^problem].
Now certain customers no longer had access to the file uploader
to which they had grown accustomed,
breaking their entire workflow.

[^problem]: This itself was also problematic.
            The issue was that designers began wanting to make changes (as is their job),
            but developers started asking questions about how to implement these changes,
            and realized it was easier to just merge features.

Note that in this setting, a customer is a large corporation with thousands of employees.
So if one of them becomes unhappy,
the startup could lose a lot.
Not just money but also reputation, which translates into future sales,
potentially threatening the survival of the startup.
Up until now, this hasn't happened;
all customers who've signed up have always been kept happy.

Anyway, long story short, since the feature was being used in production,
I strongly urged its release.
This was discussed internally,
and everyone agreed on releasing this file uploader feature.
*Everyone*. However, some requests were made, for example, there were redundant form elements.
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

I wouldn't be writing an angry blogpost if it was the only incident.
There is more.
He was right about the confusion that followed from leaving the button.
QA got confused, another dev got confused, I'm surprised more customers weren't complaining about this.
Although they likely just ended up feeling stupid and didn't complain, which is *worse*.
It seems impossible to delete that darn button, however.

Moving forward a month, a large client was preparing for a demo.
I started working on a "detection" feature which we needed to impress said client.
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
If clients see those numbers don't align with that new "detection" feature and our Elm apps,
they're going to freak out.
Because they're going to think our platform messed up their data somehow.
So I reply to his announcement, because I know he's well aware of this.

> I'm not sure if I'd release the "feature" because "our Elm tables" are kind of incoherent right now. But if you're happy with it 🤷‍♂️

Here I redacted the names of the specific features,
replacing them by "feature" and "our Elm tables".
Now, is this blunt? Yes, but there is *a lot of money on the line*.
The same manager who blamed the developers for the missing button incident
was also responsible for this release.
Naturally, he employed his remarkable management skills
and decided to throw a tantrum, persuading the CTO to scold me.
Indeed, I was reprimanded and praised simultaneously.
It was beneficial that we detected this issue early,
but I was told that my communication was too blunt and insufficiently precise.
I believe there's truth in this.
I could've been calmer, but maintaining composure is challenging in a stressful situation.
Part of this blog post is about circumventing such situations in the future.
The manager could've performed better too.
For instance, he could've inquired in #tech before releasing,
"Are there any serious issues remaining with feature X, Y, or Z?"
Just like one would do in a team.
Alternatively, he could've engaged with my response in #general to figure out what was happening. 

I was angry when starting to write this.
And while I wanted to blame a particular individual,
I won't.
This is the wrong mindset. [^dharma-samsara]
Rather than blaming specific individuals, I prefer to think in terms of systems.
How is our process so broken and causing stress?
Well, no one has cared about thinking about how all of this should work internally, at least.
This is so bad we don't even have a proper definition of what it means to release
a feature:

1. We delete the feature flag and old code.
2. We enable the feature flag for all customers as a setting.

Point `1.` should happen eventually,
but after reading the external communication process, I think we should first do `2.`,
wait for some time, and then delete the feature flag.
It'd be nice if we put a limit on that as well, so developers can safely delete at some point.
Even though there appears to be documentation on what clients need to be informed,
internally we have no clue who to tell what or get involved.
We don't have a grace period either,
allowing people to comment on a release.
For example, the customer success people can just say if a button is missing in this grace period,
or a developer implementing it can just say that hey, we're showing incoherent stuff.
There is no guidance on how big a release should be,
are smaller ones better or bigger ones.
It seems like the customer success and marketing people prefer big releases.
So they can use it as a marketing moment,
and have an easier time demoing more features in one go.
But I suspect for engineers, smaller ones are preferred,
as this allows them not to be overwhelmed if issues arise.

How would you release?
I've always learned to release early and release often.
But ever since the introduction of feature flags,
we've just not been releasing,
until it became a problem.
In typical startup style sledgehammer fashion,
the problem was solved; release a bunch of stuff,
and for the unfinished parts we peeled them off, putting them behind different flags.
But does the path towards releasing this software have to be so stressful?
I feel we're doing it wrong.
The lack of some kind of formalized strategy causes some issues here, I think.
We're just doing stuff, inconsistently, and coordination revolves around people being shocked and in a panic.
This clearly isn't working.

Documenting what I believe should happen,
without naming specific individuals or organizations, [^could-be-anyone]
has at least alleviated my anger.
I'm not sure if the procedures I've outlined constitute a good process,
hence I welcome comments,
but I believe it's a step forward from doing things haphazardly.
Getting a timeline for a release at the very least would be beneficial.
Therefore, with my anger tempered and a sense of progress, I deem this a successful blog post 💪.
Please share your thoughts in the comments.

Note that this post has a [follow up]({filename}/followup-releasing.md).


[^could-be-anyone]: Could be anyone really, since I do work freelance :)

[^dharma-samsara]: Growing deaf to dharma and being trapped in samsara.
