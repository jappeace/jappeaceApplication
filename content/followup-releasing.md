TITLE: A lesson from Anger
DATE: 2023-07-17
CATEGORY: reflection
Tags: work
OPTIONS: toc:nil

This is a follow-up on the [release rodeo]({filename}/releasing-software.md).

After I wrote that blog post, the CTO found it quickly by coincidence.
He was quite okay with everything I had written, he said.
I reminded him that I had also put in the effort of anonymizing everything.
Probably having forgotten the effort I went through to anonymize everything, he really loved that!

The thing he took most issue with was my reference to his critique of my bluntness as "scolding".
I suppose he didn't exactly scold me.
I described it that way because it aligns with my personality: I'm a harsh person,
I have high standards, and I take criticism extremely seriously.
Even when someone says, "You're kind of doing this badly sometimes,"
I interpret it as "You suck, Jappie, you need to do better".[^psyche]
He also warned me that if the PM were ever to stumble upon this post, they might get quite upset.
I am aware of this possibility, which is why I didn't advertise this article.

A few days later, another engineer found themselves tangled in the release flag situation.
At this point, I realized it was crucial to inform everyone about the proper procedures.
Therefore, I set out to describe the current situation and propose some recommendations.
After outlining the recommendations, I realized the points of contention weren't numerous.
The process worked in principle (aside from some peculiar naming conventions),
and I believe that most of the involved parties were simply uninformed.
By tagging them on the GitHub RFC[^rfc] I created, I enticed them to read it and become informed.
Because this was an open discussion, this also helped get clarification on points I had missed.
Since then, we've had no more release issues.
It appears that writing down the process and informing everyone resolved the issue.

A lesson learned from this experience is that anger can be rather productive.
If there's something you can do about the situation,
it's productive to think about why you're angry and devise a plan of action.
In this case, I indeed solved a rather annoying but important issue by taking a relatively simple action.
Anyone could've done this.
But we've also learned that indeed, in the moment,
you should be careful, or you may say something imprudent,
creating pointless and distracting strife in the process.

[^psyche]: From this, we can also derive the reason for my bluntness when angry. For a mind that is harsh on itself by default, being nice requires extra effort.
[^rfc]: Request for change, eg an invitation to discuss a change in process or system.
