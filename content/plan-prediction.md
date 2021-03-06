TITLE: Plan prediction
DATE: 2017-12-21 12:00
CATEGORY: tools
Tags: statistics, machine-learning, AI, raster, data

For the raster project a main selling point will be the automatic prediction
of future scheduling.
There are two major schools of thoughts to go with that I know of.
Constraint satisfaction solving and data driven approaches (use statistics).

First of all the most straight forward approach is to use something like
[OptaPlanner](https://www.OptaPlanner.org/). 
In this approach constraints are laid upon the problem, such as 2 cooks
need to at least work on Saturday, but preferably 3.
This single constraint has two different part, the hard minimum, and the 'soft'
preference.
With these kind of rules in place, the OptaPlanner can use heuristics to figure
out who to plan when.
You could add as many constraints as you want, such as have less preference for
when employees asked free or let students not work during the day.

This is a very precise approach and almost surely will work,
as other systems have [demostrated](https://www.youtube.com/watch?v=sOWC4qrXxFk&index=5&list=PLJY69IMbAdq0uKPnjtWXZ2x7KE1eWg3ns) [already](https://github.com/kiegroup/optashift-employee-rostering).
However the issue with this approach is the actual specification of rules.
This is quite difficult to do, so for a fact I can't push this onto the company
owners.
What I could do is create some kind off helping GUI around it.
Or simply use generic predefined rules and if owners want something more
sophisticated they should contact me.
On top of that the OptaPlanner system I know of is written in Java,
which is kind off problematic since I now need to develop some
micro-service architecture around it.

Here is started considering the second approach.
The idea I had is that I use the already entered data by the owner to create a
statistical model of how he likes to make his plannings.
Then we use this model to make future predictions.
This requires zero configuration by the owner,
and also doesn't have the language interaction problem as it's just some calculations.
What I imagine is just adding a predict button to the current roster screen
that uses the statistical model to magically create a roster.

Now the method I thought of is simple, it's an adaptation of [q-learning](https://en.wikipedia.org/wiki/Q-learning),
which uses a lookup table for getting action probabilities, if successful
probability is increased, otherwise decreased.
I'm doing something more simple, here is the gist of it:
We count per day how many job types are planned in on average over *n* weeks.
Then we plan in that amount of job types, for remaining fractions we flip a
coin.
If someone has asked free we remove him from the potential pool of workers.
If there are not enough workers for a job type we just throw a warning for that
date and let the owner figure it out.

The beauty of this technique is that it is simple,
but it takes a lot of work out of planning in people.
If this is well received there is tons of room for extension.
For example naive Bayesian networks can be used to take into account
'experience'.
If we add a reservation datasource somehow prediction could become even more
accurate by using it as another influencing factor.
Whilst the basic interface remains the same, press the 'predict' button and
magically you get a roster.

In conclusion I will attempt a statistical approach rather than OptaPlanner,
because it's more simple.
In the future I can still choose to experiment with OptaPlanner.
It would be much more interesting to use that, once I know how to register
demand accurately,
for example the amount of reservations for an evening would be a good indicator
for restaurants.
