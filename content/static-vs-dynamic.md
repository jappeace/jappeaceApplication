Title: Static vs dynamic types
Date: 2018-03-21 13:30
Category: focus
OPTIONS: toc:nil
Tags: test, reddit, project, comment
status: draft

More then I like to admit I have these debates
with people about type system.
I'm like strong type systems because it feels
like I can hack faster with them.
Yet paradoxically my conversation partners 
will claim exactly the same thing about dynamic type systems?!
What is going on?

# Proccess
Okay so let me first start of with what I do when I'm hacking:

1. Make an aggressive change to a type signature.
2. Use the file-watch to rapidly navigate and solve all type check errors.
   i3, projectile and ranger are of great help.
3. Run the program if the change caused any runtime changes.

Essentially I'm using the type system as a rapid debug
method.
This rapid debug phase can last for multiple
hours to days, because this is when I'm enfording
my will on the program.
It starts with the aggressive initial chagne
where I'm not looking at the type signature, but
then solving all the resulting errors will mostly
result in the correct output program.

I have no glorious plan when I'm making that initial aggressive
change, I just start with obvious things that I need to proceed.
Such as a map of users, or a set of days.
This is greedy hillclimbing.
Leaving the hard thinking to the end,
and usually there is no hard thinking required,
because once you are passed a certain point of collecting
shit, you end up with all the pieces together to see some clever
trick that solves everything.

I imagine people who use weakly typed langauge
will do something similar but they'd have to run
the program to spot bugs.
I think this way of debugging is much more akin
to a black box appraoch,
where you treat the entire program like a big unkown.
And use little experiments to figure out if youre changes
are correct.

Which is fine if you think you're faster like that,
I'm certainly not.
Considering the amount of context switching
my mind has to do to run something.
Or figuring out the little experiments on each change.
Why not use a type system to filter out the obvious
mistakes so you can focus on real issues?

Yes, I still need to run the program too in the end,
but most stupid stuff will have been solved.
All that's left is logic bugs, 
not that these are trivial, on the contrary.
My thesis is that a type system will help
you get to the 'logic bug' phase much faster.

Then finally there is the devops phase,
issues in this phase are rare, but they're scary.
Let's not talk about the devops phase.
Except use nix so you can get help from the sadists
that like devops, reproducibly.

# Repl
I presume it'd be easier to run a program if you'd use 
a repl? I've never really gotten into those however.
The issue I have with those is that there is an immenant
danger of losing work.
God I hate losing work, ever I lost an entire essay when
I was 12 or so I've taken aggressive measures to prevent loss.

At someone made the comparison of type systems between the cathedral
and the bazaar, where strongly typed is a cathedral (eg
well thought out before hand, and weakly typed systems are
the bazaar (chaning everything on the fly)).
