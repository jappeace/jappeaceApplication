Title: Haskell 💜 Vibes
Date: 2026.03.08
Category: reflection
OPTIONS: toc:nil
Tags: haskell, vibes

It was Friday, 27th of February 2026,
when I was reborn.
bro. I'm vibe coder now.
bro.
for real.

<style>
figure {
  float: right;
  margin: 2em;
  margin-top: 0em;
  width: 15em;
}
@media (max-width: 420px) {
  figure {
    float: none;
  }
}
figcaption{
  font-size: xx-small;
  color: #999;
}
</style>

<figure>
<iframe width="280" height="165" src="https://www.youtube.com/embed/qMQ-y9dHE2k?si=sqAwlSOezHOboQ8n" title="YouTube video player" frameborder="0" allow="accelerometer; autoplay; clipboard-write; encrypted-media; gyroscope; picture-in-picture; web-share" referrerpolicy="strict-origin-when-cross-origin" allowfullscreen></iframe>

<figcaption> This is how it looks when I'm writing code now. </figcaption>
</figure>

I managed[^dreadful] to log into a [claude code](https://code.claude.com/) terminal.
This is a CLI app, which has access to a [large language model](https://en.wikipedia.org/wiki/Large_language_model)[^llm] (LLM),
with the intention of doing the development work for you.
You ask it to do changes, and it will do it for you.
I didn't, and still don't trust it with access to my machine.
So I trapped it in [a container](https://github.com/jappeace/haskell-vibes).
I asked it to implement some front-end map features,
it did well.
Now I wondered if it could do backend development,
Haskell type errors are a lot more difficult than
emitting react blocks.
So I had pretty low expectations of it being able to do (any) Haskell.
I was wrong.

[^llm]: Read ChatGPT if you're not familiar with this term. 
        The LLM is the thing that decides what to work on next.
        Claude code just sends over the code files and 
        other context as a large chat message to the LLM, 
        and the LLM will then send back actions. 
        Which the claude code terminal can interpret.
        The other thing claude code does is keep on churning until
        it thinks it's finished. 
        It looks like the LLM decides this state as well because you can 
        modify what "done" looks like with a CLAUDE.md file or within the prompt.

[^dreadful]: Logging in to claude was dreadful.
    Why does the terminal break in docker 
    containers of certain window sizes?
    Other problems occurred as well, like service down and registration weirdness.

It's good at Haskell.
There are no mistakes.
Or rather, the compiler points them out to claude,
and it's smart enough to make progress on the compile errors.
So it does what you want it to do.
And it does it fast.

I told it to rip out basement from the [memory](https://hackage.haskell.org/package/memory) package creating the [ram](https://hackage.haskell.org/package/ram) package.[^discussion]
This is not difficult by itself, 
it's just a 
fair bit of investigation, and solving many compile errors.
It did so [with no problems](https://github.com/jappeace/ram/commit/b0e2e66ccc8537c143acb9caf749ef751a1c047f).

[^discussion]: It's part of a larger [discussion](https://discourse.haskell.org/t/fork-basement-as-baseplate/12415/82) on what to do with basement.

At work on Monday I asked it to include
geofences on the sensor return list.
You'd have to join the sensors on the geofences
in the database.
We use [Esqueleto](https://hackage.haskell.org/package/esqueleto) for that, 
which uses some advanced type system features to create a nice DSL.
No problem for Claude.
I pushed this a little further on Friday, 
I asked it to compare the postgis reference
to [Esqueleto PostGIS](https://hackage-content.haskell.org/package/esqueleto-postgis) 
library, and implement the missing functions. 
I also asked it add integration tests to show it works, 
because I don't trust it.
Esqueleto postgis is now [quite complete](https://hackage-content.haskell.org/package/esqueleto-postgis-4.1.0/docs/Database-Esqueleto-Postgis.html).

You know, the nasty part is that I love this.
I mind having to grind through compile errors.
I just want to build stuff, 
the way I get there isn't that important.
I like getting so much done with Claude.
I thought I loved writing haskell,
but I think what I really love is the productivity
it brought me.
Claude does the same but even more so.

The rest of the week has been like that.
My job changed. I changed. 
we vibin now.
This language is for vibes.
Which brings me questions.

## Trust
I don't trust claude.
For every function it writes it has to prove it works with a test.
It's not perfect, it'll gaslight you if you let it.
Sometimes it "forgets" to write tests.[^compression]
Haskell is good for claude because it forces the system to be internally consistent.
In Haskell everything is an expression, and every expression has a type.
The type has to align with the expression (aka check) or else you don't get a program but an error instead.
Claude can't gaslight you into believing the error is what you want.[^hold-beyond]
So it has to be internally consistent,
something which LLM's are bad at.
They'll just do whatever.

[^hold-beyond]: Although I don't put it past it, at several times it claimed it wasn't able to enter the `nix-shell`. Which is a flat out lie. It just doesn't want to build.

[^compression]: I think a problem is "context compression" also compressing the CLAUDE.md file, I just told it to reread it if it's doing compression, that appears to keep it in context. (this could be another bug?). See my [claude.md](https://github.com/jappeace/haskell-vibes/blob/master/CLAUDE.md) file for details

I run it in a container on yolo mode.
By default it'll ask you every command it wants to do.
I don't want to be involved however.
With no oversight, make the test suite pass, make CI pass.[^give-trust]
I review its code to verify if it's correct after it's done and overcame all these hurdles.
I don't want to babysit it.
The container protects me sufficiently from its weirdness.
There is weirdness. 

<figure>
<img  alt="Claude code trapped in container" src="/images/2026/claude-trapped.jpg" />
<figcaption> Claude code trapped in a container</figcaption>
</figure>

At one point it tried writing in `/etc/shadow` to give itself write access to the 
home folder, which I had forgotten to give.
That's a (bad) privilege escalation attempt.
It tried to hack me, 
even though it'll claim it can't do such things.

[^give-trust]: I guess here I'm trusting it to not actually try to break out. I think it could if it wanted to. I can't ask claude how it would do so, but I can ask gemini (funny). 

I don't actually ask it to solve my problems.
I ask it to write implementations, 
I give it steps to do.
I know precisely what I want, 
it just takes me a long time and it's mostly boring work.
I recognize Claude is much faster at grinding through the compile errors than I am,
and it doesn't require my attention.
I can focus on thinking about how to make the system reliable.
I can focus on making sure we don't have throughput issues.
I can do the actual engineering parts of this job.

## Job
What is my job?
Do my employers care about me writing code?
Do they care about me being the grinding through compile errors?
No.
They want a reliable working system.

I used to spend the majority of my time writing 
code because there was no other way to write it.
This may confuse you into believing writing software is your job,
because you spend so much time doing it.
Nobody actually said this was the case. 
There is an engineering part to writing software as well.

<figure>
<img  alt="Burning myself into a new image" src="/images/2026/vibe-coder.jpg" />
<figcaption> Burning myself into a new image </figcaption>
</figure>

I finished Friday with so many PRs open and I flagged this as a concern.
It's not a concern. 
We've just never been in this situation where making the implementation
is the easy part.
But actually it is the easy part.
Getting it to be correct is the hard part.
That final 10% of the implementation is now my job, which is getting it correct.
I have to ensure the software Claude produces works.
I have to think in terms of verification.


I hired [Leana](https://git.confusedcompiler.org/leana8959/blog/src/branch/trunk/content/articles/2025-12-a-comment-preserving-cabal-parser/index.md) to do exact print. 
Can I replace her with AI?
No. 
I want her to be in charge of this.
Leana decides what to do on her own.
All I do is check in on her once in a while 
to make sure she doesn't derail.
Unlike Claude instances, I trust her.
I would give her access to my machine without worries.

I realize this is what my employers want as well.
Trust the job gets done.
How isn't important.
It's almost as if automating lower value jobs[^value-job] 
creates opportunities for more high-value jobs. [^doing]
In my case I just shifted to a higher value 
job because a startup is always understaffed.

[^value-job]: For clarity, I'm not saying programming is a low value job. Even junior roles are important, you can see this on the compensation they get. I'm saying that offloading solving tons of type errors to a tool will allow you to do other stuff, which is likely of higher value.

Am I a Haskell developer?
You spend so much time learning a language that it becomes part of you.
Perhaps this is a me problem.
Everyone can write Haskell now.
You just have to tell claude to implement whatever in Haskell instead
of some other language.
You get more correctness for free.
Claude actually works, and it works well.
There is no learning curve anymore.
Verification is still a problem however. 
If you can't specify the properties 
of your system you can generate all the code 
in the world and get garbage.
Even if it's in Haskell.

## Conclusion
It has been a wild week.
It's not visible to other people,
but stopping to write code has had a large impact on me.
I've written code almost constantly for the past 20 years.
Now it's no longer me.

I'm just processing this.
This had so much impact on my life.
It's over, we're beginning.

[^doing]: See for example the book Learning by Doing: The Real Connection between Innovation, Wages, and Wealth (2015)
