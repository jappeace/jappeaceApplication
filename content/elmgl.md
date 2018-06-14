Title: Elm on fire! Shaders in elm
Date: 2018-06-10 16:00
Category: technique
OPTIONS: toc:nil
Tags: elm, shaders, programming
subreddit: elm programming shaders
status: draft

![Elm on fire](/images/2018/elm-fire.svg)

Shaders have long been on the list of possible subject to study for Jappie.
The potential of both creating [beautiful art](https://www.vertexshaderart.com/)
as well as doing parallel processing seem incredible valuable capabilities to have.
This post comments on the effort of porting a
[javasript webgl fire](https://github.com/ethanhjennings/webgl-fire-particles)
to an [elm implemention](https://github.com/jappeace/elmgl-fire).
Elm was chosen as target language because it is opinionated, easy and type safe.
We will soon find out that because elm is a pure language,
garbage collection will grind our program to an halt,
therefore we must push more logic to the shaders!

# In the beginning there was nothing.

However there are some
[example shader](https://github.com/elm-community/webgl/tree/master/examples)
setups for elm.
The `crate' was [copied over](https://github.com/jappeace/elmgl-fire/commit/fb735158f328789a7c30ae4088b8cffcc4be1fd2)
resulting into having a fully 3d crate!
This is not exactly the output desired, a crate is not a fire (obviously),
but now there is a skeleton for the
[elm architecture](https://guide.elm-lang.org/architecture/)
and two shaders to tweak and play with.

![A crate in gl](/images/2018/gl-crate.jpg)

It was here that Jappie pat himself on the back for doing nearly nothing and
accomplishing something that looked impressive.
A pattern that may recur during the blog post.

From here on there are two possible paths to continue,
first one can try and completely understand what the shaders do and how they
work,
or one can just copy over the shader code from the javascript project and see if
we can make that work.
Although
[initial work](https://github.com/jappeace/elmgl-fire/commit/96f3dd293ad72f8b199d7958500f0f14ea2ed013)
was started on the former approach,
the latter approach won out because the topic of 'shader' is just too large.
Although this is an exercise of exploration and learning,
trying to understand it all is a massive scope creep.

# Unbreak rendering
It is of no surprise that everything breaks if you swap out the shaders of a gl
program. Elm has strongly typed input for the shaders however.
Solving these mismatches wasn't a big of a deal as in for example JavaScript.
When that was [done](https://github.com/jappeace/elmgl-fire/commit/668f714294b4423ae51e8857bf7d9e8dafa4ba8c).
The example program was gutted at this point, only the basic
architecture and api calls were left in tact.
Elm forces this architecture upon us, and there is no choice in this api.
The result of this effort is shown below.

![Something in gl](/images/2018/gl-something.jpg)

Surprisngly this is counted as progress. Not having a blank screen is good in gl.
Obviousy, the next thing to do was fixing the colors.
This happened by porting the [hue code](https://github.com/jappeace/elmgl-fire/commit/dbe4c308dcc24f0af8ea6b8f85991c1d83354002),
there was no elm implementation for this particular kind of Hue representation.
Adding a [black background](https://github.com/jappeace/elmgl-fire/commit/dbe4c308dcc24f0af8ea6b8f85991c1d83354002#diff-3e16369f543b857a1fea048cf77b7315R120)
(because a white background and the hue produces
light blue, but a black background and light blue mixes into an orange).
Finally transparancy needed to be unbroken, transparancy.
Transparancy was quite interesting because my initial fix involved changing the
shader.
However the the [right option](https://github.com/jappeace/elmgl-fire/commit/dbe4c308dcc24f0af8ea6b8f85991c1d83354002#diff-3e16369f543b857a1fea048cf77b7315R136)
was eventually found
[that solved this issue](https://github.com/jappeace/elmgl-fire/commit/bc9f5d3eecbdc47c0ef0685a005c2af03e1ccd5c).
With all of this in place we get a single circle!
