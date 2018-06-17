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

![Single circle!](/images/2018/gl-reddot.jpg)

# Random spheres
This is not impressive at all. However, in life one may find that arity changes
everything.
A single dot on it's own is just a single dot, but if we randomly place it all
over the screen we get something nice to look at (live [here](/raw-html/2018/random-spheres.html)):

<video controls loop video controls>
    <source src="/images/2018/spheres.webm" type="video/webm">
    Your browser does not support the video tag.
</video>

Asside from random creation this doesn't bring us much closer to the goal of fire.
However some more work was done on it because Jappie thought it looked beautiful.
Performance was increased by after creating a particle storing it in it's
webgl representative form immediatly.
Before that there was an update loop that changed the particle types into elmgl
objects, which is a more traditional way of doing 'games'.
In this case it meant having a particle model, which in the view gets translated
into the verticies.
However it is taxing on garbage collection in a pure langauge because the
verticies have to be recreated every frame.
Keeping a list of final verticies is much lighter.

# Movement
To do movement we dropped some changes from the random sphere case.
That work was put on a branch because in the future we may want to do more
experimentation with the random speheres. (For example by introducing varying hues,
and or letting the spheres fade based upon time).
The idea of not doing an update loop at all is temporarly put asside,
it would be easier to make to just port the javascript first to elm.

It turns out however that the result is somewhat unimpressive.
Yes it kindoff looks like fire, but after about 20 seconds the garbage collection
kicks in and the program grinds to an halt, here is an example (
live [here](/raw-html/2018/slow-fire.html), may grind your computer to a
halt):

<video controls loop video controls>
    <source src="/images/2018/slow-fire.webm" type="video/webm">
    Your browser does not support the video tag.
</video>

## Speed
The problem is that aside from creating particles and sending them to a gpu,
All existing particles must every cycle be updated with the new location.
This is of course rather dumb, the path of the particles after creation is
entirly deterministic.
Why don't we let the shaders do this?
The idea being that we create particles with an initial position, timestamp and
velocity.
And then let the shaders calculate the positition for whatever the current
timestamp is.

Turns out howver this is easier said than done. Because the elm webgl api
specifies a uniform for each entity it has to always loop over the in memory
structure (causing unstable behavior).
In an unpure langauge this wouldn't have been a problem as we could just 
replace the reference with a newly updated uniform.
Elm does not support this.

What potentially could be done is redigning the shader to take into account
multiple particles per elm entity.
This is what is done.
Rather than tracking lists of entities, lists of tuples of vertecies are now being tracked:
`List (Vertex, Vertex, Vertex)`.
It would've been preferable to use `Mesh Vertex` as type, but it's not monoidal.
What is a mesh but a list of triangles? or in other words, what is a mesh but
a list of 3 verticies. Since lists can be joined together it should be monoidal.

This approach seemed to work much better, in fact this is probably how one
should use this api. It was possible to render 500 particles now comfortably:

live [here](/raw-html/2018/fast-fire.html)
<video controls loop video controls>
    <source src="/images/2018/fast-fire.webm" type="video/webm">
    Your browser does not support the video tag.
</video>

It's still not very good, as the original was able to do up to 3000 particles
per seconds quite comfortably...
Currently it is unclear how to improve the speed.
There is not a lot of things done on the cpu side, and still the javascript
implementation, which does almost everything on cpu, is faster.
Perhaps this is just a limit of using elm.

Unfortunatily wind hasn't been implemented, neither have the sparks been.
The port is quite incomplete.
For the wind we require a gsl implementation of simplex noise (perlin noise).
Which should be possible because it's just a big pre-seeded lookup table,
however this is out of the scope of this project.
The sparks are just a particle with different texture and behavior charistics.
It would be quite trivial to add, which is left as an excersize for the reader.

# In conclusion
Upon discovery of the webgl api for elm Jappie was quite excited about using that.
However after using it, and finding the rather large performance difference
the excitement has been tempered.
Perhaps elm is not yet ready for any serious game development.
