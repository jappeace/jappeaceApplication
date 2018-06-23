Title: Elm on fire! Shaders in elm
Date: 2018-06-21 16:00
Category: technique
OPTIONS: toc:nil
Tags: elm, shaders, programming
subreddit: elm programming shaders

![Elm on fire](/images/2018/elm-fire.svg)

Shaders have long been on the list of possible subject to study for Jappie.
The potential of both creating [beautiful art](https://www.vertexshaderart.com/)
as well as doing parallel processing seem incredible valuable capabilities to have.
This post comments on the effort of porting a
[JavaScript WebGL fire](https://github.com/ethanhjennings/webgl-fire-particles)
to an [elm implementation](https://github.com/jappeace/elmgl-fire).
Elm was chosen as target language because it is opinionated, easy and type safe.
In this post we explore how to get started in elm with shaders,
and then move on trying to port the fire project,
finally performance is increased as much as possible.

# In the beginning there was nothing.

There are some
[example shader](https://github.com/elm-community/webgl/tree/master/examples)
setups for elm.
The `crate' was [copied over](https://github.com/jappeace/elmgl-fire/commit/fb735158f328789a7c30ae4088b8cffcc4be1fd2)
resulting into having a fully 3d crate!
This is not exactly the output desired, a crate is not a fire (obviously),
but now there is a skeleton for the
[elm architecture](https://guide.elm-lang.org/architecture/)
and some example shaders to play with.

![A crate in gl](/images/2018/crate.jpg)

From here on there are two possible paths to continue,
one can try and completely understand what the shaders do and how they
work,
or one can just copy over the shader code from the
[JavaScript](https://github.com/ethanhjennings/webgl-fire-particles) project and see if
we can make that work.
Although
[initial work](https://github.com/jappeace/elmgl-fire/commit/96f3dd293ad72f8b199d7958500f0f14ea2ed013)
was started on the former approach,
the latter approach won out because the topic of 'shader' is just too large.
There is a lot of math involved.
Although this is an exercise of exploration and learning,
trying to understand it all is a massive scope creep.

# Unbreak rendering
After copying over the shader logic from the fire project,
everything broke.
This was not surprising as the crate project was 3d, whereas the fire project
was 2d.
Luckily Elm has strongly typed input for the shaders.
Therefore solving these mismatches was relatively easy.
We could just follow the compile errors.
After [that](https://github.com/jappeace/elmgl-fire/commit/668f714294b4423ae51e8857bf7d9e8dafa4ba8c),
the example program was essentially gutted, only the basic
architecture and API calls were left in tact.
Elm forces this architecture upon us, there is no choice in this.
The result of this effort is shown below.

![Something in gl](/images/2018/gl-something.jpg)

It does not look like much of anything, however, this is counted as progress.
Not having a blank screen is good.
The next thing to do was fixing the colors.
This happened by porting the [hue code](https://github.com/jappeace/elmgl-fire/commit/dbe4c308dcc24f0af8ea6b8f85991c1d83354002),
there was no elm implementation for this particular kind of Hue representation.
Because a white background and the hue produces light blue, we added a
[black background](https://github.com/jappeace/elmgl-fire/commit/dbe4c308dcc24f0af8ea6b8f85991c1d83354002#diff-3e16369f543b857a1fea048cf77b7315R120)
which mixes into an orange.
Now we had the right color, however transparency was also broken.
Transparency was quite interesting because my initial fix involved changing the
shader.
However the the [right (API) option](https://github.com/jappeace/elmgl-fire/commit/dbe4c308dcc24f0af8ea6b8f85991c1d83354002#diff-3e16369f543b857a1fea048cf77b7315R136)
was eventually found
[that solved this issue](https://github.com/jappeace/elmgl-fire/commit/bc9f5d3eecbdc47c0ef0685a005c2af03e1ccd5c).
With all of this in place we get a single circle with the right color!

![Single circle!](/images/2018/gl-reddot.jpg)

Baby steps. Graphics take time.

# Random spheres
This is not impressive at all. However, in life one may find that arity changes
everything.
A single dot on it's own is just a single dot, but if we randomly place it all
over the screen we get something nice to look at (live [here](/raw-html/2018/random-spheres.html)):

<video controls loop video controls autoplay>
    <source src="/images/2018/spheres.webm" type="video/webm">
    Your browser does not support the video tag.
</video>

Aside from random creation this doesn't bring us much closer to the goal of fire.
However some more work was done on it because Jappie thought it looked beautiful.
Performance was increased by converting a particle immediately into it's
WebGL representation.

# Movement
To do movement we dropped some changes from the random sphere case.
The idea of not doing an update loop at all was temporary put aside,
because using an update loop is closer to the JavaScript original.
Keeping it would make porting easier.

Doing movement is simply adding velocity times time to position every frame.
That's it. The simplex noise part of the JavaScript code was also implemented
for variation in movement.

It turns out however that the result is somewhat unimpressive.
Yes it looks like fire, but after about 20 seconds the memory is full,
garbage collection kicks in and the program grinds to an halt.
Here is an example (live [here](/raw-html/2018/slow-fire.html),
may grind your computer to a halt):

<video controls loop video controls autoplay>
    <source src="/images/2018/slow-fire.webm" type="video/webm">
    Your browser does not support the video tag.
</video>

## Speed
The problem is that aside from creating particles and sending them to a GPU,
All existing particles must every cycle be updated with the new location.
We may observer however that the path of the particles after creation is
entirely deterministic.
Why don't we let the shaders do this?
The idea being that we create particles with an initial position, timestamp and
velocity.
Then let the shaders calculate the position for whatever the current timestamp
is.

When trying to implement this, it was found out that the elm gl API was used
in a inefficient way.
The realization came that using an entity per quad doesn't allow us to share
the uniform across all quads.
These entities are analogues to WebGL 'programs'

The architecture was redesigned to take into account multiple particles per elm
entity.
Rather than tracking lists of entities,
lists of tuples of vertices are now being tracked:
`List (Vertex, Vertex, Vertex)`.
It would've been preferable to use `Mesh Vertex` as type, but this type does not
support appending in the elm shader API.

This approach seemed to work much better, in fact this is probably how one
should use this API.
It was possible to render 500 particles now and the computer didn't lock up
(at all):

live [here](/raw-html/2018/fast-fire.html)
<video controls loop video controls autoplay>
    <source src="/images/2018/fast-fire.webm" type="video/webm">
    Your browser does not support the video tag.
</video>

It's still not very good, as the original JavaScript implementation
was able to do up to 3000 particles per seconds quite comfortably
(with a much better frame rate)...
There is not a lot of things done in the this implementation on the CPU side,
and still the CPU intense JavaScript implementation is faster.
Perhaps this is just a limit of using elm.

## More speed?
After thinking about the problem for some time another idea came to mind.
To increase speed, the amount of information send to the GL pipeline can be
reduced.
Every frame sends this Mesh collection to the GPU trough a buffer,
if this buffer can be decreased in size, speed would increase.
It would also lighten the load on garbage collection, as less objects need to
be created.
The suspicion is that elm is slow just because of garbage collection.
We can do this rather trivially by representing each particle as a single vertex,
with a position and size.
Then we just use a shader to reconstruct the vertices into quads (squares).
The vertex shader would move the vertex first,
then another (unkown) shader would do reconstruction,
then the fragment shader would do drawing.
Easy as π.

Stack overflow [suggests](https://stackoverflow.com/questions/5821152/opengl-add-vertices-with-vertex-shader)
that we can use a geometry shader for this.
Unfortunately the elm GL API doesn't support this, 
it only has a slot for vertex, and fragment shader in the [entity function](http://package.elm-lang.org/packages/elm-community/webgl/2.0.5/WebGL#entity).
Jappie briefly got excited about adding this shader type to the elm API,
however he discovered that WebGL doesn't support this type of [shader at all]( https://stackoverflow.com/questions/8641119/webgl-geometry-shader-equivalent).
From this point it's unclear how to increase speed.
Changing WebGL itself is borderline impossible (it would take at least years).

# In conclusion
Upon discovery of the WebGL API for elm Jappie was quite excited about using that.
However after using it, and finding the rather large performance difference
the excitement has been tempered.
Still a lot was learned from doing this project,
elm is a good entry point for graphics development,
type-safety makes the complexity quite manageable.
In fact the idea for using geometry shaders would not had been realized at all
in a faster language.

However in future a faster language will be used.
Not being able to get everything from a machine is quite frustrating.
This therefore will exclude any use of WebGL.
A major drive for using WebGL in the first place was to share the result online,
however this reasoning is quite flawed in that a video of the result can just
be made.
After all we don't even include the live results here because of concern that it
will freeze the readers' computer.

To all who are interested in graphics elm is recommended to start with.
Especially if they are already familiar with elm or the react/redux architecture.
Type safety on shader level is really nice, especially when you do things wrong
structurally and the compiler exactly tells you where you need to repair things
(as happened during this project with the entities).
The price one pays is execution speed in return for development speed.
