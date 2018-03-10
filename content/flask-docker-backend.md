TITLE: Flask docker and the backend
DATE: 2018-03-09
CATEGORY: tools
Tags: docker, backend, flask, python, website, appengine
status: draft
[//]: # (TODO: Third person)
[//]: # (TODO: Add link to flask script that works with docker)

Jappie Klooster is working with friends on a react native app.
It was attempted to do this completely without server-side with help of Firebase.
This post describes the thought process behind not using firebase for
*everything*, and setting up a custom backend instead.

The first major issue, for this use case specifically, is the type of database
provided within Firebase.
It's a NoSQL database that promises to sync between all devices.
They probably mean syncing lazily, it's implied in the docs,
but there is no definitive statement around that.
NoSQL is the idea that [data doesn't need relationships](http://www.monitis.com/blog/cc-in-review-the-key-differences-between-sql-and-nosql-dbs/).
For the core business logic of our app, we need relationships between data,
and we need a lot of them.
Technically one can represent these within NoSQL, but now you need to solve
problems which most SQL databases have already done for you.

Another *big* disadvantage of Firebase is that it's proprietary.
Once a large chunk of the business logic, and data resides inside the Firebase
ecosystem, it's hard to get out.
This means it's relatively easy for the owner of the proprietary system, Google,
to crank up the price.
Neither Jappie nor his collaborators wanted to 

# Language choice
The primary question imposed for deciding which language to use was
"How fast do you want it?".
*Fast*.
This means the team rejected the idea that they wanted to learn from implementing
the backend.
Therefore a host of options was eliminated, such as Haskell and Rust.
In fact because we wanted a pan-team familiar choice,
only two real contenders were left. NodeJS and Python.
The choice of Python was made because it is a much more simple
language than JavaScript.
Another advantage of choosing either for these languages would be that our
editors are already setup to handle them.

# Docker
My friends made it clear that it was of utmost importance we would jam our
code into docker containers.
I was pretty okay with that, but the way they spoke about deploying it was
kind off, peculiar..
They wanted to run docker container manually on rented VPS'es.
That's like saying you want to use this awesome technology which makes
orchestration super easy and not use it for orchestration at all.
I mean it's hard to argue against such ignorance so I didn't,
instead I tried remaining in the problem domain (what do you want?
not what do you envision as an implementation).

I picked up this task because I already knew how to dockerize things and my
other major task had just completed.
I wrote the little docker compose script, made a hello world in python and
flask, put a unused Postgres container in the compose file for good measure,
tested it out and it worked nicely.
All this was in the space of about an hour or two, including stuff such as
setting up the project writing the `Dockerfile` etc.
Then it was time for the next step, connecting flask to Postgres.

# Flask
Flask is built around the idea of micro-architectures.
They give you stuff to do Rest with, and that's about it.
They sometimes provide some glue scripts that connect some projects to other
projects, but there is no main monolithic architecture behind it.

This gives the project a big advantage in that they can have
independent versioning, swap out some projects for other projects etc.
The big disadvantage, as I found out, is that its fairly challenging to get
into.
In fact it's hard.

The thing I wanted was pretty basic, an ORM that can handle migrations for me.
Those are the things any project basically needs, an ORM to do data
(unless you're a fan of NoSQL, but if we were we wouldn't have had this issue),
and a protocol to communicate through.
Generally http and Rest because [of many good things](https://www.ics.uci.edu/~fielding/pubs/dissertation/rest_arch_style.htm).

I just went over example, through example, each of which had some other problem.
Untill I found this large
[bootstrapping script](https://github.com/davidism/basic_flask) which was about
5 years old.
I ended patching it up to to modern flask, and put it into a docker container.
