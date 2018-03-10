TITLE: Flask docker and the backend
DATE: 2018-03-10
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
Many members of the team stressed the importance of containerization.
Jappie, being the only one that had worked intimately with this technology
was quite happy with this development.
What was rather surprising was the suggestion that the team ought to run
these containers by hand on rented VPS'es.
Is docker not supposed to make orchestration more easy?
Do there not exist many tools that build on top of docker to provide variances
in orchestration? Such as docker-compose, docker swarm, appengine and Kubernetes?
All these questions entered into Jappie's mind,
but he felt arguing such ignorance would be a waste of his time.
Once the team would start working with the containers they would see for
themselves how easy orchestration is.
The primary building block was in place anyway (docker) moving to another
orchestration methodology will be easy.

It was here that Jappie decided to pick up this task.
He knew about docker from previous experience already, and his other major task
had just finished.
He started with a little hello world in python served through [flask](http://flask.pocoo.org/).
The he put this in a docker container by writing a `Dockerfile`, and finally
he wrote a little docker-compose script to attach an idle Postgres container
which would be need later.
All this was done in the space of about an hour or two.
Setting up docker isn't difficult.
Then it was time for the next step, connecting flask to Postgres.

# Flask
Flask is a micro framework. 
It gives some Rest support, and that's about it.
They sometimes provide some glue scripts that connect some projects to other
projects, but there is no main monolithic architecture behind it.
In stark contrast to [Django](https://www.djangoproject.com/),
which has already done most things for you.

This micro framework setup, gives the project a big advantage in that they can
have independent versioning, and gives the capability to swap out some projects
for others quite easily.
The big disadvantage, as Jappie encountered, is that its fairly challenging to get
into.
One may even say it's hard to get started.
The reason for this is that gluing the ORM to the framework has to be done by
hand.
Jappie wanted to have an ORM to handle migrations and describe the model in.

The thing that ended up working for Jappie was this
[bootstrapping script](https://github.com/davidism/basic_flask) which was about
5 years old and of course outdated.
But Jappie managed to get [it running](https://github.com/jappeace/basic_flask),
by googling errors, and stripping features.
