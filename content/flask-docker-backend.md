TITLE: Flask, docker and the backend
DATE: 2018-03-11
CATEGORY: tools
Tags: docker, backend, flask, python, website, appengine

Jappie Klooster is working with friends on a react native app.
It was attempted to do this completely without server-side with help of Firebase.
This post describes the thought process behind not using Firebase for
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
have independent versions, and gives the capability to swap out some projects
for others quite easily.
The big disadvantage, as Jappie encountered, is that its fairly challenging to get
into.
One may even say it's hard to get started.
The reason for this is that gluing the ORM to the framework has to be done by
hand.
This is of course because people may want to choose a NoSQL storage mechanism
rather than an SQL.
Jappie wanted to have an ORM to handle migrations and as a method of
describing the model in.

The thing that ended up working for Jappie was this
[bootstrapping script](https://github.com/davidism/basic_flask) which was about
5 years old and of course outdated.
But Jappie managed to get [it running](https://github.com/jappeace/basic_flask),
by googling errors, and stripping features.
This should make it easier for future projects to start with flask in a
docker environment.
A pull request was made to merge it back into the original project.

# Deploying
Once the docker-compose setup was completed considerations went out how to
deploy this live.
Being able to setup a live instance early has the advantage that the team can
test the entire (rather complex) infrastructure.
AppEngine was chosen as method of deployment as it could save in costs.
AppEngine essentially allows client code to be shared
across Kubernetes clusters (one assumes),
making it cheaper for small apps to use as the
overhead of running Kubernetes itself does not need to be taken in account.

Since Jappie had never worked with AppEngine before he chose to do the
[hello world](https://cloud.google.com/python/getting-started/hello-world)
tutorial first.
Note that he had to choose the flexible environment as the standard one only
provides python 2.7, which at the time of writing has a bit over
[two years](https://pythonclock.org/) left until deprecated.
Using python 2.7 is therefore a really bad choice.
Even though google [says](https://cloud.google.com/appengine/docs/the-appengine-environments)
python 2.7 is the standard,
Jappie urges his peers to take this claim with suspicion.
Which the [python wiki](https://wiki.python.org/moin/Python2orPython3) backs up:

> Python 2.x is legacy, Python 3.x is the present and future of the language

After the hello world was successfully put online
(in python 3 with the flexible environment),
Jappie decided it was time to try setting up the entire system.
For this he needed to setup a managed Postgres on Google Cloud and connect to
that trough AppEngine.

Google however seems to favor more popular technologies,
and the primary docs assume you want to use [MySQL](https://cloud.google.com/python/getting-started/using-cloud-sql).
Jappie had used MySQL quite a lot when he was younger.
But these days he had grown fond of Postgres.
It is more
[standard compliant and has better data types](https://www.quora.com/What-are-pros-and-cons-of-PostgreSQL-and-MySQL-With-respect-to-reliability-speed-scalability-and-features).
An uber post however claims there are some interesting advantages
to [mysql](https://eng.uber.com/mysql-migration/) if you manage the architecture
yourself.
This is done by google however in this use case,
besides because an ORM is used, switching database technology is relatively easy.

The search engine doesn't show the docs for Postgres with flask in AppEngine.
Jappie found those in the [example project](https://github.com/GoogleCloudPlatform/python-docs-samples/tree/master/appengine/flexible).
The source file had a link to
[using postgres with AppEngine](https://cloud.google.com/appengine/docs/flexible/python/using-cloud-sql-postgres).
To test if the Postgres instance was running 
this [guide](https://cloud.google.com/sql/docs/postgres/connect-admin-proxy)
was used.
Jappie was uncertain why a cloud proxy was recommended this much,
in his memory he hadn't set this up last time with a Django project.
However with that in place one can create a
[specialized user](https://medium.com/@mohammedhammoud/postgresql-create-user-create-database-grant-privileges-access-aabb2507c0aa)
for manipulating the database (rather than the root Postgres account).

Going trough those tutorials Jappie collected the useful commands into a
makefile. With this in place he had no longer need to refer to them
and could just `cat` the makefile to see how to deploy for example:

```make
deploy:
    SQLALCHEMY_DATABASE_URI=postgresql+psycopg2://USER:PASWORD@/DB?host=localhost python ./manage.py db upgrade || (echo "make sure the proxy is running" && false)
    gcloud app deploy

describe:
    gcloud sql instances describe DB_NAME

proxy:
    cloud_sql_proxy -instances=INSTANCE_PATH=tcp:5432

connect:
    echo "warning, requires you to setup the proxy in another terminal"
      # psql "host=127.0.0.1 sslmode=disable user=postgres"
    psql "host=127.0.0.1 sslmode=disable user=USER dbname=DB password=PASSWORD"
```

Now any of the team could deploy with `make deploy`. Note that the credentials
where retracted out of security reasons.

# Conclusion
There is a lot involved to setting up a backend.
It took about two days going from knowing almost nothing about flask and
AppEngine, to having a hello world.
Not even taking into consideration all the previous knowledge Jappie already
had acquired with docker, Postgres and python itself.
Note that at this point we haven't even made anything of interest.
This post was about setting up infrastructure...
It is to hope that this story may help some other hackers,
so that they can save a few days.
