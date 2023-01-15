TITLE: The peculiar event sourced deadlock
DATE: 2023-01-14
CATEGORY: reflection
Tags: postgres deadlock programming

![THE UNDEAD LOCK OF DETH](images/2023/postgresql-deadlock.png)

About a week ago the system became unsuable 



This is an after action report of a complicated
system level bug.
It took me a week to come to a satisfying
solution.
To start I need to sketch context.
Consider an event source system:

```sql
CREATE TABLE event (
    id serial PRIMARY KEY NOT NULL,
    payload jsonb NOT NULL,
    type character varying NOT NULL,
    created timestamp with time zone NOT NULL
);

CREATE TABLE event_last_applied (
    id serial PRIMARY KEY NOT NULL,
    event_id bigint NOT NULL REFERENCES event (id)
);
```

In here the event 'type' and `payload` fields
contains the information to re-apply that event.
The type will indicate what business code or
queries to execute, and the payload holds
information for that logic.
This application or re-application is called projecting.
The `id` provides a unique global ordering.
And `created` contains a timestamp of when the event
was created, mostly for admin purposes.
The `event_last_applied` table is used to indicate
whichever event was last applied, so the system
can figure out if additional events need to be 
reprojected from the `event` table.

Inserting an event
works by first applying an event to normal
postgres tables in a transaction.
Then,
once it's not rejected by foreign keys,
type errors or program exceptions,
the event gets recorded like so:
```sql
begin;

/* projection code, insert user into tables here */

INSERT INTO event (payload, type, created)
    VALUES ('{"email":"hi@jappie.me"}', 'create-user', now());
INSERT INTO event_last_applied (id, event_id)
SELECT
    1,
    max(id)
FROM
    event
ON CONFLICT (id)
    DO UPDATE SET
        event_id = lastval();
commit;
```

If the projection fails the entire event
gets rejected,
which means all changes within the
transaction get rolled back by postgres.
So this applies relational guarantees,
to a non-relational system trough a transaction.
We also weave this transaction trough business logic
code,
so in case of an exception in code,
we rollback.
Quite an elegant solution, which
[I did](https://garba.org/posts/2016/event-sourcing/#materialised-view-pattern)
[not invent](https://www.ahri.net/2019/07/practical-event-driven-and-sourced-programs-in-haskell/).

On system boot we figure out if we need to reproject
or not,
the query is rather simple:
```sql
SELECT type, payload FROM event
WHERE
    id > (
        SELECT event_id FROM event_last_applied
        WHERE id = 1)
ORDER BY
    id ASC;
```
which returns something like this, telling the system what to do:
```
    type     |          payload          
-------------+---------------------------
 create-user | {"email": "hi@jappie.me"}
```
With that, we can replay history.

## Deadlock
There is one more important piece of context.
Namely, an event maybe composed with other events
into a larger transaction.
For example, if we create a user, we may
also assign him to a company,
or mark them as a test user,
within the same transaction.
So for example:

```sql
BEGIN;

/* projection code, insert user into tables here */
INSERT INTO event (payload, type, created)
    VALUES (
        /* whatever event source data*/
        '{"email":"hi@jappie.me"}', 'create-user', now());
INSERT INTO event_last_applied (id, event_id)
SELECT
    1,
    max(id)
FROM
    event
ON CONFLICT (id)
    DO UPDATE SET
        event_id = lastval();

/* projection code, connect user to company */
INSERT INTO event (payload, type, created)
    VALUES (
        /* whatever event source data*/
        '{"company-id":2, "user-id": 1}', 'connect-company', now());
INSERT INTO event_last_applied (id, event_id)
SELECT
    1,
    max(id)
FROM
    event
ON CONFLICT (id)
    DO UPDATE SET
        event_id = lastval();
COMMIT;
```

So yes, transactions form proper monoids,
and they can grow arbitrarily large.
This is good because even for large chuncks
of business logic we always gaurantee our
event log remains in a valid state.
We'd expect our re-projections to always work,
because only correct ones get recorded.
Where does this go wrong then?

The issue is concurrency,
consider connection `A` and `B`:

1. `A` opens a transaction and inserts a user, but has to do other stuff as well
2. `B` opens a transaction and wants to insert an event,
   `B` has to wait until `A` completes.
   This is because `A` made an update to the `event_last_applied` row `1`,
   which is locked until that transaction completes.
3. `A` completes and releases the lock on row `1`.
4. `B` can now complete as well.

This is not a deadlock as long as A completes.
`B` can wait a long time
because our transactions can grow arbitrarily big.
For example when we're inserting millions of rows of data,
this can take up to half an hour.
Which is far beyond the HTTP session length of 30 seconds,
or whatever any user finds acceptable.
This was indeed the production bug encountered at [supercede](https://supercede.com/).
One user was doing pack ingestion,
which involves reading millions of excell file rows,
and the rest of the system became unusable because of that.
We hadn't notice this before because our system
only experiences light usage normally,
and pack ingestation only happens once a year.

Actually at first we thought this was because
of high cpu usage.
The graphs clearly showed high load for CPU,
so maybe the rest of the system just got deprioritized.
But before assuming that was the issue I decided
to just reproduce the issue first.
Here I noticed I could for example load the risk index
easily, but connecting to a pack (or a write),
would hang forever.
This made me suspect that the issue wasn't
CPU usage at all,
so I asked postgres to list [it's locks](https://wiki.postgresql.org/wiki/Lock_Monitoring).
Which clearly showed several locks in progress.
This lead me to the event source system.


## Now what? 
At first I started with the most obvious solution.
I re-grouped how even sourcing took place.
If I put the even sourcing code to the end of the
transaction,
the event source table remained available
for other transactions up till that point.

This worked!
It worked for this transaction with pack ingestation,
I didn't know if there were any other transactions
like this in our code base.
Furthermore, I had to bypass parts of the event
sourcing API to work,
for example I had to project events by hand,
and insert events by hand.
Something which was normally done by the internal library.
I decided this was a really bad precedence to set.
So I went looking for other solutions.

Another idea is that instead of doing the large transaction,
we could split it up into smaller ones.
Allowing other events to clear while this bigger one
was in progress.
I didn't like this either.
For one this code was old, tried and tested,
making a rather large modification like splitting
the transaction could introduce many unintended bugs.
For example when cleanup doesn't happen correctly on failure.
I thought this was likely because this transaction was large,
and covered many tables.
Also our normal tools such as types and integration tests
wouldn't help a lot with guaranteeing cleanup.
Furthermore I had a much more simple but thorough solution in mind.

I decided to redesign the even source tables.
Naturally my colleagues exclaimed shouts of joy when
I decided to modify an even older system.
But I believed it was much easier to modify,
and more importantly,
easier to test for correctness.
The new schema looks almost identical to the old one:

```sql
CREATE TABLE event (
    id serial PRIMARY KEY NOT NULL,
    payload jsonb NOT NULL,
    type character varying NOT NULL,
    created timestamp with time zone NOT NULL
);

CREATE TABLE event_applied (
    id serial PRIMARY KEY NOT NULL,
    event_id bigint NOT NULL REFERENCES event (id),
    created timestamp with time zone NOT NULL
);

```
The big difference is that we renamed 
`event_last_applied` to `event_applied`
and added a created field.

Now inserting events is also quite similar:
```
BEGIN;
INSERT INTO event (payload, type, created)
    VALUES ('{"email":"hi@jappie.me"}', 'create-user', now());
INSERT INTO event_applied (event_id, created)
SELECT
    last_value,
    now()
FROM
    event_id_seq;
COMMIT;
```
The big difference is that insert of modifying always
row 1 to be the latest ID, we insert a new row into
`event_applied` with the latest id.
This avoids locking of row 1.
Reprojection we truncate the event_applied
table, allowing the code to rerun all those events.
The big difference is in figuring out which events
haven't been applied yet:
```sql
SELECT type, payload FROM event AS e
WHERE
    NOT EXISTS (
        SELECT 1 FROM event_applied
        WHERE event_id = e.id)
ORDER BY
    id ASC;
```
We compare the event table to the `event_applied` table,
and return any events that don't exist in that.
We're still ordering by id to ensure the correct order.

Is this correct? Let's consider concurency once more:
consider connection `A` and `B`:

1. `A` opens a transaction and inserts a user, but has to do other event source queries as well.
2. `B` opens a transaction does it's projection work and wants to insert an event,
   `B` creates a new row in the `even_applied` table and completes.
   There is no need to wait since there is no single row lock.
3. `A` finishes it's other event sourcing completes.

This doesn't deadlock.
However it's not completely correct in that A get's id 1.
and `B` get's id 2,
but `A` finishes after `B`.
So on reprojection `A` get's applied before `B`,
but the transaction will check for
correctnes after the result of `B`.
This *may* cause issues.
Perhaps to solve this one could order by commit
timestamp of a transaction.
Another solution would be to group the events by transaction id.
And then order by last created event.
In this case all events created before `B` would be pushed
behind it by the event happening after `B`.
For now what we'll definitely do is re-projecting all
all events also in a transaction,
so we can figure out by hand what happened if a reprojection
failed.
And not cause downtime.
It would be rather silly for example to cause downtime
before christmass just because you'd urgently wanted
to solve some projection bug.

This works, and allows arbitrary sized transactions to
project alongside each-other.

You'd say the auto-incrementing rows are also an issue.
However this is not the case,
because apparently `nextval`
of postgres works outside of a transaction.
There is no need to replace the id with uuid's
and figure out event order based on dates.
Although this is also possible.


Eventual consistency issues.

## Closing thoughts
I think the biggest lesson I've learned from this
is to make sure you reproduce an issue first,
before diving into solutions.
Even nasty business threatening system level bugs like these can
sometimes be solved with some minor modifications to the system.
Having an automated test available showing the issue was
a great help with iterating potential solutions.
If we has skipped this small step of reproducing the issue,
we may have feared of to
moving pack ingestation to a sepate machine,
which would've taken weeks to implement
and not actually solve anything.

Furthermore, it's humbling to see that even after having
used relational databases for more then a decade,
I still can learn new things.
For example that the auto increment sidesteps the postgres
transaction was quite shocking to me.
Here once more the automated test helped,
because adapted initially the implementation to show how it wouldn't work to a colleague.
But to my surprise the test passed!

