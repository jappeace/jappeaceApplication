TITLE: The peculiar event sourced deadlock
DATE: 2023-01-14
CATEGORY: reflection
Tags: postgres, deadlock, programming, sql, database

![THE UNDEAD LOCK OF DETH](images/2023/postgresql-deadlock.png)

One thing that always surprises me is how casually 
serious problems are phrased by business people
in their blissful ignorance.
"hey why am I seeing the down for maintenance screen?"
"Oh try it now, the pack uploading has finished",
Said the QA engineer to the product manager.
Once I saw this on slack, I grew really suspicious
and started asking questions.
After all, isn't it a bit odd we're seeing a down for maintenance screen
in one part of the system,
simply because another part is being used?

At first we thought this was because of high CPU usage.
The graphs showed high CPU load while processing packs,
so maybe the rest of the system was being deprioritized somehow.
Before assuming that was the cause however,
I decided to reproduce the issue first.
Here I noticed I could for example load the risk index
easily (a read operation),
but connecting a risk to a pack (a write operation), would hang forever.
This made me suspect that the issue wasn't
CPU usage at all,
so I asked Postgres to list [it's locks](https://wiki.postgresql.org/wiki/Lock_Monitoring).
Which showed several locks in progress.
This lead me to the event source system.
The event source system is at the core of all our business logic.
In essence, it provides a ledger of all important business write
activities that can happen.
[This is useful](https://www.youtube.com/watch?v=8JKjvY4etTY)
for auditing purposes for example.

Welcome to an after action report of a complicated
system level bug.
It took me a week to find a satisfying
solution.
To start I need to sketch context.
I'll only have to use raw SQL because 
it's all related to the database and how we use it for event sourcing.
So consider the tables of an event source system:

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

In here the `type` and `payload` fields
contains the information to (re)apply that event.
The `type` will indicate what business logic or
queries to execute, and the `payload` holds
information for that logic.
As we'll see later,
these queries will involve modifying other normal
tables within a transaction.
This application of events, or re-application
through business logic or queries is called projecting.
A `type` can for example be `create-user`
and the `payload` would contain the data required for creating said user,
for example `{email:'hi@jappie.me'}`.
The `id` provides a unique global ordering,
and the `created` field contains a timestamp of when the event
was created, which is used for database administration purposes.
Finally, the `event_last_applied` table is used to indicate
whichever event was last applied, so the system
can figure out if additional events need to be 
re-projected from the `event` table.

Inserting an event
works by projecting an event to normal
Postgres tables in a transaction.
Once this operation is not rejected by foreign keys,
type errors or program exceptions,
the event gets recorded in the ledger,
also known as the `event` table. For example:
```sql
begin;

/* left out projection code, insert user into tables here,
or do other projection stuff, as dictated by the event type*/

INSERT INTO event (payload, type, created)
    VALUES ('{"email":"hi@jappie.me"}', 'create-user', now());
INSERT INTO event_last_applied (id, event_id)
SELECT 1, max(id) FROM event
ON CONFLICT (id)
    DO UPDATE SET
        event_id = lastval();
commit;
```

If the projection fails the entire event
gets rejected,
which means all changes within the
transaction get rolled back by Postgres.
This applies relational guarantees,
to a non-relational system trough a transaction.
We also weave this transaction trough business logic
code,
so that in case of an exception,
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
With that, we can reproject, also known as replaying history.
Replaying history involves truncating all tables
that are event sourced.
And then truncating the `event_last_applied`
table,
which in this case just removes the one row.
Then the system will notice it needs to replay
events on boot for example.
This is a rather dangerous operation,
because if any event fails, 
you may have potentially lost data.
A lot of things can go wrong with a large history,
foreign keys, exceptions, serialization mismatches,
events out of order etc.
Transactions can help here as well, and make this re-projection safe.

## Deadlock
There is one more important piece of context:
An event maybe composed with other events
into larger transactions.
For example,
if we create a user,
we may also assign him to a company
within the same transaction.
In SQL that looks like this:

```sql
BEGIN;

/* left out projection code, insert user into tables here */

INSERT INTO event (payload, type, created)
    VALUES (
        /* whatever event source data*/
        '{"email":"hi@jappie.me"}', 'create-user', now());
INSERT INTO event_last_applied (id, event_id)
SELECT 1, max(id) FROM event
ON CONFLICT (id)
    DO UPDATE SET
        event_id = lastval();

/* left out projection code, connect user to company */

INSERT INTO event (payload, type, created)
    VALUES (
        /* whatever event source data*/
        '{"company-id":2, "user-id": 1}', 'connect-company', now());
INSERT INTO event_last_applied (id, event_id)
SELECT 1, max(id) FROM event
ON CONFLICT (id)
    DO UPDATE SET
        event_id = lastval();
COMMIT;
```

Transactions form proper [monoids](https://hackage.haskell.org/package/base-4.17.0.0/docs/Data-Monoid.html),
and they can grow arbitrarily large.
This is good because even for large chuncks
of business logic we always gaurantee our
event log remains in a valid state.
We'd expect our re-projections to always work,
because only correct ones get recorded.
Where does this go wrong then?

The issue is concurrency,
consider connection `A` and `B`:

1. `A` opens a transaction and inserts a user,
    but has to do other projections and event insertions as well
2. `B` opens a transaction and wants to insert an event,
   `B` has to wait until `A` completes.
   This is because `A` made an update to the `event_last_applied` on row number `1`,
   as part of the insert event logic.
   This row is locked until `A` completes, so `B` has to wait.
3. `A` completes and releases the lock on row `1`.
4. `B` can now complete as well.

This is not a deadlock as long as `A` completes.
`B` can wait a long time
because our transactions can grow arbitrarily large.
For example when we're inserting millions of rows of data,
taking up half an hour.
Which is far beyond the HTTP session length of 30 seconds,
or whatever length a user finds acceptable.
This was indeed the production bug encountered at [supercede](https://supercede.com/).
One user was doing pack ingestion,
which involves reading millions of excell file rows,
and the rest of the system became unusable because of that.

## Now what? 
At first I started with the most obvious solution.
I re-grouped how event sourcing took place.
I Put the even sourcing code at the end of the
transaction in pack ingestion,
so that the event source table remained available
for other transactions up till that point.
This worked!
However it only worked for this transaction with
pack ingestation,
I didn't know if there were any other transactions
like this in our code base.
Furthermore, I had to bypass parts of the event
sourcing interface to make this work.
For example, I had to project events by hand,
and insert events by hand,
rather then using the internal library.
I decided this was a bad precedence to set.
I was afraid other engineers would copy this approach,
when it wasn't necessary.
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
So this would become difficult to maintain fast.
Which is problematic for a piece of code which is the "money maker",
and needs to change often.
Furthermore I had a much more simple but thorough solution in mind.

I decided to redesign the event source tables.
Naturally my colleagues exclaimed shouts of joy when
I decided to modify an even older system.
The event source system described above is almost as old as
supercede.
But I believed it was easier to modify,
and more importantly,
easier to test for correctness.
Furthermore this would also solve the problem for other,
possibly unknown, or future, large transactions.
This change would keep our code easy to maintain
and solve a bug.
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
With this change,
inserting events is also quite similar to the initial system:
```sql
BEGIN;
INSERT INTO event (payload, type, created)
    VALUES ('{"email":"hi@jappie.me"}', 'create-user', now());
INSERT INTO event_applied (event_id, created)
SELECT last_value, now() FROM event_id_seq;
COMMIT;
```
The big difference is that instead of modifying always
row number 1 to be the latest ID, we insert a new row into
`event_applied` with the latest id.
This avoids locking of row number 1.
For 
re-projection we truncate the `event_applied`
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
Is this correct? Let's consider concurrency once more
with connection `A` and `B`:

1. `A` opens a transaction and inserts a user, but has to do other event source queries as well.
2. `B` opens a transaction does it's projection work and wants to insert an event,
   `B` creates a new row in the `even_applied` table and completes.
   There is no need to wait since there is no single row lock.
   So `B` finishes.
3. `A` finishes it's other event sourcing completes.

This doesn't deadlock.
However it's not completely correct in that `A` get's id 1.
and `B` get's id 2,
but `A`'s transaction finishes after `B` by inserting another event with id 3.
So on reprojection one of `A`'s events get's applied before `B`.
But in the initial projection, all of `A`'s event happened *after* `B`.
So the first event of `A` is out of order.
This *may* cause issues.
This problem was also present in the original implementation,
since an id is acquired before the lock waiting happens.
I think a solution would be to group the events by transaction id,
and then order by last created event.
In this case all events created before `B` in `A`'s transaction
would be pushed behind it by an event happening after `B` finishes.
If we do that, the event table gets an extra field:
```sql
CREATE TABLE event (
    id serial PRIMARY KEY NOT NULL,
    payload jsonb NOT NULL,
    type character varying NOT NULL,
    created timestamp with time zone NOT NULL,
    transaction_id bigint NOT NULL
);
```
Our insert function retrieves the transaction id with [`txid_current`](https://www.postgresql.org/docs/9.0/functions-info.html#FUNCTIONS-TXID-SNAPSHOT):
```sql
BEGIN;
INSERT INTO event (payload, type, created, transaction_id)
    VALUES ('{"email":"hi@jappie.me"}'
           , 'create-user'
           , now()
           , txid_current());
INSERT INTO event_applied (event_id, created)
SELECT last_value, now() FROM event_id_seq;
COMMIT;
```
And our unnaplied events query now groups:
```sql
SELECT
    array_agg(type) AS types,
    array_agg(payload) AS payloads
FROM event AS e
WHERE NOT EXISTS (
      SELECT 1 FROM event_applied WHERE event_id = e.id
    )
GROUP BY transaction_id
ORDER BY max(id) ASC;
```

If we run that unnaplied events query on an event table like this:
```
id |        payload        |      type       | created    | transaction_id 
---+-----------------------+-----------------+------------+----------------
 6 | {email: hi@jappie.me} | delete-user     | 2023-01-15 | 77958
 7 | {email: hi@jappie.me} | create-user     | 2023-01-15 | 77959
 8 | {company-id: 2}       | delete-company  | 2023-01-15 | 77958
```
We'd get a result like:
```
             types             |              payloads
-------------------------------+-----------------------------------------
 {create-user}                 | {{email: 'hi@jappie.me'}}
 {delete-user,delete-company}  | {{email: 'hi@jappie.me'},{company-id: 2}}
```
Which is what we want.
Even though the create user event happened
while the delete user event was happening,
the delete user event was part of a larger transaction.
So the create user even should come first when re-projecting.
This allows 
arbitrary sized transactions to
project alongside each-other and provides better
ordering guarantees then the original.

## Closing thoughts
Phew, that was a lot.
I didn't think this would become such a large post.
Designing an event source system
on Postgres transactions is rather hard.
All I wanted to do is clear my thoughts on the matter,
but that grouping issue is another bug I just found
by writing about this 😅.

I think the biggest lesson I've (re)learned from 
the deadlock bug itself is to make sure you
reproduce an issue first before diving into solutions.
Even nasty business threatening system level bugs like these can
sometimes be solved with some minor modifications to the system.
If we had skipped this small step of reproducing the issue,
we may have focused on the CPU observation and moved 
pack ingestation to a separate machine,
which would've taken weeks to implement
and not solve anything.

Furthermore, it's humbling to see that even after having
used relational databases for more then a decade,
I still can learn new things about them.
For example that the auto increment sidesteps the Postgres
transaction was quite shocking to me.

I made a [github repository](https://github.com/jappeace/MAHDB)
for playing around with the queries more easily.
I hope you enjoyed this article, please leave 
a comment if you have any questions or suggestions below.

## Resources

+ The [code](https://github.com/jappeace/MAHDB) in this [blogpost](https://github.com/jappeace/MAHDB)
+ Postgres [Lock monitoring](https://wiki.postgresql.org/wiki/Lock_Monitoring)
+ [Blogs](https://garba.org/posts/2016/event-sourcing/#materialised-view-pattern) on [event sourcing](https://www.ahri.net/2019/07/practical-event-driven-and-sourced-programs-in-haskell/)
+ Presentation on [event sourcing](https://www.youtube.com/watch?v=8JKjvY4etTY)
