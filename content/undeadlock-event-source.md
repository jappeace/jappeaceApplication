TITLE: The peculiar event sourced deadlock
DATE: 2023-01-14
CATEGORY: reflection
Tags: postgres deadlock programming
Status: draft

![THE UNDEAD LOCK OF DETH](images/2023/postgresql-deadlock.png)

This is an after action report of a complicated
system level bug.
It took me a week to come to a satisfying
solution.
To start I need to sketch context.
Consider an event source system:

```sql
create table event (
        id SERIAL primary key NOT NULL,
        payload jsonb NOT NULL,
        type character varying NOT NULL,
        created timestamp with time zone NOT NULL
);


create table event_last_applied(
    id SERIAL primary key NOT NULL,
    event_id bigint NOT NULL
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

insert into event(payload,type,created) values (
  '{"email":"hi@jappie.me"}', /* whatever event source data*/
  'create-user',
  now());
insert into event_last_applied(id, event_id)
select 1, max(id) from event
 on conflict (id) do
 update set event_id=lastval();
;

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
select type,payload from event
where id > (
  select event_id from event_last_applied where id=1
) order by id asc;
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
begin;

/* projection code, insert user into tables here */

insert into event(payload,type,created) values (
  '{"email":"hi@jappie.me"}', /* whatever event source data*/
  'create-user',
  now());
insert into event_last_applied(id, event_id)
select 1, max(id) from event
 on conflict (id) do
 update set event_id=lastval();
;

/* projection code, connect user to company */

insert into event(payload,type,created) values (
  '{"company-id":2, "user-id": 1}', /* whatever event source data*/
  'connect-company',
  now());
insert into event_last_applied(id, event_id)
select 1, max(id) from event
 on conflict (id) do
 update set event_id=lastval();
;

commit;
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
consider connection A and B.
1. A opens a transaction and inserts a user, but has to do other stuff as well
2. B opens a transaction and wants to insert an event,
   he has to wait until A completes.
3. A completes.
4. B can now continue to complete.

So not a deadlock as long as A completes.
But since our transactions can go arbitrarily big,
and say we're inserting millions of rows of data,
`B` can wait a long time,
perhaps even 30 minutes.
This is far beyond the http session length,
or whatever any user finds acceptable.

This was indeed the production bug encountered at supercede.
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

The big boss came with another idea.
Instead of doing the insertion in a large transaction,
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
create table event (
        id SERIAL primary key NOT NULL,
        payload jsonb NOT NULL,
        type character varying NOT NULL,
        created timestamp with time zone NOT NULL
);

create table event_applied(
    id SERIAL primary key NOT NULL,
    event_id bigint NOT NULL references event(id),
    created timestamp with time zone NOT NULL
    );

```
The big difference is that we renamed 
`event_last_applied` to `event_applied`
and added a created field.

Now inserting events is also quite similar:
```
begin;

/* projection code, insert user into tables here */

insert into event(payload,type,created) values (
  '{"email":"hi@jappie.me"}', /* whatever event source data*/
  'create-user',
  now());
insert into event_applied(event_id, created)
SELECT last_value, now() FROM event_id_seq;

commit;
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
select type,payload from event as e
where not exists (
    select 1 from event_applied where event_id=e.id
  ) order by id asc;
```
We compare the event table to the event_applied table,
and return any events that don't exist in that.
We're still ordering by id to ensure the correct order.

This works, and allows arbitrary sized transactions to
project alongside each-other.

You'd say the auto-incrementing rows are also an issue.
However this is not the case,
because apparently `nextval`
of postgres works outside of a transaction.
There is no need to replace the id with uuid's
and figure out event order based on dates.
Although this is also possible.

## Conclusion
I think the biggest lesson I've learned from this whole
sharade is to make sure you reproduce an issue first,
before diving into solutions.
Even nasty system level bugs like these can be solved
with some minor modifications to the system.
I think the initial solutions we talked about
were about moving pack ingestation to a sepate machine,
which would've taken weeks to implement
and not actually solve anything.

Furthermore, the big boss man isn't always right.
Yes chucnking transactions would've worked,
but it would've taken much longer to implement and get correct,
and that would've only worked for this particular transaction.
As the system would've grown, more transactions woudl've popped
up requiring even more engineering effort.
Solving the problem at the root just saves so much effort.

