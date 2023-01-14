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

/* insert user into tables here */

insert into event(payload,type,created) values (
  '{"email":"hi@jappie.me"}', /* whatever event source data*/
  'create-user',
  now());
insert into event_last_applied(id, event_id)
select 1, max(id) from event
 on conflict (id) do
 update set event_id=lastval();
;

/* connect user to company */

insert into event(payload,type,created) values (
  '{"company-id":2, "user-id": 1}', /* whatever event source data*/
  'connect-company,
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

pt 1 delaying


pt 2 intercarlation

pt 3 lockless

## Conclusion
