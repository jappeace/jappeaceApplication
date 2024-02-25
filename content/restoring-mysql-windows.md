Title: Restoring mysql innodb on windows.
Date: 2022-06-16 22:30
Modified: 2023-01-17 17:00
Category: tools
OPTIONS: toc:nil
Tags: programming, windows, mysql, database

Over the weekend a company had a power outage,
causing corruption to the on premise hosted mysql innodb database.
This means the company can't do any work,
so I had to fix this fast before they opened again on Monday.
It's not trivial because the system runs on
windows, which means the online guides don't quite work.
Therefore I wrote these notes for future me.

We can boot the database with [force recovery](https://dev.mysql.com/doc/refman/5.6/en/forcing-innodb-recovery.html)
In my case I had to set it to 3, try as low as possible.
It puts everything in read only but that's good enough.
The recovery is a process of exporting these read only databases
to sql,
creating an entire new instalation with a new datafolder,
and reading these back into that.
The linux instruction can be found [here](https://dba.stackexchange.com/questions/65728/forcing-innodb-recovery-of-a-corrupted-database).
This post is specifically about windows.
We need to run from cmd not powershell,
powershell doesn't understand `<`, and I've doubts about it understanding `>`.

To get a working installation I set `innod_force_recovery = 3` in `mysql\bin\my.ini`,
which can be found in xampp.
First we have to restart mysql so we can get the data out.
Now we can dump the tables:

```cmd
./mysqldump.exe -u root -pwhatever --skip-lock-tables -A > all.sql
```
And shut down the server.

```cmd
./mysqladmin -u root -pwhatever shut
```

I just emptied the datafolder with explorer,
by navigating to `T:/xampp/mysql/data`, and deleting everything in there.
Now I ran:

```bash
./mysql_istall_db.exe --datadir="T:\xampp\mysql\data"
```

I set `innod_force_recovery` to a comment again
and restart mysql from xampp,
I also checked if it ran in process monitor.

set the root password correctly again:

```bash
mysql -u root
SET PASSWORD FOR 'root'@'localhost' = PASSWORD('whatever');
FLUSH PRIVILEGES;
```
restore:

```bash
mysql -u root -pwhatever < all.sql
```

