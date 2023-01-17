Title: Restoring mysql innodb on windows.
Date: 2023-01-17 17:00
Category: tools
OPTIONS: toc:nil
Tags: programming, windows, mysql

The backup scripts on the main server stopped working.
The IT person at that company
had already figured out this was because
of the database not booting on the machines which were
supposed to receive the backups,
but figuring out the underlying cause was beyond his capability.
So I was called in.
Yes. I'm slowly turning into a database administrator
at [this](https://jappie.me/restoring-mysql-innodb-on-windows.html) [rate](https://jappie.me/the-peculiar-event-sourced-deadlock.html).

So xampp[^why-xampp] told us to look at the logs, those said:
```
error: trying to access page number 123 in space 0
```
Stack overflow [said](https://stackoverflow.com/questions/38245974/mysql-server-crashes-innodb-outside-the-tablespace-bounds)
that we need to recover from backup if `innodb_force_recovery`
doesn't work.
It doesn't say how.
Remember, we can't even start the database!
What I did instead was deleting the innodb0 file, and the innodb.log files.
Remember, I actually don't care about the data,
it's just a backup machine.
This made the database boot again,
making it ready to receive backups.

One more peculiarity occurred (of course).
The script would timeout saying it's a connection error.
However this error happened every time I executed the script,
which made me think it was just mislabeling the error.
Once again [stack overflow](https://stackoverflow.com/a/24555191) agreed.
This database has lots of blob data,
so setting `max_allowed_packet` to `64M` indeed fixed the connection
error.

I'm really starting to think I should move this entire
stack over to postgres,
at least that doesn't randomly start failing,
or has mislabeled errors.
But I know that's next to impossible with this clients'
resources.
So for now, Jappie is here for all your database concerns.

[^why-xampp]: Why are they using xampp? Legacy choices.
              Remember I don't get to chose their tech stack,
              that has been decided more then a decade ago.
              I'm just keeping it operational.
