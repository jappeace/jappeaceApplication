Title: Announcement: Esqueleto textsearch & postgiss
Date: 2024-02-25 15:01
Category: tools
OPTIONS: toc:nil
Tags: programming, haskell, database

I've updated the esqueleto bindings for
esqueleto-textsearch to include [tutorials](https://hackage.haskell.org/package/esqueleto-textsearch#tutorial) and
[documentation](https://hackage.haskell.org/package/esqueleto-textsearch-1.1.4/docs/Database-Esqueleto-TextSearch.html) so it no longer requires a 
guesswork on how to use this.
Furthermore I've also created new [esqueleto bindings for PostGIS](https://hackage.haskell.org/package/esqueleto-postgis).

[Esqueleto](https://hackage.haskell.org/package/esqueleto) 
is a more advanced query library that builds on top of the [persistent](https://hackage.haskell.org/package/persistent) ORM[^object-relational].
[Postgres text search](https://rachbelaid.com/postgres-full-text-search-is-good-enough/) brings Elasticsearch like functionality to postgres.

Roughly four years ago I had drafted out an implementation for the admin pages for some company.
This was part of my "trial" 2 days, where you just work as a "technical interview". [^great-for-me]
Ironically the admin pages ended up having far better search than the main app,
and implementing proper text search to the main app was never prioritized.
A colleague of mine had ported that to the [current library](https://hackage.haskell.org/package/esqueleto-textsearch).
However, I completely had forgotten how it worked and there was essentially [no documentation](https://hackage.haskell.org/package/esqueleto-textsearch-1.0.0.3/docs/Database-Esqueleto-TextSearch-Language.html). [^hackage-upload]
Since I was somehow the maintainer of this package I decided to just fix this for all Haskellers.

PostGIS has a different motivation.
My current contract is about finding stuff.
Now I could do this with some geometry library, 
but considering most logic is already in the database, 
I decided it was worth a try to see if postgis would be a good fit.
There was already some existing art done, such as [reading](https://hackage.haskell.org/package/wkt-geom-0.0.12/docs/Data-Ewkb.html#v:parseHexByteString) from the database,
but there was no off-the-shelf, functional writing.
I initially handcrafted some haphazard persistent instances to see if a full solution was feasible.
It worked and
one interesting outcome is that I could combine this postgis implementation with the text-search!
So we've named locations on a map, and objects within the database have some point on the map,
we can now search for some location on the map and find the objects trough normal full-text search.
The database does all the thinking.
Because this worked so well, I got motivated to do the full bindings, 
and prevent the issue I just had with textsearch.

I'm slowly transforming into a database engineer 😅.
Please let me know if you've any comments or suggestions.
Furthermore, if you need help with database woes, please let me know :).

[^great-for-me]: I perform way better in such a situation then a take-home test, because it's incredibly hard for me to get motivated to do throw away work, but here I did something useful.
[^hackage-upload]: Note that the hackage upload was also done by me, because my dear colleague had ported it to a library which wasn't even published! It was just some github repository. After a couple years I decided to just upload it, right before I jumped ship to another company.

[^object-relational]: Object relational mapping
