Title: Pragmatic Haskell III: Beam Postgres DB
Date: 2018-08-05 17:30
Category: tools
OPTIONS: toc:nil
Tags: haskell, programming, tools, database
subreddit: haskell programming

No need to read a book to use Haskell!
This post will get you going with a serious web application while
only sticking to the concepts that are encountered.
This is a Haskell safari with as end goal a working webapp with database.

1. [Pragmatic Haskell: Simple servant web server]({filename}/pragmatic-haskell-simple-servant.md)
1. [Pragmatic Haskell II: IO Webservant]({filename}/pragmatic-haskell-message-servant.md)
1. [Pragmatic Haskell III: Beam Postgres DB]({filename}/pragmatic-haskell-database.md)

![fancy db image](/images/2018/haskell-beam-postgres.svg)

Web applications need to store data.
In the [previous blog post]({filename}/pragmatic-haskell-message-servant.md)
we did this in a file for simplicity.
Now we will use something more appropriate: A relational database.
The beam library is used for this because it is closest to the "ORM" way of
thinking: Model a schema, generate SQL to query that schema,
and have migrations to move between different versions of that schema.
Migrations are left for another post for simplicity.

[For the inpatient: Resulting source](#complete-sources)

# Preparation
Unfortunately this post requires us to do quite a bit of devops to get started.
We need to:

1. Install Postgres
2. Create a user
3. Create db
4. Populate structure

Installing Postgres is out of the scope of this post.
We sidestep using migrations for with the `data_model.sql` file
[(see sources)](#data_modelsql).
Use the following commands to prepare the database:

```bash
sudo -u postgres createuser -s $USER
dropdb awesome_db
createdb awesome_db
psql -f ./data_model.sql -d awesome_db
```

Congratulations, devops was survived.
Note that using this sql file is not idiomatic to beam.
The schema should be managed by beam,
but getting migrations to function is currently hard (it's a work in progress).

# Creating structure
The beam library models our desired structure at type level.
This is done in a separate file called `DB.hs`.
It can be seen in [the sources](#dbhs).
With help of this code beam can inspect the definitions,
and it also provides type safety for the beam sql domain specific language.
In other words if the migrations work there would be a path towards bringing
the database up to date with the code base,
or a compile error.
This is very pleasant because we get a thight feedback loop.
Now let us carefully inspect that file to understand it.

## Language extensions
```haskell
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE StandaloneDeriving    #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE DuplicateRecordFields #-}
```
This looks daunting however these extensions are individually quite simple.
Generally they either make nicer API's or an easier language to use.
We will go over each of them.

### StandaloneDeriving
`StandaloneDeriving` allows us to use the derive mechanism outside of a data
declaration, for example:

```haskell
deriving instance Show Message
```

This mechanism allows deriving (automatic code generation)
to be used more flexibly.
In this case we want to do this because the `MessageT f` type constructor
to derive (as `f` is unknown), but `MessageT Identity` is known 
so we can derive that.
The GHC [manual lists](https://downloads.haskell.org/~ghc/latest/docs/html/users_guide/glasgow_exts.html#stand-alone-deriving-declarations)
more possible reasons to derive like this instead of the standard method.

### TypeFamilies
Type families allow us to declare `data` inside an instance.
The `Table` class requires `TypeFamilies` to instantiate because
it needs a `data` called primary key in it's instance:

```haskell
instance Table UserT where
    data PrimaryKey UserT f = UserId (Columnar f Int) deriving Generic
    primaryKey = UserId . _id
```
This example is a prime reason to use type families:
It allows beam to assume the primary key exist for all tables.

The Haskell [wiki](https://wiki.haskell.org/GHC/Type_families)
goes more in depth on type families

### FlexibleInstances
If we don't enable `FlexibleInstances` we get the following error:

```
/home/jappie/projects/haskell/awesome-project-name/src/DB.hs:36:10: error:
    • Illegal instance declaration for ‘Beamable (PrimaryKey MessageT)’
        (All instance types must be of the form (T a1 ... an)
         where a1 ... an are *distinct type variables*,
         and each type variable appears at most once in the instance head.
         Use FlexibleInstances if you want to disable this.)
    • In the instance declaration for ‘Beamable (PrimaryKey MessageT)’
   |
36 | instance Beamable (PrimaryKey MessageT)
   |          ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
```

Without Flexible instances parenthesis aren't allowed.
We know the parenthesis are the problem because the following line
does not get an error:

```haskell
instance Beamable MessageT
```

### MultiParamTypeClasses
If `MultiParamTypeClasses` is disabled an error appears:

```
/home/jappie/projects/haskell/awesome-project-name/src/DB.hs:65:10: error:
    • Illegal instance declaration for ‘Database be AwesomeDb’
        (Only one type can be given in an instance head.
         Use MultiParamTypeClasses if you want to allow more, or zero.)
    • In the instance declaration for ‘Database be AwesomeDb’
   |
65 | instance Database be AwesomeDb
   |          ^^^^^^^^^^^^^^^^^^^^^
```

Because we use two parameters for this instance (`be` and `AwesomeDb`).
By default Haskell only allows one.

### DeriveGeneric
`DeriveGeneric` was discussed in a
[previous blog post]({filename}/pragmatic-haskell-simple-servant.md).
In short: `Generic` allows for introspection of data structures using the
fact any data structure can be modeled in a regular (generic) pattern.

### OverloadedStrings
`OverloadedStrings` is probably the most common language extension.
It converts string automatically, for example `String -> ByteString`.
In our case it's only used for connection string:

```haskell
connectionString :: BS.ByteString
connectionString = "dbname=awesome_db"
```

In this case it inserts automatically a function `String -> ByteString`.
Using this extension avoids tedious conversions.

### DuplicateRecordFields
`DuplicateRecordFields` allows creation of records with the same name.
For example both user and messages have an `_id` record.
Because they have the same name,
type annotations are used to determine which function is called,
for example in:

```haskell
instance Table UserT where
    data PrimaryKey UserT f = UserId (Columnar f Int) deriving Generic
    primaryKey = UserId . (_id :: UserT f -> C f Int)
```

## Imports
```haskell
import qualified Data.ByteString                  as BS
```
ByteString is required for the type signature of connection string.

```haskell
import qualified Data.Text                      as Text
```
Text is required for the column datatype.

```haskell
import           Database.Beam
```
We import all of beam, for convenience.
This entire module is a client of beam, there is no need
for explicit imports.

## User table
We start with defining the structure of our user table.
```haskell
data UserT f = User
                { _id     :: C f Int
                , _name   :: C f Text.Text
                , _email  :: C f Text.Text
                }
                  deriving Generic
```
What are these `C` and `f`'s doing here?
The `C` is an abbreviation for
[Columnar](http://hackage.haskell.org/package/beam-core-0.7.2.2/docs/Database-Beam-Schema.html#t:Columnar),
which is a type that requires two other types to complete.
In this case `C` is given an `f`,
and a second argument with the actual type of the column.
`f` is not defined, instead it's also an argument of `UserT`, therefore `UserT`
is of kind `* -> *`. What we know however is that this `f` is the same for all
columns in `UserT`.

Now this `f` can be thought of as a 'gap' that can be filled up with anything.
This gap allows the beam library to inspect the structure we have defined,
and therefore create a schema out of it and hold the data of a row.
The [hackage page](https://hackage.haskell.org/package/beam-0.3.0.0/docs/Database-Beam-Schema.html#g:2)
goes deeper into the definition.

```haskell
type User = UserT Identity
```
This defines a type alias. A userT with Identity is simply a user.
In this case we are filling the `f` with Identity, [a container](http://hackage.haskell.org/package/base-4.11.1.0/docs/Data-Functor-Identity.html)
that exposes on function `runIdentity` which removes the container and
does nothing with the content.
This is usefull for the case where we want to have the user as a result from
the database.

```haskell
deriving instance Show (PrimaryKey UserT Identity)
deriving instance Show User
```
Implement the `show` function automatically for the identity case and for the
primary key which will be introduced below.

```haskell
instance Table UserT where
    data PrimaryKey UserT f = UserId (Columnar f Int) deriving Generic
    primaryKey = UserId . (_id :: UserT f -> C f Int)
type UserId = PrimaryKey UserT Identity -- For convenience
```
Here we make UserT an instance of a Table.
To do this we must specify a PrimaryKey, of which we define the type in the data
line.
The next line tells which function must be used to access the primary key.
In other words we implement the primaryKey function here by saying `_id`
must be used for it.
We use a type annotation to indicate which `_id` function is used.
Leaving that out results in an ambigious type error.

```haskell
instance Beamable UserT
instance Beamable (PrimaryKey UserT)
```

`Beamable` provides several ['introspection routines'](http://hackage.haskell.org/package/beam-core-0.7.2.2/docs/Database-Beam-Schema.html#t:Beamable).
We require it to create a `Database` out of the table.
The database will be described below.

## Message table

```haskell
data MessageT f = Message
                { _id        :: C f Int
                , _from      :: PrimaryKey UserT f
                , _content   :: C f Text.Text
                }
                  deriving Generic
type Message = MessageT Identity
deriving instance Show (PrimaryKey MessageT Identity)
deriving instance Show Message

instance Table MessageT where
    data PrimaryKey MessageT f = MessageId (Columnar f Int) deriving Generic
    primaryKey = MessageId . (_id :: MessageT f -> C f Int)
type MessageId = PrimaryKey MessageT Identity -- For convenience

instance Beamable MessageT
instance Beamable (PrimaryKey MessageT)
```

The boiler plate is similar to that of User, the only new concept is the
from field, which points at the user table with the primary key.
Beam can make joins on this with the DSL.

## Database
```haskell
data AwesomeDb f = AwesomeDb
                      { _users    :: f (TableEntity UserT)
                      , _messages :: f (TableEntity MessageT) }
                        deriving Generic
```
This type defines the entire database.
Again it provides a 'hole' with the `f`.

```haskell
connectionString :: BS.ByteString
connectionString = "dbname=awesome_db"
```
The `connectionString` is required to connect to the database.
One probably doesn't want to hard code it,
but for this guide hard coding is good enough.

```haskell
instance Database be AwesomeDb
```
Here we create a beam database out of the AwesomeDB type.
the `be` hole is reserved for a back end, which we don't specify.

```haskell
awesomeDB :: DatabaseSettings be AwesomeDb
awesomeDB = defaultDbSettings
```
The implementation of AwesomeDB just uses the default database settings.
All structural information is already provided at type level.

# Using structure
Now we have a database structure defined we can use it
in `Lib.hs`.
We have already seen most of this source file in the previous [blog post]({filename}/pragmatic-haskell-message-servant.md),
the new version can be seen in [the sources](#dbhs).

The functionality is still the same except now we're using a database as
backend rather than a file.
Just like with the previous post, the example shows both how to insert,
as well as retrieve data.
Let's inspect the new changes.

```haskell
messages :: Connection -> Message -> Handler [Message]
messages conn message = do 
  messages <- liftIO $ 
```
Messages will hold the resulting messages we're to querying from the database.
`liftIO` allows functions within the `IO` context (eg, interact with the world).

```haskell
    PgBeam.runBeamPostgres conn $ do
```
Run the beam Monad with help of a connection.
In other words, everything within this do block is a query for the database,
and we're explicitly using postgres to solve ambiguity.

```haskell
      let user = from message
```
Retrieve the user from message for convenience.

```haskell
      [user] <- runInsertReturningList (DB._users DB.awesomeDB) $ Beam.insertExpressions [DB.User{
            DB._userId = Beam.default_,
            DB._name = Beam.val_ (pack $ name $ user ),
            DB._email = Beam.val_ (pack $ email $ user )
        }]
```
the `runInsertReturningList` function call is quite complex.
The first argument defines in which table we're using, we want to insert
something into the users table.
The second argument is a list of expressions. To get the expressions we use the
`Beam.insertExpressions` function.
This is how we insert an item, in this case we only want to insert one user.
We use the User constructor defined earlier in the `DB.hs` module to obtain a user.
The fields are populated with values or a special default value.

Note that although this function is complex, if we do anything wrong we get a
type error.
Our code will not compile unless we do it right.
This is one of the strengths of beam.

```haskell
      _ <- runInsertReturningList (DB._messages DB.awesomeDB) $ Beam.insertExpressions $ [DB.Message{
            DB._messageId = Beam.default_,
            DB._from = Beam.val_ (Beam.pk user),
            DB._content = Beam.val_ (pack $ content message)
        }]
```
These lines insert the message into the db,
linking it up with the newly inserted
user trough the pk.

```haskell
      Beam.runSelectReturningList $ Beam.select $ do 
        usr <- (Beam.all_ (DB._users DB.awesomeDB))
        msg <- Beam.oneToMany_ (DB._messages DB.awesomeDB) DB._from usr
        pure (msg, usr)
```
This query gets the resulting messages and their respective users joined
together.

```haskell
  pure $
    fmap (
      \(msg, usr) -> Message
        (User
          (unpack $ DB._name usr)
          (unpack $ DB._email usr))
        (unpack $ DB._content msg)
    ) messages
```
here we convert the database user and database message, to the 'API' user and
'API' messages.
The reason we need to do this is because our database data structure does not
implement `toJSON`.
Also the database structure has extra information such as the primary key which
we may want to hide from API clients.

# Execute!
To run the program we use:
```bash
   stack build
   stack exec webservice
```

To test it a simple curl request was made:
```bash
   curl --header "Content-Type: application/json" -v --data '{"from":{"email":"d","name":"xyz"}, "content": "does it word?"}' http://127.0.0.1:6868/message/ 
```

We can inspect the database with postgres

```bash
   psql "dbname=awesome_db"
   \dt
   select * from messages;
```

# Conclusion
We have looked at the beam library in this post and it's interaction with
postgres.
Although the example is simple, there is quite a bit of boilerplate involved,
but once setup it provides a complete type safe DSL to the database.
With the database and web server in place nothing is stopping the reader from
making his next major project in Haskell!
We hereby conclude our Haskell safari successfully. 


# Complete sources
The complete sources can be found on [github](https://github.com/jappeace/awesome-project-name/tree/beam-postgre-no-migrate), and below.

## Db.hs
```haskell
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE StandaloneDeriving    #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE DuplicateRecordFields #-}

-- | db structure and source of truth
module DB where
import qualified Data.ByteString                as BS
import qualified Data.Text                      as Text
import           Database.Beam


data UserT f = User
                { _id :: C f Int
                , _name   :: C f Text.Text
                , _email  :: C f Text.Text
                }
                  deriving Generic
type User = UserT Identity
deriving instance Show UserId
deriving instance Show User

instance Table UserT where
    data PrimaryKey UserT f = UserId (Columnar f Int) deriving Generic
    primaryKey = UserId . (_id :: UserT f -> C f Int)
type UserId = PrimaryKey UserT Identity -- For convenience

instance Beamable UserT
instance Beamable (PrimaryKey UserT)

  
data MessageT f = Message
                { _id :: C f Int
                , _from      :: PrimaryKey UserT f
                , _content   :: C f Text.Text
                }
                  deriving Generic
type Message = MessageT Identity
deriving instance Show (PrimaryKey MessageT Identity)
deriving instance Show Message

instance Table MessageT where
    data PrimaryKey MessageT f = MessageId (Columnar f Int) deriving Generic
    primaryKey = MessageId . (_id :: MessageT f -> C f Int)
type MessageId = PrimaryKey MessageT Identity -- For convenience

instance Beamable MessageT
instance Beamable (PrimaryKey MessageT)


data AwesomeDb f = AwesomeDb
                      { _ausers    :: f (TableEntity UserT)
                      , _messages :: f (TableEntity MessageT) }
                        deriving Generic

connectionString :: BS.ByteString
connectionString = "dbname=awesome_db"

instance Database be AwesomeDb

awesomeDB :: DatabaseSettings be AwesomeDb
awesomeDB = defaultDbSettings
```

## Lib.hs
```haskell
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DeriveGeneric #-}

module Lib
    ( webAppEntry
    ) where

import Servant
import Control.Monad.IO.Class(liftIO)
import Data.ByteString.Lazy as LBS (writeFile, readFile) 
import Data.Aeson(ToJSON, FromJSON, encode, decode)
import GHC.Generics(Generic)
import Network.Wai(Application)
import Network.Wai.Handler.Warp(run)
import           Database.PostgreSQL.Simple   (Connection)
import qualified DB as DB
import           Database.Beam.Backend.SQL.BeamExtensions (runInsertReturningList)

import qualified Database.Beam                            as Beam
import qualified Database.Beam.Postgres                            as PgBeam
import Data.Text(pack, unpack)

type UserAPI = "users" :> Get '[JSON] [User]
      :<|> "message" :> ReqBody '[JSON] Message :> Post '[JSON] [Message]

data Message = Message {
  from :: User,
  content :: String
} deriving (Eq, Show, Generic)

instance ToJSON Message
instance FromJSON Message

data User = User
  { name :: String
  , email :: String
  } deriving (Eq, Show, Generic)

instance ToJSON User
instance FromJSON User

users :: [User]
users =
  [ User "Isaac Newton"    "isaac@newton.co.uk"
  , User "Albert Einstein" "ae@mc2.org"
  ]

messages :: Connection -> Message -> Handler [Message]
messages conn message = do 
  messages <- liftIO $ 
    PgBeam.runBeamPostgres conn $ do
      let user = from message
      [user] <- runInsertReturningList (DB._ausers DB.awesomeDB) $ 
          Beam.insertExpressions [DB.User 
            Beam.default_
            (Beam.val_ (pack $ name $ user ))
            (Beam.val_ (pack $ email $ user ))
        ]
      _ <- runInsertReturningList (DB._messages DB.awesomeDB) $ 
          Beam.insertExpressions 
            [DB.Message 
              Beam.default_ 
              (Beam.val_ (Beam.pk user))
              (Beam.val_ (pack $ content message))
            ]
      Beam.runSelectReturningList $ Beam.select $ do 
        usr <- (Beam.all_ (DB._ausers DB.awesomeDB))
        msg <- Beam.oneToMany_ (DB._messages DB.awesomeDB) DB._from usr
        pure (msg, usr)
  pure $
    fmap (
      \(msg, usr) -> Message
        (User
          (unpack $ DB._name usr)
          (unpack $ DB._email usr))
        (unpack $ DB._content msg)
    ) messages


server :: Connection -> Server UserAPI
server conn= (pure users) :<|> (messages conn)

userAPI :: Proxy UserAPI
userAPI = Proxy

app :: Connection -> Application
app conn = serve userAPI (server conn)

webAppEntry :: Connection -> IO ()
webAppEntry conn = do
  run 6868 (app conn)
```
## data_model.sql
``` SQL
DROP TABLE ausers cascade;
DROP TABLE messages cascade;
CREATE TABLE ausers (
    id serial NOT NULL PRIMARY KEY,
    "name" varchar NOT NULL,
    email varchar NULL
);

CREATE TABLE messages (
    id serial NOT NULL PRIMARY KEY,
    from__id int REFERENCES ausers(id),
    content varchar NULL
);
```
