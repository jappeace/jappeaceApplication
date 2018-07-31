Title: Pragmatic Haskell III: Beam Postgres DB
Date: 2018-06-26 12:00
Category: tools
OPTIONS: toc:nil
Tags: haskell, programming, tools, database
subreddit: haskell programming
status: draft

No need to read a book to learn Haskell!
This guide will get you going with a serious web application while
sticking to only the concepts that are encountered.

1. [Pragmatic Haskell: Simple servant web server]({filename}/pragmatic-haskell-simple-servant.md)
1. [Pragmatic Haskell II: IO Webservant]({filename}/pragmatic-haskell-message-servant.md)
1. [Pragmatic Haskell III: Beam Postgres DB]({filename}/pragmatic-haskell-database.md)

![fancy db image](/images/2018/haskell-beam-postgres.svg)

Web applications need to store data.
Often relational databases are used for this purpose, so will we.
They also fit nice together with strong typing.
In this post we will describe how to attach a database to our existing webserver,
still assuming the reader has no previous Haskell experience.

[For the inpatient: Resulting source](https://github.com/jappeace/awesome-project-name/tree/beam-postgres-db)

# Preperation
Unfortunately this post requires us to do quite a bit of devops to get started.
We need to:

1. Install Postgres
2. Create a user
3. Create db

It is up to the reader to install Postgres on his configuration.
This surely will be painful, we wish them good luck.
However some <s>spells</s> commands for creating the user and database are given,
we can create a super user in Postgres for our main account:

```bash
sudo -u postgres createuser -s $USER
dropdb awesome_db
createdb awesome_db
```

Congratulations, devops was survived.

# Creating structure
In line with our previous guide we will first show all code and then step trough
it line by line.
This time we need to handle multiple files however so we split up this task.
It's best this time to consult the
[full sources](https://github.com/jappeace/awesome-project-name/tree/beam-postgres-db)
for a working example.
To represent our database the beam library is used.
It defines an organized way of modeling our database fields.
We do this in a seperate file called `DB.hs`:

```haskell
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE StandaloneDeriving    #-}
{-# LANGUAGE TypeFamilies          #-}

-- | db structure and source of truth
module DB where
import qualified Data.ByteString                as BS
import qualified Data.Text                      as Text
import           Database.Beam


data UserT f = User
                { _userId :: C f Int
                , _name   :: C f Text.Text
                , _email  :: C f Text.Text
                }
                  deriving Generic
type User = UserT Identity
deriving instance Show UserId
deriving instance Show User

instance Table UserT where
    data PrimaryKey UserT f = UserId (Columnar f Int) deriving Generic
    primaryKey = UserId . _userId
type UserId = PrimaryKey UserT Identity -- For convenience

instance Beamable UserT
instance Beamable (PrimaryKey UserT)

  
data MessageT f = Message
                { _messageId :: C f Int
                , _from      :: PrimaryKey UserT f
                , _content   :: C f Text.Text
                }
                  deriving Generic
type Message = MessageT Identity
deriving instance Show (PrimaryKey MessageT Identity)
deriving instance Show Message

instance Table MessageT where
    data PrimaryKey MessageT f = MessageId (Columnar f Int) deriving Generic
    primaryKey = MessageId . _messageId
type MessageId = PrimaryKey MessageT Identity -- For convenience

instance Beamable MessageT
instance Beamable (PrimaryKey MessageT)


data AwesomeDb f = AwesomeDb
                      { _users    :: f (TableEntity UserT)
                      , _messages :: f (TableEntity MessageT) }
                        deriving Generic

connectionString :: BS.ByteString
connectionString = "dbname=awesome_db"

instance Database be AwesomeDb

awesomeDB :: DatabaseSettings be AwesomeDb
awesomeDB = defaultDbSettings
```

This code will do several things. First it will define how we want our database
to look. Secondly it provides machinary for beam to inspect these definitions.

## Langauge extensions
```haskell
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE StandaloneDeriving    #-}
{-# LANGUAGE TypeFamilies          #-}
```
This looks daunting however these are individually all quite simple,
they all just do one thing.
Generally they either make nicer API's or an easier language to use.
We will go over each of them.

### StandaloneDeriving
`StandaloneDeriving` allows us to use the derive mechanism outside of a data
declaration, for example:

```haskell
deriving instance Show Message
```

### TypeFamilies
`TypeFamilies` are required to instantiate the Table class.
The `Table` type class requires us to define a `data` called
primary key in it's instance:

```haskell
instance Table UserT where
    data PrimaryKey UserT f = UserId (Columnar f Int) deriving Generic
    primaryKey = UserId . _userId
```

The primary key is a member of the table "family",
which is defined concretely by it's instance.

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

It's trying to say it doesn't like parenthesis. We must enable Flexible
instances to allow parenthesis.
We know the parenthesis are the problem because the following line
does not get an error:

```haskell
instance Beamable MessageT
```

### MultiParamTypeClasses
If we disable `MultiParamTypeClasses` we get the following error:

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
Because we use two parameters for this instance (`be` and `AwesomeDb`)
we must lift the default restriction on this with this langauge extension.

### DeriveGeneric
`DeriveGeneric` was discussed in a
[previous blog post]({filename}/pragmatic-haskell-simple-servant.md).
In short: `Generic` allows for introspection of data structures since they can
be represented as a recursive structure.

### OverloadedStrings
`OverloadedStrings` is probably the most common language extension.
It converts string automatically, for example `String -> ByteString`.
In our case it's only used for connectionString:

```haskell
connectionString :: BS.ByteString
connectionString = "dbname=awesome_db"
```

You can do this without this extension, however finding the right conversion 
function takes up time and this extension does it for you, why bother?

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
```haskell
data UserT f = User
                { _userId :: C f Int
                , _name   :: C f Text.Text
                , _email  :: C f Text.Text
                }
                  deriving Generic
```
We start with defining the structure of our user table.
This is somewhat different as how we defined it in the previous post
as we now have these `C` and `f`'s popping up.
The `C` is an abbreviation for
[Columnar](http://hackage.haskell.org/package/beam-core-0.7.2.2/docs/Database-Beam-Schema.html#t:Columnar),
which is a type that accepts two
arguments. The first one being this `f`, the second is the actual type of the
column.

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
In this case we want to use UserT for just holding data and nothing else.

```haskell
deriving instance Show (PrimaryKey UserT Identity)
deriving instance Show User
```
Implement the `show` function automatically for the identity case and for the
primary key which will be introduced below.

```haskell
instance Table UserT where
    data PrimaryKey UserT f = UserId (Columnar f Int) deriving Generic
    primaryKey = UserId . _userId
type UserId = PrimaryKey UserT Identity -- For convenience
```
Here we make UserT an instance of a Table.
To do this we must specify a PrimaryKey, of which we define the type in the data
line.
The next line tells which function must be used to access the primary key.
In other words we implement the primaryKey function here by saying `_userId`
must be used for it.

Note that it is possible to call this primary key `_id`,
however the `{-# LANGUAGE DuplicateRecordFields #-}` langauge extension
must be used in this case if multiple models are defined in the same module.
Then also the type must be defined inline to allow the compiler to figure
out which `_id` record was used.
We left it out for simplicity

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
                { _messageId :: C f Int
                , _from      :: PrimaryKey UserT f
                , _content   :: C f Text.Text
                }
                  deriving Generic
type Message = MessageT Identity
deriving instance Show (PrimaryKey MessageT Identity)
deriving instance Show Message

instance Table MessageT where
    data PrimaryKey MessageT f = MessageId (Columnar f Int) deriving Generic
    primaryKey = MessageId . _messageId
type MessageId = PrimaryKey MessageT Identity -- For convenience

instance Beamable MessageT
instance Beamable (PrimaryKey MessageT)
```

The boiler plate is similar to that of User, the only new introduction is the
from field, which points at the user table.
Beam makes this automatically into a foreign key and can do joins on it
with the DSL, as we will see later.

## Database
```haskell
data AwesomeDb f = AwesomeDb
                      { _users    :: f (TableEntity UserT)
                      , _messages :: f (TableEntity MessageT) }
                        deriving Generic
```
This type defines the entire database.
Again it provides a 'hole' with the `f`, presumably for schema creation.

```haskell
connectionString :: BS.ByteString
connectionString = "dbname=awesome_db"
```
The `connectionString` is simply required to connect to the database.
One probably doesn't want to hard code it, but for this guide hard coding is
good enough.

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
in our already defined servant module `Lib.hs`.
We have already seen most of this source file in the previous [blog post]({filename}/pragmatic-haskell-message-servant.md),
the complete new version is listed below:

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
      [user] <- runInsertReturningList (DB._users DB.awesomeDB) $ Beam.insertExpressions [DB.User{
            DB._userId = Beam.default_,
            DB._name = Beam.val_ (pack $ name $ user ),
            DB._email = Beam.val_ (pack $ email $ user )
        }]
      _ <- runInsertReturningList (DB._messages DB.awesomeDB) $ Beam.insertExpressions $ [DB.Message{
            DB._messageId = Beam.default_,
            DB._from = Beam.val_ (Beam.pk user),
            DB._content = Beam.val_ (pack $ content message)
        }]
      Beam.runSelectReturningList $ Beam.select $ do 
        usr <- (Beam.all_ (DB._users DB.awesomeDB))
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

The functionality is still the same except now we're using a database as
backend rather than a file.
What's interesting about the example is that it shows both how to insert, as
well as retrieve data with help of beam.
It also does a `JOIN` operation.
This is enough to build a large backend system from.

## Messages
```haskell
messages :: Connection -> Message -> Handler [Message]
messages conn message = do 
  messages <- liftIO $ 
```

Messages will hold the messages we're going to query from the database.
```haskell
    PgBeam.runBeamPostgres conn $ do
```

Run the beam monad with help of a connection.
In other words, everything within this do block is a query for the database.
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
This is how we insert an item, beam always inserts lists of items, in our case 
we only want to insert one user.

```haskell
      _ <- runInsertReturningList (DB._messages DB.awesomeDB) $ Beam.insertExpressions $ [DB.Message{
            DB._messageId = Beam.default_,
            DB._from = Beam.val_ (Beam.pk user),
            DB._content = Beam.val_ (pack $ content message)
        }]
```
These lines insert the message into the db, linking it up with the newly inserted
user trough the pk.

```haskell
      Beam.runSelectReturningList $ Beam.select $ do 
        usr <- (Beam.all_ (DB._users DB.awesomeDB))
        msg <- Beam.oneToMany_ (DB._messages DB.awesomeDB) DB._from usr
        pure (msg, usr)
```
This query gets ll resulting messages and their respecive users joined together.

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
here we convertt the database user and database message, to the 'api' user and
'api' messages.
The reason we need to do this is because our database data scrturcture does not
implement toJSON.
Also the database structure has extra information such as the primary key which
we may want to hide from api clients.

# Execute!

