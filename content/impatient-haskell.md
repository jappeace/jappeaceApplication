Title: Pragmatic Haskell: Simple servant webserver
Date: 2018-06-25 12:00
Category: tools
OPTIONS: toc:nil
Tags: haskell, programming, tools
subreddit: haskell programming
status: draft

There are many [guides available](https://github.com/bitemyapp/learnhaskell)
for learning Haskell.
Setting up a webserver however something simple like a webserver isn't so
straight forward.
Perhaps choosing one of the [14 libraries](https://wiki.haskell.org/Web/Frameworks)
is a bit much.

![Type level hell: Haskell sucks](/images/2018/haskell-sucks.jpg)

This guide will give opinionated webserver start.
This guide assumes no experience with Haskell,
and will get you up to speed with a (rest) webserver called [servant](http://haskell-servant.readthedocs.io/en/stable/).
Servant is a good choice as it can describe both a server and client api.
In the future this guide may be used as a foundation to create something
more meaningful than just a REST api, it's a good start none the less.

# From nothing we start with build tools
Install stack:

```bash
curl -sSL https://get.haskellstack.org/ | sh
```

Only attempt shortly to install it trough a package manager.
More than 5 minutes is not worth it. (at some point it may become possible).
There are other build tools, they will be more difficult in use.
There is also the possibility for fully reproducible builds at a system level
(nix).
Which is out of the scope of this guide.

Now setup a new project:

```bash
stack new awesome-project-name 
cd awesome-project-name
```

# Hello world with stack
Lets first see what happens when we build this:

```bash
stack build && stack exec awesome-project-name-exe
```

This should build successfully and output `someFunc`.
Now lets dive into haskell! Open up `src/Lib.hs` with your favorite editor.
This contains a few lines, created by stack:

```haskell
module Lib
    ( someFunc
    ) where

someFunc :: IO ()
someFunc = putStrLn "someFunc"
```

This is where the someFunc output came from when we just ran the program.
Lets change it to something a bit more appropriate, and lets rename the function
too.

```haskell
module Lib
    ( webAppEntry
    ) where

webAppEntry :: IO ()
webAppEntry = putStrLn "This is the beginning of my greetings to world"
```

Does it compile?

```bash
stack build && stack exec awesome-project-name-exe

/home/jappie/projects/haskell/awesome-project-name/app/Main.hs:6:8: error: Variable not in scope: someFunc :: IO ()
  \|
6 \| main = someFunc
  \|        ^^^^^^^^

```

It does not compile.
There is an app folder where by default all the executable reside
(which is where the error occurs),
and a `src` folder where your library code lives (the modified file is in there).
Future proving yourself by putting as much code in the library as is reasonable
seems wise.

Let's fix the error in `app/Main.hs`:

```haskell
module Main where

import Lib

main :: IO ()
main = webAppEntry
```

It builds!
Functions can be renamed, simple compile errors can be solved, and strings
can be changed. Progress!

# Servant: Your first dependencies
It will be easy, don't worry!
For the impatient, there is a minimal example already [available](https://github.com/haskell-servant/example-servant-minimal)
by the library author.
This guide will explain how to get there step by step.
In `./package.yaml`, on line 22 there is a `dependencies` key,
add servant to it like this:

```yaml
dependencies:
- base >= 4.7 && < 5
- servant-server
- aeson
- wai
- warp 
```

It may seem strange to immediately add 4 new dependencies,
however this is because Haskell libraries are setup to be flexible.
Even small projects grow quickly to have into the 20's of dependencies.
Code reuse is not [a myth](https://www.youtube.com/watch?v=Jn3kdTaa69U).

`servant-server` is the [servant web server](http://haskell-servant.readthedocs.io/en/stable/).
[`aeson`](http://hackage.haskell.org/package/aeson)
is for JSON parsing and producing.
[`wai`](http://hackage.haskell.org/package/wai) is a web application interface and
[`warp`](http://hackage.haskell.org/package/warp) uses `wai`
to implement a web application (EG it binds to the port).
There are alternatives to all of these, we will not consider them.

Note that you should *not* add this to the dependencies of the executable or
library.
Stack provides a way of specifying dependencies of either of these.
However if we do it on line 22 (root of the yaml file),
it will be a dependency for everything in the project.

# A minimal servant
A good start is going to servants' [Hackage](http://hackage.haskell.org/package/servant)
page,
which linked to a [tutorial](http://haskell-servant.readthedocs.io/en/stable/tutorial/index.html).
Servant does API definition [at type level](http://haskell-servant.readthedocs.io/en/stable/tutorial/ApiType.html).
What the author of this post did was simply study the tutorial and then try to
get it to compile.
Then he stepped trough the result and gave a line by line
explanation of everything he deemed worthy of explanation.

If it's unknown to the reader what a type is, think of it as a shape we can
attach to a function,
functions of different shapes don't fit together, and won't compile.
What servant allows us to do is define this shape for a REST API.
Let's look at a concrete example line by line.
These are the 'magic spells' required to setup a minimal servant (`Lib.hs`) server:

```haskell
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DeriveGeneric #-}

module Lib
    ( webAppEntry
    ) where

import Servant(serve, Proxy(..), Server, JSON, Get, (:>))
import Data.Aeson(ToJSON)
import GHC.Generics(Generic)
import Network.Wai(Application)
import Network.Wai.Handler.Warp(run)

type UserAPI = "users" :> Get '[JSON] [User]

data User = User
  { name :: String
  , email :: String
  } deriving (Eq, Show, Generic)

instance ToJSON User

users :: [User]
users =
  [ User "Isaac Newton"    "isaac@newton.co.uk"
  , User "Albert Einstein" "ae@mc2.org"
  ]

server :: Server UserAPI
server = return users

userAPI :: Proxy UserAPI
userAPI = Proxy

app :: Application
app = serve userAPI server

webAppEntry :: IO ()
webAppEntry = run 6868 app
```

## Language extensions
The first three lines are languages extensions,
Haskell behaves different for this module according to these.
We can temporary delete data kinds to see what happens:

```bash
/home/jappie/projects/haskell/awesome-project-name/src/Lib.hs:14:16: error:
    Illegal type: ‘"users"’ Perhaps you intended to use DataKinds
   |
14 | type UserAPI = "users" :> Get '[JSON] [User]
   |                ^^^^^^^

/home/jappie/projects/haskell/awesome-project-name/src/Lib.hs:14:31: error:
    Illegal type: ‘'[JSON]’ Perhaps you intended to use DataKinds
   |
14 | type UserAPI = "users" :> Get '[JSON] [User]
   |                               ^^^^^^^
```

So we need data kinds to insert data into a type.
(A string being data in this case, it is unclear what `'[JSON]` is,
probably also something data).
If we disable `TypeOperators`, GHC says it doesn't like `:>` in the type line.
And if we disable `DeriveGeneric` it just says we need to enable that to derive
[generic](https://wiki.haskell.org/GHC.Generics)
in the data definition of User, this is required for serialization
(in our case JSON conversion).

## Modules
```haskell
module Lib
    ( webAppEntry
    ) where

import Servant(serve, Proxy(..), Server, JSON, Get, (:>))
import Data.Aeson(ToJSON)
import GHC.Generics(Generic)
import Network.Wai(Application)
import Network.Wai.Handler.Warp(run)
```
Moving onward, there is the module definition that stack generated for us,
modules are just namespaces, or similar to python modules.
Nothing really special about those.
Then we have a bunch of Imports which pull functions into the module namespace.
There is syntax for name clash management, but we won't introduce it here.

## Type level REST API 
```haskell
type UserAPI = "users" :> Get '[JSON] [User]
```
This line defines the UserAPI type, which will serve as the REST endpoint.
The image at the beginning of the post was about this line.
Let's read it as a sentence, without worrying about how it fits together just
now:
It's a Get request, mounted below `/user`, returning something JSON and of
shape/type User.
Conveniently we will discuss what a user is in the next section.

## Domain data
```haskell
data User = User
  { name :: String
  , email :: String
  } deriving (Eq, Show, Generic)

instance ToJSON User
```
User is just a data structure consisting of two strings:
Email and name.
The way we declared data here is called [record syntax](http://learnyouahaskell.com/making-our-own-types-and-typeclasses#record-syntax).
This data structure derives
[Show](https://hackage.haskell.org/package/base-4.9.1.0/docs/Text-Show.html),
[Eq](http://hackage.haskell.org/package/base-4.11.1.0/docs/Data-Eq.html)
and Generic.
Deriving means that GHC will generate function implementations for this
data structure. so if you call `show` on a User, it will know what to do
(show is toString).
`instance ToJSON User` allows the User to be converted to JSON
(implementation is provided by generic).

## Functions
Done with data, let's move on to some code!
```haskell
users :: [User]
```
Specifies a function that will always return a list of Users.
There are no arguments to this function.
We can safely assume the list is always the same
(because it's a pure function, no monads involved).
This is how we specify constants.

```haskell
users =
  [ User "Isaac Newton"    "isaac@newton.co.uk"
  , User "Albert Einstein" "ae@mc2.org"
  ]
```
This is the implementation of before defined function.
There are apparently two users in this list, one Isaac, and another Einstein.
Note that we use the positional arguments to create the Users.

## Servant server
```haskell
server :: Server UserAPI
```
`server :: Server UserAPI` tells us that there is something called a Server which
has a UserAPI.
But we know what a UserAPI is, we defined that above.
It's the routes map.
A [`Server`](http://hackage.haskell.org/package/servant-server-0.14/docs/Servant-Server.html#t:Server)
is defined in servant.
The type signature is rather complicated:
`type Server api = ServerT api Handler`, looking at the definition of `ServerT`
makes my head spin: `type ServerT api (m :: * -> *) :: *`.

There are some clues we can derive (such as that `m`),
but they take many words, and it's not that important to make something work.
Therefore this guide ignores it.
Note that ignoring scary looking things is an important Haskell technique.
One should never ask for help, that is a sign of weakness,
help can be found [here](https://groups.google.com/forum/#!forum/haskell-servant),
just in case ❤.

```haskell
server = return users
```
The implementation is very simple however.
The reader should be cautious, to think that return is a keyword.
It's a function, the same function as pure.
What both return and pure do is wrap a value into a container.
For example we can wrap an element like this in a list:
`pure 2 == [2]`.
That's all we need to know for now (the interested reader may look at
[monads](https://wiki.haskell.org/Monad#Monad_class)).

## Proxy
```haskell
userAPI :: Proxy UserAPI
userAPI = Proxy
```
This is just some type [level magick](http://hackage.haskell.org/package/base-4.11.1.0/docs/Data-Proxy.html).
Library author needed type information for a function, 
but they didn't need a value.
Proxy does that.
It's useful if you store data at type level,
(for example with the datakinds language extension).

## Application
```haskell
app :: Application
app = serve userAPI server
```
This combines the poxy and server.
So a serve function takes a Proxi api, Server api and returns an application.
Note that in haskell we do function application by white spaces.
If we look at the type Application we can appreciate what serve does for us
better:
```haskell
type Application = Request -> (Response -> IO ResponseReceived) -> IO ResponseReceived 
```
The arrows indicate function arguments.
An application receives a request, then we get a callback which expects a response
to produce an IO action which gives the result responseRecevied.
However to return this function must also return a type response Received
wrapped in IO.
We can conjecture that the only way to obtain this response received is to call
that callback.
We receive the freedom to do whatever we want meanwhile (as return type is IO,
which means do whatever you want).

## Running it!
```haskell
webAppEntry :: IO ()
webAppEntry = run 6868 app
```
Our initial function!
rather than saying hello world we're running the app on port 6868 (best port).

Now build and run it in one terminal, and in another curl it:

```bash
stack build && stack exec awesome-project-name-exe &
curl localhost:6868/users

> [{"email":"isaac@newton.co.uk","name":"Isaac Newton"},{"email":"ae@mc2.org","name":"Albert Einstein"}]
```

# Add another endpoint
Let's make it a bit more interesting by doing a post request!
First we must make a file to store the results in:
```bash
echo "[]" > messages.txt
```

Now we must add another dependency for dealing with bytestrings.
Your dependencies should look like this:

```yaml
dependencies:
- base >= 4.7 && < 5
- servant-server # http server
- aeson # json
- wai # web application (interface)
- warp # web application implementation
- bytestring
```

Then we have the magick spells which of which we will treat the changes:

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
import Data.Maybe (fromMaybe)

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
messageFile :: FilePath
messageFile = "messages.txt"

messages :: Message -> Handler [Message]
messages message = do 
  read <- liftIO $ LBS.readFile messageFile
  case decode read of
    Nothing -> pure []
    Just x -> do
      let contents = x ++ [message]
      liftIO $ LBS.writeFile messageFile (encode contents)
      return contents

server :: Server UserAPI
server = (pure users) :<|> messages

userAPI :: Proxy UserAPI
userAPI = Proxy

app :: Application
app = serve userAPI server

webAppEntry :: IO ()
webAppEntry = run 6868 app
```

This will setup another endpoint for messages.
Which will accept a post request under messages.
It will write the message to a file and then it will return the contents of
that file.


## Module changes
```haskell
import Servant
import Control.Monad.IO.Class(liftIO)
import Data.ByteString.Lazy as LBS (writeFile, readFile) 
import Data.Aeson(ToJSON, FromJSON, encode, decode)
import GHC.Generics(Generic)
import Network.Wai(Application)
import Network.Wai.Handler.Warp(run)
```
No we just import everything from Servant, it become a little crazy to do the
explicit imports.
`liftIO` is required to perform side affects (such as reading and writing to a file)
inside the Handler container.
Bytestring does the actual read and write to the file.
We also require the ability to encode and decode JSON now.

## Api changes

```haskell
type UserAPI = "users" :> Get '[JSON] [User]
      :<|> "message" :> ReqBody '[JSON] Message :> Post '[JSON] [Message]
```

The `:<|>` operator is used to add an extra endpoint.
The is of a similar pattern as what we already had.

## Message data
```haskell
data Message = Message {
  from :: User,
  content :: String
} deriving (Eq, Show, Generic)

instance ToJSON Message
instance FromJSON Message
```
This is how a message looks, apparently it's from a user and has some content.
Aside from using another data type inside an existing data type,
there is nothing new going on here.

## File path
```haskell
messageFile :: FilePath
```
The type `FilePath` is just an alias for a string. Eg: `type FilePath = String`,
in other words we can use them interchangeably.
Using `FilePath` is just an extra bit of documentation for code readability.

```haskell
messageFile = "messages.txt"
```
This is where we define what file name is used.

## Message handler
```haskell
messages :: Message -> Handler [Message]
```
We define a Handler (which is an servant api endpoint).
To make it work it requires a message, then it will return a list of messages
within a handler container.
We will see shortly that the handler container is special.

```haskell
messages message = do 
```
The do keyword allows us to code in [do syntax](https://en.wikibooks.org/wiki/Haskell/do_notation).
This allows us to do assignments with `<-` (not assignment, but might as well be.

```
  read <- liftIO $ LBS.readFile messageFile
```
Here we read the message file as a lazy bytestring.
Lazy bytestring is just a convenient format to use because it fits with
encode and decode.
The liftIO function is required to do an IO action within a handler.
The dollar sign can be replaced with an open parentheses, which is closed.
at the end of the line: `liftIO ( LBS.readFile messageFile)` is equivalent.
As said before `<-` is used for assignment.


```haskell
  case decode read of
```  
decoding JSON may not succeed and therefore returns a maybe.
To get rid of it we pattern match on it with case of. 
This can be thought of as a switch case statement in other langauges,
or just an if else if else construct.

```haskell
    Nothing -> pure []
```    
In case decoding fails we return an empty list.

```haskell
    Just x -> do
```
If decoding succeeds we want to store the result in an x.
We also start another do block.
```
      let contents = x ++ [message]
```
This is how we create the new contents of the file.
x is the existing list of messages, and we add the received message to it.
```haskell
      liftIO $ LBS.writeFile messageFile (encode contents)
```
We encode the contents, then we write it to the message file in an IO effect.

```haskell
      return contents
```
Finally we return the contents as result.


## Adding the handler to the routesmap

```haskell
server :: Server UserAPI
server = (pure users) :<|> messages
```

We use the same operator to also add the messages handler into the server.
Note that we don't need the messages in a container, as the return type is
already of type `Handler`.

# In conclusion

The complete code can be found [here](https://github.com/jappeace/awesome-project-name/tree/simple-servent-setup).
In future, no promises,
a post may be done on how to add more api points,
and on how to add a database.
Jappie is also exploring these libraries with the reader so it involves a lot
of work.
