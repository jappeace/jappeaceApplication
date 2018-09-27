Title: Pragmatic Haskell: Simple servant web server
Date: 2018-06-25 15:25
Category: tools
OPTIONS: toc:nil
Tags: haskell, programming, tools, servant, stack, tutorial, pragmatic-haskell
subreddit: haskell programming

1. [Pragmatic Haskell: Simple servant web server]({filename}/pragmatic-haskell-simple-servant.md)
1. [Pragmatic Haskell II: IO Webservant]({filename}/pragmatic-haskell-message-servant.md)
1. [Pragmatic Haskell III: Beam Postgres DB]({filename}/pragmatic-haskell-database.md)

There are many [guides available](https://github.com/bitemyapp/learnhaskell)
for learning Haskell.
Setting up a something simple like a web server isn't so
straight forward.
Perhaps choosing one of the [14 libraries](https://wiki.haskell.org/Web/Frameworks)
is a bit much.

![Type level hell: Haskell sucks](/images/2018/haskell-sucks.jpg)

This guide will give opinionated web server start.
This guide assumes no experience with Haskell,
and will get you up to speed with a (REST) web server called [Servant](http://haskell-servant.readthedocs.io/en/stable/).
Servant is a good choice as it can describe both a server and client API.
In the future this guide may be used as a foundation to create something
more meaningful than just a very basic REST API,
this will provide a good starting point however.
Basic UNIX (command line) skills are assumed.

# From nothing, start with build tools
Install stack:

```bash
curl -sSL https://get.haskellstack.org/ | sh
```

Only attempt shortly to install it trough a package manager.
There are other Haskell build tools, they will be more difficult in use.
There is also the possibility for fully reproducible builds at a system level
(nix).
Which is out of the scope of this guide.

Now setup a new project:
```bash
stack new awesome-project-name 
cd awesome-project-name
```

# Hello world with stack
Appreciate what happens when this is build:
```bash
stack build && stack exec awesome-project-name-exe
```

This should build successfully and output `someFunc`.
Open up `src/Lib.hs` with one's favorite editor.
This contains a few lines, created by stack:

```haskell
module Lib
    ( someFunc
    ) where

someFunc :: IO ()
someFunc = putStrLn "someFunc"
```

This is where the `someFunc` output came from when the program was ran.
Change it to something a bit more appropriate, and rename the function too:
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
and a `src` folder where the library code lives (the modified file is in there).
One can future proving themselves by putting as much code in the library as is
reasonable.

Fix the error in `app/Main.hs`:

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
For the impatient, there is a minimal example already [available](https://github.com/haskell-servant/example-servant-minimal)
by the library author.
This guide will explain how to get there step by step.
In `./package.yaml`, on line 22 there is a `dependencies` key,
add `servant-server`, `aeson`, `wai` and `warp` to it like this:

```yaml
dependencies:
- base >= 4.7 && < 5
- servant-server
- aeson
- wai
- warp 
```

It may seem strange to immediately add four new dependencies,
however this is because Haskell libraries are setup to be flexible.
Even small projects grow quickly to have into the twenties of dependencies.
Code reuse is not [a myth](https://www.youtube.com/watch?v=Jn3kdTaa69U).

`servant-server` is the [servant web server](http://haskell-servant.readthedocs.io/en/stable/).
[`aeson`](http://hackage.haskell.org/package/aeson)
is for JSON parsing and producing.
[`wai`](http://hackage.haskell.org/package/wai) is a web application interface and
[`warp`](http://hackage.haskell.org/package/warp) uses `wai`
to implement a web application (it binds to the port).

Ensure that that this is done at the root of the yaml file (no indentation).
Stack provides a way of specifying dependencies of either the executable or
library.
If its done on line 22, the root of the yaml file,
it will be a dependency for everything in the project.

# A minimal servant
A good start is going to servants' [Hackage](http://hackage.haskell.org/package/servant)
page,
which linked to a [tutorial](http://haskell-servant.readthedocs.io/en/stable/tutorial/index.html).
Servant does API definition [at type level](http://haskell-servant.readthedocs.io/en/stable/tutorial/ApiType.html).

If it's unknown to the reader what a type is, think of it as describing the
shape of a function.
Functions of different shapes don't fit together, and won't compile.
What servant allows us to do is define this shape for a REST API.
To gain a deeper understanding of this a concrete example will be inspected
line by line.
First all lines are listed for a minimal servant (`Lib.hs`) server:

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
data kinds Can be temporary deleted to see what happens:

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

Data kinds is needed to insert data into a type.
A string being data in this case, it is unclear what `'[JSON]` is,
probably also something data.
Temporary breaking a program to see what GHC will say is an
effective way of learning more about Haskell.

If `TypeOperators` is disabled, GHC says it doesn't like `:>` in the `UserAPI` line.
Apparently `:>` is a type operator.
Apparently types can have operators.

If `DeriveGeneric` is disabled, GHC says it needs to derive
[generic](https://wiki.haskell.org/GHC.Generics)
in the data definition of User. Generic is required for serialization
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
Moving onward, there is the module definition that stack generated,
modules are just namespaces, or similar to python modules.
Nothing really special about those.
Then there are many imports which pull functions into the module namespace.

## Type level REST API 
```haskell
type UserAPI = "users" :> Get '[JSON] [User]
```
This line defines the UserAPI type, which will serve as the REST endpoint.
The image at the beginning of the post was about this line.
Perhaps reading it as a sentence will give us some insight,
without worrying about how it fits together:
It's a Get request, mounted below `/user`, returning something JSON and of
shape/type User.
Conveniently what a `User` is will be discussed in the next section.

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
This declaration method is called [record syntax](http://learnyouahaskell.com/making-our-own-types-and-typeclasses#record-syntax).
This data structure derives
[Show](https://hackage.haskell.org/package/base-4.9.1.0/docs/Text-Show.html),
[Eq](http://hackage.haskell.org/package/base-4.11.1.0/docs/Data-Eq.html)
and Generic.
Deriving means that GHC will generate function implementations for this
data structure. If one calls `show` on a User, it will know what to do
(show is toString in Haskell).
`instance ToJSON User` allows the User to be converted to JSON
(implementation is provided by generic).

## Functions
Done with data, time for code!
```haskell
users :: [User]
```
Specifies a function that will always return a list of Users.
There are no arguments to this function.
It can be assumed the list is always the same.
This is how immutable constants are specified.

```haskell
users =
  [ User "Isaac Newton"    "isaac@newton.co.uk"
  , User "Albert Einstein" "ae@mc2.org"
  ]
```
This is the implementation of the before defined function.
There are apparently two users in this list, one Isaac, and another Einstein.
Note that positional arguments are used to create the Users.

## Servant server
```haskell
server :: Server UserAPI
```
`server :: Server UserAPI` says that there is something called a Server which
has a UserAPI.
A UserAPI is known, it is defined above.
A [`Server`](http://hackage.haskell.org/package/servant-server-0.14/docs/Servant-Server.html#t:Server)
is defined in servant.
The type signature is rather complicated:
`type Server api = ServerT api Handler`, looking at the definition of `ServerT`
introduces a lot of complexity: `type ServerT api (m :: * -> *) :: *`.

There are some clues that can be derived (such as that `m`),
but it's not that important to make something work.
Therefore this guide ignores it.
Note that ignoring scary looking things is an important Haskell technique.
If one is interested, help can be found [here](https://groups.google.com/forum/#!forum/haskell-servant),
just in case ❤.

```haskell
server = return users
```
The implementation is very simple however.
The reader should be cautious, to think that return is a keyword.
It's a function.
What both return does is wrap a value into a container.
For example an element can be wrapped in a list:
`return 2 == [2]`.
That's all one needs to know for now
(the interested reader may look at
[monads](https://wiki.haskell.org/Monad#Monad_class)).

## Proxy
```haskell
userAPI :: Proxy UserAPI
userAPI = Proxy
```
This is just some type [level magic](http://hackage.haskell.org/package/base-4.11.1.0/docs/Data-Proxy.html).
Library author needed type information for a function, 
but they didn't need a value.
Proxy does that.
It's useful if you store data at type level,
for example with the datakinds language extension, which was seen earlier.

## Application
```haskell
app :: Application
app = serve userAPI server
```
This combines the proxy and server.
A serve function takes a Proxi API, Server API and returns an application.
If type Application is inspected one can appreciate what serve does for us
better:
```haskell
type Application = Request -> (Response -> IO ResponseReceived) -> IO ResponseReceived 
```
The arrows indicate function arguments.
An application receives a request, then a callback which expects a `Response`
to produce an IO action which gives the result `ResponseReceived`.
However to return this function must also return a type `ResponseReceived`
wrapped in IO.
It may be the case that the only way to obtain this response received is to call
that callback.
The freedom to do whatever one wants is meanwhile granted with the IO return type.
To compile that `ResponseReceived` has to be obtained however.

## Running it!
```haskell
webAppEntry :: IO ()
webAppEntry = run 6868 app
```
Our initial function!
Rather than saying hello world the app is ran on port 6868 (best port).
Now build and run it in one terminal, and in another curl it:

```bash
stack build && stack exec awesome-project-name-exe &
curl localhost:6868/users

> [{"email":"isaac@newton.co.uk","name":"Isaac Newton"},{"email":"ae@mc2.org","name":"Albert Einstein"}]
```

![Good job!](./images/2018/good-job.svg)

# In conclusion
A lot of concepts have been treated within this blog post while also moving
towards something productive.
The reader can now start a new project and add arbitrary dependencies.
He knows what language extensions are and how to see them in use.
Type level magic has been encountered, and wisely was ignored.
In future this post will build on top of this work
to extend the API and do something with something within the handlers.
However this post has grown to big already.

The complete code can be found [here](https://github.com/jappeace/awesome-project-name/tree/simple-servent-setup).
