Title: Hey you! Go build a Haskell webserver with servant!
Date: 2018-06-23 12:00
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

![Progress!](/images/2018/good-job.svg) 

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

Okay now we have a servant we must learn to use it.
To do this we go to [Hackage](http://hackage.haskell.org/package/servant),
which linked to a [tutorial](http://haskell-servant.readthedocs.io/en/stable/tutorial/index.html).
Servant does API definition [at type level](http://haskell-servant.readthedocs.io/en/stable/tutorial/ApiType.html).
If it's unknown to the reader what a type is, think of it as a shape we can
attach to a function.
What servant allows us to do is define this shape for a rest api.
Let's look at a concrete example line by line.
These are the magic spells required to setup servant:

```haskell
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DeriveGeneric #-}

module Lib
    ( webAppEntry
    ) where

import Servant
import Data.Aeson
import Data.Aeson.Types
import GHC.Generics
import Network.Wai.Handler.Warp

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

The first three lines are langauges extensions, Haskell behaves different
for this module.
Lets temporary disable datakinds to see what happens:

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

So we need datakinds to insert data into a type.
(String being data in this case, it is unclear what `'[JSON]` is,
probably also something data).
If we disable `TypeOperators`, GHC says it doesn't like `:>` in the type lines.
And if we disable `DeriveGeneric` it just says we need to enable that to derive
generic in the data definition of User, this is required for serialization
(in our case JSON conversion).

Moving onward, there is the module definition that stack generated for us,
modules are just namespaces, or like python modules.
Nothing really special about those except syntax.
Then we have a bunch of Imports which pull functions into the module namespace.

Then we have the line which defines the UserAPI type, which will serve as the
REST endpoint.
It's a Get request, mounted below user, returning something JSON and of shape User.

User is defined below that, which is just a data structure consisting of two
strings.
Email and name.
This datastrcutre derives Show, Eq and Generic.
Deriving means that GHC will generate function implementations for this
data structure. so if you call `show` on a User, it will know what to do
(show is toString).
`instance ToJSON User` allows the User to be converted to JSON
(implementation is provided by generic).

Done with data, let's move on to some code!
`users :: [User]` specifies a function that will always return a list of Users.
`user = ` specifies what this list contains.

`server :: Server UserAPI` tells us that there is something called a Server which
has a UserAPI. Okay we can figure out what happened here.
Server is a type imported from Warp. It expect basically a routes map which is
defined in the wai interface.
The servant api provides this one function `:>` which has two arguments,
the first one being the route, and the second one being the endpoint.
The result is something which fits in the Warp Server, because Warp is build on
top of the WAI interface.
So servant uses WAI, warp uses WAI, WAI expects any kindoff text.
We can create text with JSON. Everything fits.



Now build and run it in one terminal, and in another curl it:

```bash
curl localhost:6868/users

> [{"email":"isaac@newton.co.uk","name":"Isaac Newton"},{"email":"ae@mc2.org","name":"Albert Einstein"}]
```

Hurray success.
Now lets add a database.

# Database
