Title: Impatient haskell
Date: 2018-06-23 12:00
Category: tools
OPTIONS: toc:nil
Tags: haskell, programming, tools
subreddit: haskell programming
status: draft

![Type level hell: Haskell sucks](/images/2018/haskell-sucks.jpg)

Getting into haskell is easy. well, at least there are some
[guides available](https://github.com/bitemyapp/learnhaskell).
Setting up a webserver however,
hah, once you know the language, you do can anything, can't you?
Perhaps choosing one of the [14 libraries](https://wiki.haskell.org/Web/Frameworks)
is a bit much.

This guide will give an opinion on how to get started.
This guide assumes no experience with Haskell,
and will get you up to speed with a webserver and database.
Then is described how to setup a 
reactive programming in Haskell for both web and android.

# Build tools
Install stack:

    curl -sSL https://get.haskellstack.org/ | sh

Only attempt shortly to install it trough a package manager.
More than 5 minutes is not worth it. (at some point it may become possible).
There are other build tools, they're all ~~bad~~, not as easy.
There is also the possibility for fully reproducible builds at a system level
(nix).
Which is out of the scope of this guide (but there is [another](http://localhost:8000/fun-with-stack-haskell-dependency-management.html)).

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

No, time to explain about stacks project structure.
There is an app folder where by default all the executables reside
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

## Your first dependency
Now, to setup servant. It will be easy, don't you worry!
In `./package.yaml`, on line 22 there is a `dependencies` key,
add servant to it like this:

```yaml
dependencies:
- base >= 4.7 && < 5
- servant-server
```

Note that you should *not* add this to the dependencies of the executable or
library.
Stack provides a way of specifying dependencies of either of these.
However if we do it on line 22 (root of the yaml file),
it will be a dependency for everything in the project.

Okay now we have a servant we must learn to use it.
To do this we go to [hackage](http://hackage.haskell.org/package/servant),
which linked to a [tutorial](http://haskell-servant.readthedocs.io/en/stable/tutorial/index.html).
Oh wow, servant does api definition [at type level](http://haskell-servant.readthedocs.io/en/stable/tutorial/ApiType.html).
If it's unkown to the reader what a type is, think of it as a shape we can
attach to a function.
If one function has a shape that does not fit in another function, it won't work,
and compilation will throw an error.

Here it was realized that Jappie hadn't decided what to build.
So here we design this little chatroom?
Where we can identitify, at root level,
then we can send messages which will be boadcasted to all users.

Obviously this will work on web, and android in an single page app.
Fly high Icarus!

Dependencies:
```yaml
dependencies:
- base >= 4.7 && < 5
- servant-server # http server
- aeson # json
- wai # web application (interface)
- warp # web application implementation
```

The magick spells required to setup servant.

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

Now build and run it in one terminal, and in another curl it:

```bash
curl localhost:6868/users
```
