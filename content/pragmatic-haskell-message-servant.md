Title: Pragmatic Haskell II: IO Webservant
Date: 2018-06-26 12:00
Category: tools
OPTIONS: toc:nil
Tags: haskell, programming, tools, servant, IO
subreddit: haskell programming

Most Haskell language guides will leave IO
[until](http://www.seas.upenn.edu/%7Ecis194/spring13/lectures/08-IO.html)
[later](http://learnyouahaskell.com/input-and-output).
This guide is different,
this guide is about *using* Haskell.
Our focus is different: We build first, then learn trough delight.

![Fancy intro image](/images/2018/io-webserver.svg)

In the [previous blog]({filename}/pragmatic-haskell-simple-servant.md)
post it was explained how to get going with a simple minimalist servant
web server.
In this blog post the simple web server will get an extra REST endpoint that can
do IO actions.
This is an important part of pragmatic Haskell programming.
Without IO our program can do nothing.
Programmers are not theorists, therefore we need IO.

The structure of this guide is as follows:
First we prepare our system,
then we throw lots of code in your face,
after which we step the newly introduced concepts line by line.
Finally we execute it and pat ourselves on the back.

# Preparation
The code assumes a file exists.
Create one with an empty JSON array in the project root:
```bash
echo "[]" > messages.txt
```

Bytestrings are a convenient way of opening files and putting the results into
`aeson`, the JSON library.
Dealing with bytestrings requires another dependency:

```yaml
dependencies:
- base >= 4.7 && < 5
- servant-server # http server
- aeson # json
- wai # web application (interface)
- warp # web application implementation
- bytestring
```

These are the <s>magick spells</s> changes which add an enpoint,
explained in detail below:

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
  result <- liftIO $ LBS.readFile messageFile
  case decode result of
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
The new endpoint will accept a post request under "/message".
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
All of Servant is important now, it became hard to do the explicit imports.
The `liftIO` import will be discussed below.
`Bytestring` is the format we get from reading and writing to a file,
it's conveniently also the format aeson accepts for decoding and encoding.

## Api changes

```haskell
type UserAPI = "users" :> Get '[JSON] [User]
      :<|> "message" :> ReqBody '[JSON] Message :> Post '[JSON] [Message]
```

The `:<|>` operator is used to add an extra endpoint, it like the `+` operator
is for integers, it combines the two endpoints into one UserAPI.
Like true Haskellers we will ignore the [inner workings](operator link) again.

The extra end point "message" is similar in structure to the existing "user"
endpoint.
If read like a sentence "message" is a POST only endpoint,
which accepts a Message JSON body, and it returns a list of Messages in JSON.

## Message data
```haskell
data Message = Message {
  from :: User,
  content :: String
} deriving (Eq, Show, Generic)

instance ToJSON Message
instance FromJSON Message
```

This is Message, apparently it's from a `User` and has some content `String`.
Aside from using another data type inside an existing data type,
no new concepts are introduced.

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

### Do notation
```haskell
messages message = do 
```
The do keyword allows us to code in [do syntax](https://en.wikibooks.org/wiki/Haskell/do_notation).
This allows us to do assignments with `<-` (not assignment, but might as well be.
This only works when the result container is a [Monad](https://wiki.haskell.org/Monad).
*How* monads work is a mystery, but usage is simple: Use do syntax.

### LiftIO
```
  result <- liftIO $ LBS.readFile messageFile
```
Here we read the message file as a lazy bytestring and put it into the result.
One may wonder why we are using liftIO, if it's deleted we get this:

```bash
/home/jappie/projects/haskell/awesome-project-name/src/Lib.hs:46:13: error:
    • Couldn't match type ‘IO’ with ‘Handler’
      Expected type: Handler Data.ByteString.Lazy.Internal.ByteString
        Actual type: IO Data.ByteString.Lazy.Internal.ByteString
    • In a stmt of a 'do' block: result <- LBS.readFile messageFile
      In the expression:
        do result <- LBS.readFile messageFile
           case decode result of
             Nothing -> pure []
             Just x -> do ...
      In an equation for ‘messages’:
          messages message
            = do result <- LBS.readFile messageFile
                 case decode result of
                   Nothing -> pure ...
                   Just x -> ...
   |
46 |   result <- LBS.readFile messageFile
   |             ^^^^^^^^^^^^^^^^^^^^^^^^
```

The `LBS.readfile` function requires a return type of `IO`.
However the return type of `messages` is `Handler`.
Therefore the compiler says that it expects `Handler`, but the actual type is `IO`.
Handler implements the `MonadIO` typeclass however, which allows us to
choose to do things within `IO` by calling the `liftIO` function.
The `liftIO` function simply tells the Handler container to execute some
function within the `IO` container.

The dollar sign can be replaced with an open parentheses,
which is closed at the end of the line, for example:
`liftIO (LBS.readFile messageFile)` is equivalent.

As said before `<-` is (basically) used for assignment in do notation,
using `<-` is a good way to get rid of a monad container.
`result` now contains the contents of messageFile,
the IO is being evaluated and removed by the `<-` operator.
In Haskell we deal a lot with wrapping and unwrapping things into and from
containers.

### Case .. of
```haskell
  case decode result of
```  
Here we're decoding the contents of `result`.
decoding JSON may not succeed and therefore the library authors of `aeson`
made
[decode](http://hackage.haskell.org/package/aeson-1.4.0.0/docs/Data-Aeson.html#v:decode)
return a maybe container:
```haskell
decode :: FromJSON a => ByteString -> Maybe a 
```
In this signature, `a` can be anything as long as it implements FromJSON.
We do this with generic and `instance FromJSON Message`.
We give as bytestring the `result` to decode, in return it gives us `Maybe a`.
The compiler figures out that `a` in this case is `[Message]`.

The return value will have content if decoding succeeded (`Just`),
or it won't if it fails (`Nothing`).
To get rid of the container we pattern match it. 
This can be thought of as a switch case statement in other langauges,
or just an `if elif else` construct.

#### Nothing
```haskell
    Nothing -> pure []
```    
In this case decoding fails.
An empty list is returned to the client.
We still must wrap this list in a `Handler`, pure is used for that.
Note that `pure == return`.
These functions both exist for [historical reasons](https://stackoverflow.com/questions/32788082/difference-between-return-and-pure).

#### Just a
```haskell
    Just x -> do
```
In this case there is success,
the result is taken and put into x,
after which another do block starts.

### Let
```
      let contents = x ++ [message]
```
Unlike the `<-` operator, let does not do any unwrapping.
We can see what happens if we replace the let binding by
`contents <- x ++ [message]`,
the errors are:

```bash
/home/jappie/projects/haskell/awesome-project-name/src/Lib.hs:50:19: error:
    • Couldnt match type ‘[]’ with ‘Handler’
      Expected type: Handler Message
        Actual type: [Message]
    • In a stmt of a 'do' block: contents <- x ++ [message]
      In the expression:
        do contents <- x ++ [message]
           liftIO $ LBS.writeFile messageFile (encode contents)
           return contents
      In a case alternative:
          Just x
            -> do contents <- x ++ [message]
                  liftIO $ LBS.writeFile messageFile (encode contents)
                  return contents
   |
50 |       contents <- x ++ [message]
   |                   ^^^^^^^^^^^^^^

/home/jappie/projects/haskell/awesome-project-name/src/Lib.hs:52:7: error:
    • Couldnt match type ‘Message’ with ‘[Message]’
      Expected type: Handler [Message]
        Actual type: Handler Message
    • In a stmt of a 'do' block: return contents
      In the expression:
        do contents <- x ++ [message]
           liftIO $ LBS.writeFile messageFile (encode contents)
           return contents
      In a case alternative:
          Just x
            -> do contents <- x ++ [message]
                  liftIO $ LBS.writeFile messageFile (encode contents)
                  return contents
   |
52 |       return contents
   |       ^^^^^^^^^^^^^^^

```

In the first error the compiler says that a list is not a `Handler` container.
Which we expect because the return type of this function is `Handler [Message]`.

The second error asumes contents is of the correct type,
which means we unwrap the list, contents would be of type `Message`.
The return type does not fit in this case either,
we would get `Handler Message` instead  of `Handler [Message]`.

### Write
```haskell
      liftIO $ LBS.writeFile messageFile (encode contents)
```
We encode the contents,
then we write it to the message file in an IO effect.
Type safety ensures encoding always succeeds.

### Return
```haskell
      return contents
```
Finally we wrap the contents in a `Handler` type.

## Adding the handler to the routesmap

```haskell
server :: Server UserAPI
server = (pure users) :<|> messages
```

We use the same operator to also add the messages handler into the server.
We don't need to put `messages` in a container with pure because the functions'
return type is already a `Handler`.

# Test it!

```shell
curl --header "Content-Type: application/json" \ 
  --request POST \
  --data '{"from":{"email":"d","name":"xyz"}, "content": "does it word?"}' \
  http://localhost:6868/message
```

Worked on this machine...

![Worked on my machine, lol, now we use IO everything is over](/images/2018/machine-on-fire.svg)

Perhaps that's why the theorists avoid IO 🤔.

# In conclusion
A lot was encountered in this blogpost without going into much theory.
We learned that haskell is primarly dealing with containers.
We saw `liftIO`, to put certain functions in `IO` rather than
the return type.
Also use of `do` notation was seen, which is used a lot.
Now we can affect the world with our programs trough IO!

The complete code can be found [here](https://github.com/jappeace/awesome-project-name/tree/simple-servent-setup).
In the future we shall attach this simple webserver to a database.
