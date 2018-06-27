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
Our focus is different: We build first, then learn trough [delight](https://medium.com/the-polymath-project/programming-for-personal-growth-64052e407894).

![Fancy intro image](/images/2018/io-webserver.svg)

The [previous blog]({filename}/pragmatic-haskell-simple-servant.md)
post explained how to get going with a simple minimalist servant
web server.
In this blog post the simple web server will get an extra REST endpoint that can
do IO actions.
This is an important part of pragmatic Haskell programming.
Without IO our program can do nothing.
Programmers are not theorists, therefore we need IO.

# Preparation
To keep things simple, the code assumes a file exists.
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

# A lot of code
These are the <s>magic spells</s> changes which add an endpoint,
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

# Line by line inspection
```haskell
type UserAPI = "users" :> Get '[JSON] [User]
      :<|> "message" :> ReqBody '[JSON] Message :> Post '[JSON] [Message]
```

The
[`:<|>` operator](http://hackage.haskell.org/package/servant-0.14/docs/Servant-API-Alternative.html#t::-60--124--62-)
is used to add an extra endpoint.
It combines the two endpoints into one.
In these lines, we are constructing something akin to a jump table.
The decleration of this operator is surprisingly simple:
```haskell
data a :<|> b = a :<|> b
```
Left sign of equality is used for type, right side for data construction.
Skimming over this, like true Haskellers we will ignore the
[inner workings](http://hackage.haskell.org/package/servant-0.14/docs/src/Servant-API-Alternative.html#%3A%3C%7C%3E)
.

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
The type `FilePath` is just an alias for a `String`: `type FilePath = String`.
In other words we can use them interchangeably.
`FilePath` acts as documentation.

```haskell
messageFile = "messages.txt"
```
This is where we define what file name is used.

## Message handler
```haskell
messages :: Message -> Handler [Message]
```
We define a Handler (which is an servant api endpoint).
It requires a message, then it will return a list of messages
within a `Handler`.

### Do notation
```haskell
messages message = do 
```
The do keyword allows us to code with [do notation](https://en.wikibooks.org/wiki/Haskell/do_notation).
This allows us to do assignments with `<-` (not assignment, but close enough).
This only works when the result container is a [Monad](https://wiki.haskell.org/Monad).
*How* monads work is a mystery, but usage is simple: Use do notation.

### LiftIO
```
  result <- liftIO $ LBS.readFile messageFile
```
Here we read the message file as a lazy bytestring and put it into the result.
One may wonder why we are using liftIO, if it's deleted we get this:

```bash
/home/jappie/projects/haskell/awesome-project-name/src/Lib.hs:46:13: error:
    • Couldnt match type ‘IO’ with ‘Handler’
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

The `LBS.readfile` function has a return type of `IO`.
However the return type of `messages` is `Handler`.
Therefore the compiler says that it expects `Handler`, but the actual type is `IO`.
Handler implements the `MonadIO` typeclass however, which allows
`IO` by calling the `liftIO` function.
The `liftIO` function simply tells the Handler container to execute some
function within the `IO` container.

The dollar sign can be replaced with an open parentheses,
which is closed at the end of the line. This is equivalent for example:
```haskell
liftIO (LBS.readFile messageFile)
```

As said before `<-` is (basically) used for assignment in do notation,
using `<-` is a good way to get rid of a monad container.
`result` now contains the contents of messageFile,
the IO is being evaluated and removed by the `<-` operator.
In Haskell a lot of wrapping and unwrapping is done. 

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
We fulfill this condition with generic and `instance FromJSON Message`.
We give as bytestring the `result` to decode, in return it gives us `Maybe a`.
The compiler deduces that `a` in this case is `[Message]`.

The return value will have content if decoding succeeded (`Just`),
or it won't if it fails (`Nothing`).
To get rid of the container we pattern match it. 
This can be thought of as a switch case statement in other languages.

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

The second error assumes `contents` is of the correct type,
since `<-` unwraps `contents` would be of type `Message`.
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

## Adding the handler to the routes map

```haskell
server :: Server UserAPI
server = (pure users) :<|> messages
```
This is the implementation of the `UserAPI` type described before.
We use the same operator to also add the messages handler into the server.
We don't need to put `messages` in a container with pure because the functions'
return type is already a `Handler`.

Something worth pointing out is that this construction is in order,
if we pull it out of order we would get a type level.
Changing the line into:

```haskell
server =  messages :<|> (pure users)
```

Will cause an error:

```bash
/home/jappie/projects/haskell/awesome-project-name/src/Lib.hs:55:11: error:
    • Could not match type ‘[User]’ with ‘Handler [Message]’
      Expected type: Server UserAPI
        Actual type: (Message -> Handler [Message])
                     :<|> (Message -> [User])
    • In the expression: messages :<|> (pure users)
      In an equation for ‘server’: server = messages :<|> (pure users)
   |
55 | server =  messages :<|> (pure users)
   |           ^^^^^^^^^^^^^^^^^^^^^^^^^^
```

# Execute it!

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
Without going into much theory, we dealt with `IO`.
For example we saw that haskell is about dealing with containers,
to put certain functions in `IO` rather than the return type, one uses `liftIO`.
`do` notation was also encountered, which makes working with monads easier.
Now we can affect the world with our programs trough IO!

The complete code can be found [here](https://github.com/jappeace/awesome-project-name/tree/simple-servent-setup).
In the future we shall attach this simple web server to a database.
