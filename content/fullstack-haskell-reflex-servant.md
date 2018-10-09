Title: Fullstack Haskell: Reflex and Servant
Date: 2018-10-09 12:08
Category: tools
OPTIONS: toc:nil
Tags: haskell, programming, tools, database
subreddit: haskell programming reflexfrp

In the [pragmatic haskell](/pragmatic-haskell.html) series, we saw how to setup a simple webserver with database.
But at some point you still need a frontend.
If it were 2005 you may have been able to get away with just [blaze](http://hackage.haskell.org/package/blaze-html).
But we are in 2018+, and [JavaScript is a problem](https://wiki.haskell.org/The_JavaScript_Problem).
In this blog post we will explore how to deal with JavaScript trough reflex and GHCJS.
An alternative to consider is [miso](https://github.com/dmjio/miso),
which uses the [elm architecture](https://guide.elm-lang.org/architecture/)
(or [redux](https://redux.js.org/) if you're from JS),
here is a [comparison](https://www.reddit.com/r/haskell/comments/7nxni9/reflexdom_vs_miso/).
Obviously I chose reflex.

![fancy db image](/images/2018/reflex-and-servant.svg)

# Preparation
First we need to setup the dev environment.
This time we'll double down on [nix](https://nixos.org/nix/) because reflex does
that too and fighting build tools is no fun.
This has the advantage that the [resulting code on github](https://github.com/jappeace/awesome-project-name/tree/reflex)
is reproducible.
All need to be done is setup the file watch for which I wrote a make command:
```bash
make file-watch
```
This rebuilds both the Haskell back end and JavaScript front end incrementally.

There are two separate environments now, one is for the native Haskell target (x86),
and the other is the JavaScript target.
We can enter the shell environment for the native target with `make enter`
and the JavaScript target with `make enter-js`.
This is convenient for doing one of commands.

The biggest issue I had when setting this up was figuring out how to add extra dependencies
not in the nix repo.
I found out by reading the nix code that this can be done with the overrides flag.
Another issue was tools for shells,
such as hpack which generates cabal files from the `package.yaml` file.
I really wanted to use that as I didn't want to learn cabal,
besides, hpack's is much more succinct, it doesn't require explicit module exports.
there is a shellOverrides attribute for that.

```nix
    overrides = self: super: rec {
      beam-core = self.callPackage ./packages/beam-core.nix { };
	  ...
    };
	...
    shellToolOverrides = ghc: super: {
        inherit (ghc) hpack;
        fswatcher = pkgs.inotify-tools;
		...
    };
```

# Back end
The backend is mostly the same as the result of the [pragmatic haskell](/pragmatic-haskell.html)
series.
We moved the API endpoints that need to be accessed by client to the e common code,
and added an additional endpoint for hosting the html.
Normally we wouldn't use Haskell to deliver static assets and use a
specialized program such as [nginx](https://www.nginx.com/).
Since this is for experimentation however we made an exception:

```haskell
type Webservice = ServiceAPI 
      :<|> Raw -- JS entry point

webservice :: Proxy Webservice
webservice = Proxy

... 

server :: Connection -> Server Webservice
server conn=
  (pure users :<|> messages conn) :<|> serveDirectoryFileServer "dist-ghcjs/build/x86_64-linux/ghcjs-0.2.1/frontend-0.1.0.0/c/webservice/build/webservice/webservice.jsexe/"
```

The webservice type definition has the aditional `Raw` endpoint,
which [allows hosting](http://haskell-servant.github.io/servant/Servant-API-Raw.html)
of custom wai apps.
The `erveDirectoryFileServer` is that custom wai app and just host the JavaScript
output of the client.


# Common code
This is where the shared code between client and server lives.
We put the API definition in here.
Since servant can create both servers and clients it's a great library
for this use case.

Any change in API will now cause the type checker to tell us where this is affected
in both client and server.
Type safety becomes amplified,
making bugs more obvious and increasing developer productivity.

The actual [content](#commonsrccommonhs) of this module isn't that interesting,
it's just the API definition.
Common code gets compiled within the JavaScript client.
This means it's public, one should not put any passwords or trade secrets in here.

# Front end
I started with trying to get reflex to work with servant because it seemed the most uncertain.
After this I intended to use `servant-client` for generating the client functions,
here I ran into another hurdle as the latest servant wasn't available.
Apparently reflex is pinned to an old hackage repository,
I attempted to upgrade but abandoned that endeavour as it required more nix modifications
and I'd prefer to keep the same pin as upstream so I could get help when I needed it.
Using the older servant, I hit a run time exception:
```
uncaught exception in Haskell main thread: ReferenceError: h$hsnet_getaddrinfo is not defined
```

This is because servant client uses a system call for networking
which is unavailable in the browser sandbox.
A bit of googling led me to [servant reflex](https://github.com/imalsogreg/servant-reflex).
Using this was hard because are no official [haddocs](http://hackage.haskell.org/package/servant-reflex-0.3.3/candidate)
since it hasn't been released yet.
Finding an [example](https://github.com/meditans/haskell-webapps/tree/master/UI/ReflexFRP/mockLoginPage)
was of great help which led me to this client definition:

```haskell
apiClients :: forall t m. (MonadWidget t m) => _
apiClients = client serviceAPI (Proxy @m) (Proxy @()) (constDyn url)
  where url :: BaseUrl
        url = BasePath "/"
```
This creates both functions for querying the `serviceAPI` from the common module.
All this seems to do is getting that `m` in scope and applying it to the client with a proxy.
The partial type signature was left from the example this way intentionally,
because it's formalized below anyway and it's rather big.

```haskell
getUsers :: MonadWidget t m
          => Event t ()
          -> m (Event t (ReqResult () [User]))
postMessage :: MonadWidget t m
            => Dynamic t (Either Text.Text Message)
            -> Event t ()
            -> m (Event t (ReqResult () [Message]))
(getUsers :<|> postMessage) = apiClients
```
This pulls out the functions from `apiClients`,
we also get there final signature here.
The entire file can be seen in the [sources](#frontendsrcservantclienths).

## Reflex
After getting the API to function I started working on making an actual UI.
Which is what this codes does for the `getUsers` function:
```haskell
reflex :: IO()
reflex =
  mainWidget $
    el "div" $ do
        -- babys steps, get users from memory
        intButton  <- button "Get Users"
        serverInts <- fmapMaybe reqSuccess <$> getUsers intButton
        display =<< holdDyn ([User "none" "none"]) serverInts
```

mainWidget is the root of reflex, we use `el` to specify HTML elements that surround other elements.
the button functions creates a button (no surprise).
This is within the monad widget,
interaction between components is handled trough that monad.

On the next line we use the intButton immediately.
If we look at the getUsers type signature we see that it requires an `Event t ()` argument,
this is satisfied by the button. In other words `getUsers` will triggered on the button event.
The result is once more put in the monadwidget.
Finally we map the result to assume success or Nothing,
it will be just a list of users now.
The holdDyn function is then used to give a default value to the resulting event in case of nothing,
we always display either the default or the request result.

## Markup with reflex
For the `postMessage` function I made a form with text inputs:
```haskell
		-- Post a usermessage and display results
		input <- messageInput 
		sendMsg <- button "Send Message"
		messages <- fmapMaybe reqSuccess <$> postMessage (Right <$> input) sendMsg 
		resulting <- holdDyn
			([Message (User "none" "none") "ddd"]) -- what to show if nothing
			messages -- source of messages (if any)
        _ <- el "div" $
            simpleList resulting fancyMsg 
```

`messageInput` is a function that returns a "dynamic message" (see below),
the button is for sending of messages.
To display we use a similar pattern however this time we'll mark it up in HTML
with fancy messages.
We traverse over the dynamic list with the `simpleList` function,
here I expected traverse to work.

```haskell
  where
    fancyMsg :: (MonadWidget t m) => Dynamic t Message -> m (Element EventResult GhcjsDomSpace t)
    fancyMsg msg = elClass "div" "message" $ do
        _ <- elDynHtml' "h1" $ Text.pack . name . from <$> msg
        elDynHtml' "span" $ Text.pack . content <$> msg
```
Every message is put in a `div` element,
for displaying dynamic content however we need to use `elDynhtml'` function,
there is no way of getting a value out of dynamic,
we can only show it to the user.
This is a strong safety guarantee.

## Reflex "react component"
Input fields can be combined together into larger components,
which is showcased in the Message form:
```haskell
messageInput :: (MonadWidget t m) => m (Dynamic t Message)
messageInput = do
    user <- userInput
    message <- labeledInput "message"
    pure $ (Message <$> user) <*> (Text.unpack <$> _textInput_value message)

userInput :: (MonadWidget t m) => m (Dynamic t User)
userInput = do
        username <- labeledInput "username"
        emailInput <- labeledInput "email"
        pure $ User . Text.unpack <$> _textInput_value username <*> (Text.unpack <$> _textInput_value emailInput)

labeledInput :: (MonadWidget t m) => Text.Text -> m (TextInput t)
labeledInput label = elClass "div" "field" $ do
    elClass "label" "label" $ text label
    elClass "div" "control" $ textInput (def & textInputConfig_attributes .~ constDyn (Text.pack "class" =: Text.pack "input"))
```
This is done with applicative fmap `<$>` and  spaceship `<*>`.

Note that these functions are analogue to a react component, and see the difference!
They compose perfectly and will function independently.

Feel the power.

My only complaint is that the resulting JavaScript binary is huge,
8MB, 2MB after using the [closure compiler](https://github.com/ghcjs/ghcjs/wiki/Deployment).

# Conclusion
I'm very pleased with reflex,
now I don't have to deal with JavaScript,
I can prototype my API rapidly and I'm not restricted to an architecture.
It is better than I expected, the core seems really well designed.
The only downside is the large binary.
None the less I'm willing to use this for a larger project.

# Links
For convenience here is a list of used resources:

- [Complete sources for this post](https://github.com/jappeace/awesome-project-name/tree/reflex)
- [Reflex](https://github.com/reflex-frp/reflex)
- [Reflex project development](https://github.com/reflex-frp/reflex-platform/blob/develop/docs/project-development.md)
- [Example frp app](https://github.com/meditans/haskell-webapps/tree/master/UI/ReflexFRP/mockLoginPage)
- [Servant reflex release candidate docs](http://hackage.haskell.org/package/servant-reflex-0.3.3/candidate)
- [Explenation of things](http://docs.reflex-frp.org/en/latest/)

# Sources
The project has become too big to share all files,
as always there is the [github link](https://github.com/jappeace/awesome-project-name/tree/reflex).
I will however put all discussed code in complete form in here.

## backend/src/Lib.hs
```haskell
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

{-# OPTIONS_GHC -Wno-missing-monadfail-instances #-}


module Lib
    ( webAppEntry
    ) where

import Servant
import Common
import Control.Monad.IO.Class(liftIO)
import Network.Wai(Application)
import Network.Wai.Handler.Warp(run)
import           Database.PostgreSQL.Simple   (Connection)
import qualified DB as DB
import           Database.Beam.Backend.SQL.BeamExtensions (runInsertReturningList)

import qualified Database.Beam                            as Beam
import qualified Database.Beam.Postgres                            as PgBeam
import Data.Text(pack, unpack)

type Webservice = ServiceAPI 
      :<|> Raw -- JS entry point

webservice :: Proxy Webservice
webservice = Proxy

users :: [User]
users =
  [ User "Isaac Newton"    "isaac@newton.co.uk"
  , User "Albert Einstein" "ae@mc2.org"
  ]

messages :: Connection -> Message -> Handler [Message]
messages conn message = do 
  fromDb <- liftIO $ 
    PgBeam.runBeamPostgres conn $ do
      let user = from message
      [foundUser] <- runInsertReturningList (DB._ausers DB.awesomeDB) $ 
          Beam.insertExpressions [DB.User 
            Beam.default_
            (Beam.val_ (pack $ name $ user ))
            (Beam.val_ (pack $ email $ user ))
        ]
      _ <- runInsertReturningList (DB._messages DB.awesomeDB) $ 
          Beam.insertExpressions 
            [DB.Message 
              Beam.default_ 
              (Beam.val_ (Beam.pk foundUser))
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
    ) fromDb


server :: Connection -> Server Webservice
server conn=
  (pure users :<|> messages conn) :<|> serveDirectoryFileServer "dist-ghcjs/build/x86_64-linux/ghcjs-0.2.1/frontend-0.1.0.0/c/webservice/build/webservice/webservice.jsexe/"

app :: Connection -> Application
app conn = serve webservice (server conn)

webAppEntry :: Connection -> IO ()
webAppEntry conn = run 6868 (app conn)
```

## common/src/Common.hs
```haskell
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DeriveGeneric #-}

module Common where

import GHC.Generics(Generic)
import Servant.API
import Data.Proxy
import Data.Aeson(ToJSON, FromJSON)

type ServiceAPI = "api" :> "1.0" :> "users" :> Get '[JSON] [User]
      :<|> "api" :> "1.0" :> "message" :> ReqBody '[JSON] Message :> Post '[JSON] [Message]

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

serviceAPI :: Proxy ServiceAPI
serviceAPI = Proxy

```

## frontend/src/Lib.hs
```haskell
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fprint-explicit-kinds -Wpartial-type-signatures #-}


module Lib
  ( reflex
  ) where
import Reflex
import Reflex.Dom
import qualified Data.Text as Text
import Control.Applicative ((<*>), (<$>))
import Common
import Servant.Reflex
import ServantClient

reflex :: IO()
reflex =
  mainWidget $
    el "div" $ do
        -- babys steps, get users from memory
        intButton  <- button "Get Users"
        serverInts <- fmapMaybe reqSuccess <$> getUsers intButton
        display =<< holdDyn ([User "none" "none"]) serverInts

        -- Post a usermessage and display results
        input <- messageInput 
        sendMsg <- button "Send Message"
        messages <- fmapMaybe reqSuccess <$> postMessage (Right <$> input) sendMsg 
        resulting <- holdDyn
            ([Message (User "none" "none") "ddd"]) -- what to show if nothing
            messages -- source of messages (if any)

        _ <- el "div" $
            simpleList resulting fancyMsg 
        pure ()
  where
    fancyMsg :: (MonadWidget t m) => Dynamic t Message -> m (Element EventResult GhcjsDomSpace t)
    fancyMsg msg = elClass "div" "message" $ do
        _ <- elDynHtml' "h1" $ Text.pack . name . from <$> msg
        elDynHtml' "span" $ Text.pack . content <$> msg

messageInput :: (MonadWidget t m) => m (Dynamic t Message)
messageInput = do
    user <- userInput
    message <- labeledInput "message"
    pure $ (Message <$> user) <*> (Text.unpack <$> _textInput_value message)

userInput :: (MonadWidget t m) => m (Dynamic t User)
userInput = do
        username <- labeledInput "username"
        emailInput <- labeledInput "email"
        pure $ User . Text.unpack <$> _textInput_value username <*> (Text.unpack <$> _textInput_value emailInput)

labeledInput :: (MonadWidget t m) => Text.Text -> m (TextInput t)
labeledInput label = elClass "div" "field" $ do
    elClass "label" "label" $ text label
    elClass "div" "control" $ textInput (def & textInputConfig_attributes .~ constDyn (Text.pack "class" =: Text.pack "input"))
```
## frontend/src/ServantClient.hs
```haskell
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -Wno-partial-type-signatures #-}

{-# LANGUAGE NoMonomorphismRestriction          #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE  TypeApplications #-}
{-# LANGUAGE TypeOperators #-}

-- | This modules purpose is just to generate the xhr clients.
--   there is some type magick going on generating these,
--   therefore the functions are isolated.
module ServantClient
  ( postMessage, getUsers 
  ) where

import Reflex
import Reflex.Dom
import qualified Data.Text as Text
import Servant.API
import Common
import Servant.Reflex
import Data.Proxy

-- | This intermediate definition is necisarry because the @m is similar for both clients,
--   they have the same wrapping monad however the containing type is different
--   (which is why we have the nomonomorphism restirction disabled)
apiClients :: forall t m. (MonadWidget t m) => _
apiClients = client serviceAPI (Proxy @m) (Proxy @()) (constDyn url)
  where url :: BaseUrl
        url = BasePath "/"

getUsers :: MonadWidget t m
          => Event t ()  -- ^ Trigger the XHR Request
          -> m (Event t (ReqResult () [User])) -- ^ Consume the answer
postMessage :: MonadWidget t m
            => Dynamic t (Either Text.Text Message)
            -> Event t ()
            -> m (Event t (ReqResult () [Message]))
(getUsers :<|> postMessage) = apiClients
```

