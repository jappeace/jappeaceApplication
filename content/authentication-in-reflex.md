Title: Authentication in Reflex & Servant
Date: 2018-10-09 12:08
Category: tools
OPTIONS: toc:nil
Tags: haskell, programming, tools, reflex, frp
subreddit: haskell programming reflexfrp
status: draft

In the previous [Fullstack Haskell]({filename}/fullstack-haskell-reflex-servant.md)
post I discussed how to setup reflex so that it interacts with servant.
Although that gets the basics, there are several
more hurdles to overcome to get comfortable with reflex.
I think most of these are encountered by building a simple login system.

The first time this is difficult for several reasons:

1. We need to 'switch screens' after login, which requires recursive do.
2. Dealing with cookies yourself is pretty hard.
3. We need to render widgets based on the login result. This requires 'widgetHold' or 'dyn'

I'm of course a world expert on login systems so do everything
exactly as I do, in prod.
Trust me, I'm from the internet.
But seriously If you see something dubious which I'm not pointing out do contact me, 
I'll happily rectify mistakes.

# API Endpoints
```haskell
type ServiceAPI = PublicAPI :<|> Auth '[Cookie, JWT] User :> AuthAPI

type PublicAPI = "api" :> "1.0" :> "login" :> ReqBody '[JSON] User
								:> Post '[JSON] (AuthCookies NoContent)

type AuthAPI =
           "api" :> "1.0" :> "me" :> Get '[JSON] User
      :<|> "api" :> "1.0" :> "users" :> Get '[JSON] [User]
      :<|> "api" :> "1.0" :> "message" :> ReqBody '[JSON] Message
						  :> Post '[JSON] [Message]

```
First we need to split our API into two types.
The public API and the authenticated API.
Once the user is logged in he gets access to the authenticated API,
otherwise he'll get a 401 unauthorized status code.

The ServiceAPI was the name of our original API in the [previous blog post]({filename}/fullstack-haskell-reflex-servant.md).
It only contained the users and message endpoints.
Now we've extended it with a login and `getme` endpoint.
The login is obviously used to login,
and the `getme` is a hack to do auto login with cookies.
More on this later.

There are also a bunch of type aliases for cookie headers.
These are there just boilerplate,
the browser will handle cookies on client side,
whereas on server side we'll use some dedicated functions to add these headers
provided by [servant auth](http://hackage.haskell.org/package/servant-auth-server).

Next we'll implement these types into the actual endpoints.
The backend first as it's the most complicated one:

```haskell
login :: ApiSettings -> User -> Handler (AuthCookies NoContent)
login settings user = if elem user users then do
    withCookies <- liftIO $ acceptLogin cookies (jwtSettings settings) user
    pure $ maybe (clearSession cookies NoContent) (\x -> x NoContent) withCookies
  else throwAll err401 -- unauthorized
  where
    cookies = cookieSettings settings

authenticatedServer :: ApiSettings -> AuthResult User -> Server AuthAPI
authenticatedServer settings (Authenticated user) =
    (pure user :<|> pure users :<|> messages (connection settings))
authenticatedServer _ _ = throwAll err401 -- unauthorized

server :: ApiSettings -> FilePath -> Server Webservice
server settings staticFolder =
	(login settings :<|> authenticatedServer settings)
	:<|> serveDirectoryFileServer staticFolder

app :: ApiSettings -> FilePath -> Application
app settings staticFolder =
  serveWithContext webservice context $ server settings staticFolder
  where
    context = cookieSettings settings :. jwtSettings settings :. EmptyContext
```

We added a new login handler, which just checks the existing user list if the user
exists.
If the user is in the list, we use `acceptLogin` to create a JWT from the user.
The authenticated server handles the endpoints for the authenticated API.
The only new one is `getMe`, which just returns the authenticated user
(from the JWT).

This is probably not the way you want to do login, for the following reasons:

+ User login information is public,
  there is no secret pass so we're never sure if a user is who he says he is.
+ One shouldn't use JWT's for [sessions](http://cryto.net/~joepie91/blog/2016/06/13/stop-using-jwt-for-sessions/)

But since we're more interested in reflex we'll call it version 0.1
and move on.

I quickly want to discuss the settings which is a datatype:

```haskell
data ApiSettings = ApiSettings
  { cookieSettings :: CookieSettings
  , jwtSettings    :: JWTSettings
  , connection     :: Connection
  }
```
You can't use connection like this in prod, it needs to be a [pool](http://hackage.haskell.org/package/resource-pool-0.2.3.2/docs/Data-Pool.html),
because servant is [fully concurrent](https://www.reddit.com/r/haskell/comments/4tq2q0/does_servant_run_on_multicore_cpu/).
You'll end up with data races if using a plain connection.

For the cookie config I modified the defaults quite a bit:
```haskell
      defaultCookieSettings
        { cookieIsSecure = NotSecure
        , cookieXsrfSetting = Nothing
        -- ^ TODO add xsrf protection 
        , cookieMaxAge = Just $ secondsToDiffTime $ 60 * 60 * 24 * 365
        }
```
I haven't spend time on getting [XSRF](https://github.com/haskell-servant/servant-auth#xsrf-and-the-frontend)
to work properly.
Cookies are set to not secure to allow it to work on HTTP.
Which is required for local testing.
The max age is simply an auto sign out after a period,
in this case a year.

The client API is much simpler:
```haskell
postLogin :: MonadWidget t m
          => Dynamic t (Either Text.Text User)
          -> Event t ()
          -> m (Event t (ReqResult () (AuthCookies NoContent)))
getUsers :: MonadWidget t m
          => Event t ()
          -> m (Event t (ReqResult () [User]))
getMe :: MonadWidget t m
          => Event t ()
          -> m (Event t (ReqResult () User))
postMessage :: MonadWidget t m
            => Dynamic t (Either Text.Text Message)
            -> Event t ()
            -> m (Event t (ReqResult () [Message]))
(postLogin :<|> (getMe :<|> getUsers :<|> postMessage)) = apiClients
```
This is 95% redefining in reflex terms what goes into these functions,
and 5% expending the API.

# Reflex
The reflex changes are rather small, but it's pretty dense:
```haskell
loginWidget :: (MonadWidget t m) => m (Event t User)
loginWidget = do
  pb <- getPostBuild
  autoLoginEvt <- getMe pb
  user <- userInput
  buttonEvt <- button "login"
  postResultEvt <- postLogin (Right <$> user) buttonEvt 
  void $ flash postResult $ text . Text.pack . show . reqFailure
  pure $ leftmost [withSuccess autoLoginEvt, current user <@ withSuccess postResultEvt]
```
First we get the post build event, an event that fires after
the widget is placed in the DOM.
We use that to greedily try and get a me.
If successful we'll use that as the resulting event (see the final line).
However we assume it's not successful and start rendering the login
form with `userInput`.
This input has remained the same as in the [previous blog post]({filename}/fullstack-haskell-reflex-servant.md).
Then we create a button, and use the resulting `buttonEvt` event to call `postLogin`
with the User from the form.
Then we flash the `postResultEvt` in case of failure,
in case of success we tag the `postResultEvt` with the user that's
filled in the form and use that as resulting event.
Note that `<@`, is the same as `<$`, except it works on behaviors:
The event gets the value of whatever the behavior is.

Next we move onto the function that ties everything together:
```haskell
reflex :: MonadWidget t m => m ()
reflex = do
  rec loginEvt <- elDynAttr "div" loginAttr loginWidget
      loginAttr <- holdDyn (Map.empty) $ hidden <$ loginEvt
  void $ holdEvent () loginEvt authenticatedWidget

```
The rec keyword is from [recursive do](https://wiki.haskell.org/MonadFix).
It allows you to reference variables higher in a do block.
In our case we need to have `loginAttr` within the `elDynAttr` function.
`loginAttr` is used before it's defined.
This is impossible in a normal do block,
however it is possible with recursive do.
[This](http://hackage.haskell.org/package/base-4.12.0.0/docs/Control-Monad-Fix.html)
explains how it works, (note the thesis on value recursion too).
Let's just say it's magic.

To start of we put the login widget in a div with dynamic attributes.
These attributes are set on the next line, which is an empty Map,
until the login event happens, than it becomes `hidden`
(a map with `display:none`).
Then we use the `holdEvent` function to extract the user as a value 
on login.

The `holdEvent` functions is a convenience function around
[widgetHold](https://hackage.haskell.org/package/reflex-dom-core-0.4/docs/Reflex-Dom-Widget-Basic.html#v:widgetHold):
```haskell`
holdEvent :: (Dom.DomBuilder t m, MonadHold t m)
  => b
  -> Event t a
  -> (a -> m b)
  -> m (Dynamic t b)
holdEvent val evt fun =
  Dom.widgetHold (pure val) $ fun <$> evt
```
The first argument is the default value.
The second argument is the event which we want to hold.
The final argument is the function we want to execute producing a widget.
The function keeps returning the default value,
until the event fires for the first time,
then it will keep on displaying the fired event.

Note that widgetHold is slow (and [dyn](https://hackage.haskell.org/package/reflex-dom-core-0.4/docs/Reflex-Dom-Widget-Basic.html#v:dyn)
for that matter), as they modify the DOM.
(even though some [reddit thread](https://www.reddit.com/r/javascript/comments/6115ay/why_do_developers_think_the_dom_is_slow/)
disagrees with me thinking the DOM is slow and saying it's because of
[layout trashing](https://gist.github.com/paulirish/5d52fb081b3570c81e3a).
Heretics).

From my own experience, it's much better to use [dynText](https://hackage.haskell.org/package/reflex-dom-core-0.4/docs/Reflex-Dom-Widget-Basic.html#v:dynText)
and [elDynAttr](https://hackage.haskell.org/package/reflex-dom-core-0.4/docs/Reflex-Dom-Widget-Basic.html#v:elDynAttr)
to modify the dom/layout.
However, widgetHold is really convenient to just get values out of the events.
Another advantage of widgetHold is,
parts of the app inside it don't get evaluated until the event
occurs.
This is really convenient for login.
It doesn't load the bulk of your app on initial page load.
widgetHold and dyn can effectively stop large parts of your
app from being evaluated.
Which makes that initial render much faster.
(Come to think about it,
widgetHold and dyn may be a good candidate for code splitting entry points).

Anyway as we can see from the type signature,
the `b=()`, and the `a=User`. Which leads us to authenticatedWidget:
```haskell
authenticatedWidget :: MonadWidget t m => User -> m ()
authenticatedWidget user =
  el "div" $ do
    getUsersWidget
    sendMsgWidget user
```
This is the same as the app discussed in the previous blog post.
Although now we use the logged in user to send messages.

# Conclusion
So there you have it. Authentication.
Yes nobody gets excited about it.
But usually once this is done you can start making something cool.
PM me your cool projects.
