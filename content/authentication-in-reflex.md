Title: Authentication in Reflex & Servant
Date: 2018-10-09 12:08
Category: tools
OPTIONS: toc:nil
Tags: haskell, programming, tools, reflex, frp
subreddit: haskell programming reflexfrp
status: draft

In the previous [post]({filename}/fullstack-haskell-reflex-servant.md)
I discussed how to setup reflex so that it interacts with servant.
Although that gets the basics,
there are several more hurdles to overcome to get comfortable with Reflex.
I think most of these are encountered by building a simple login system.
So what I want to do is build a simple login system, in which:
The user first sees a login form,
and after a successful login the user sees what we build in the previous post.

With this post I hope to lessen the difficulties I had for other people:

1. 'Switching screens' after login, requires [recursive do](https://wiki.haskell.org/MonadFix).
2. [Dealing with cookies](http://hackage.haskell.org/package/servant-auth-server-0.4.3.0/docs/Servant-Auth-Server.html)
	yourself is pretty hard, many pesky (security) details.
3. We need to render widgets based on the login result. This requires [widgetHold](https://hackage.haskell.org/package/reflex-dom-core-0.4/docs/Reflex-Dom-Widget-Basic.html#v:widgetHold)
   or 'dyn'.
   
I'm of course a world expert on login systems so do everything
exactly as I do, in prod.
Trust me, I'm from the internet.
But seriously If you see something dubious which I'm not pointing out, do contact me, 
I'll happily rectify mistakes, and put you on the [page of honour](/pages/page-of-honour.html) (if you want).

Now is a perfect time for this post because there is a new reflex release on Hackage.
For completeness here are the better docs:

+ [Reflex](https://hackage.haskell.org/package/reflex)
+ [Reflex dom](https://hackage.haskell.org/package/reflex-dom-core-0.4)
+ [Servant reflex](http://hackage.haskell.org/package/servant-reflex-0.3.3/candidate)
   (still a candidate, I already [pestered them](https://github.com/imalsogreg/servant-reflex/issues/23#issuecomment-467178912))

The full source for this 'simple login system'
is available on [github](https://github.com/jappeace/awesome-project-name/tree/auth),
I left some code out of this blog post for succinctness.

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
trough cookies.
If he does not have a proper cookie, he'll get a [401 unauthorized](https://tools.ietf.org/html/rfc7235#section-3.1)
status code.

The ServiceAPI was the name of our original API in the [previous blog post]({filename}/fullstack-haskell-reflex-servant.md).
It only contained the users and message endpoints.
Now we've extended it with a `login` and `getme` endpoint.
The `login` is obviously used to login,
and the `getme` is a hack to do auto login with cookies.
More on this later.

Next we'll implement these types into the actual handlers:
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

We added a new login handler, which checks if the exists within the users list.
If the user is in the list, we use `acceptLogin` to create a JWT from the user.
The authenticated server handles the endpoints for the authenticated API.
The only new one is `getMe`, which just returns the authenticated user
(from the JWT).

This is probably not the way you want to do login, for the following reasons:

+ We don't handle passwords, period. (Authentication by trust is a thing?)
+ One shouldn't use JWT's for [sessions](http://cryto.net/~joepie91/blog/2016/06/13/stop-using-jwt-for-sessions/)

But since we're more interested in reflex we'll call it version 0.1
and move on.

However I quickly want to discuss the settings,
and more flaws:

```haskell
data ApiSettings = ApiSettings
  { cookieSettings :: CookieSettings
  , jwtSettings    :: JWTSettings
  , connection     :: Connection
  }
```
You can't use connection like this in prod, it needs to be a [pool](http://hackage.haskell.org/package/resource-pool-0.2.3.2/docs/Data-Pool.html),
because servant is [fully concurrent](https://www.reddit.com/r/haskell/comments/4tq2q0/does_servant_run_on_multicore_cpu/).
You'll end up with data races if you use a plain connection.

For the `cookieSettings` I modified the defaults quite a bit:
```haskell
      defaultCookieSettings
        { cookieIsSecure = NotSecure -- this is good, comments never lie
        , cookieXsrfSetting = Nothing -- ^ TODO add xsrf protection 
        , cookieMaxAge = Just $ secondsToDiffTime $ 60 * 60 * 24 * 365
        }
```
I haven't got [XSRF](https://github.com/haskell-servant/servant-auth#xsrf-and-the-frontend)
to work properly yet.
It only accepts one request once I enable it.
For the sake of argument I just disabled it.

Cookies are set to NotSecure to allow it to work on HTTP.
Which is required for local testing.
You should simply disable HTTP in production anyway.
There is no good reason for using that.

The max age is simply an auto sign out after a period,
in this case a year.
This is a bit more secure in that we won't trust
someone's login forever.

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
The clients are still generated, the only thing I've done is spell
out the type signatures.
You could also leave those out to avoid boilerplate,
but I like using them as a reference.

# Reflex
Enough dirty hax, time for some reflex.
The reflex changes are rather small, but dense:
```haskell
loginWidget :: (MonadWidget t m) => m (Event t User)
loginWidget = do
  pb <- getPostBuild
  autoLoginEvt <- getMe pb
  user <- userInput
  buttonEvt <- button "login"
  postResultEvt <- postLogin (Right <$> user) buttonEvt 
  void $ flash postResult $ text . Text.pack . show . reqFailure
  pure $ leftmost
	[ withSuccess autoLoginEvt
	, current user <@ withSuccess postResultEvt
	]
```
This function perform either an auto login,
or shows a login form.

First we get the post build event, an event that fires after
the widget is placed in the DOM.
We use that to greedily try and get a me.
If successful we'll use that as the resulting event (see the final line).
However we assume it's not successful and start rendering the login
form with `userInput`.
This input has remained the same as in the [previous blog post]({filename}/fullstack-haskell-reflex-servant.md).
Then we create a button, and use the resulting `buttonEvt` event to call `postLogin`
with the User from the form.
In case of failure the `postResultEvt` is 'flashed',
or shown briefly for a couple of seconds.
In case of success we tag the `postResultEvt` with the user
from the form and use that as resulting event.

Note that `<@`, is the same as `<$`, except it works on behaviors:
The event gets the value of whatever the behavior is at the time of the event.
Remember a behavior is something that always has a value, but which can
change at any moment.
Whereas an event is something that happens at a point in time with some
value.
A mouse position is an example of behavior,
whereas a mouse click is an event.
Our user form returns a dynamic user,
which is both a behavior as well as an event:
the event fires on behavior change.

Next we move onto the function that ties everything together:
```haskell
reflex :: MonadWidget t m => m ()
reflex = do
  rec loginEvt <- elDynAttr "div" loginAttr loginWidget
      loginAttr <- holdDyn (Map.empty) $ hidden <$ loginEvt
  void $ holdEvent () loginEvt authenticatedWidget

```
This function will hide the loginWidget once the login event happens.
It will also put the authenticatedWidget on the dom once the
login event happens.

The rec keyword is from [recursive do](https://wiki.haskell.org/MonadFix).
It allows you to reference variables higher in a do block.
In our case we need to have `loginAttr` within the `elDynAttr` function.
`loginAttr` is used before it's assigned.
This is impossible in a normal do block,
however it is possible within a `rec` block.
[This](http://hackage.haskell.org/package/base-4.12.0.0/docs/Control-Monad-Fix.html)
explains how it works, (note the thesis on value recursion too).
For simplicity, let's just say it's magic.

To start of we put the login widget in a div with dynamic attributes.
These attributes are set on the next line, which is an empty Map,
until the login event happens, than it becomes `hidden`
(a map with `display:none`).
Then we use `holdEvent` to extract the user as a value
from the event. It also renders a new part of the DOM.

The `holdEvent` functions is a convenience function around
[widgetHold](https://hackage.haskell.org/package/reflex-dom-core-0.4/docs/Reflex-Dom-Widget-Basic.html#v:widgetHold):
```haskell
holdEvent :: (Dom.DomBuilder t m, MonadHold t m)
  => b
  -> Event t a
  -> (a -> m b)
  -> m (Dynamic t b)
holdEvent val evt fun =
  Dom.widgetHold (pure val) $ fun <$> evt
```
This function will use the first argument as default dynamic value
until the event occurs, then it will execute the function
and display that on the dom instead of nothing.

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
However, widgetHold is really convenient to just values out of the events.
Another advantage of widgetHold is that
the parts of the app inside it don't get evaluated until the event
occurs.
This is really convenient for login.
It doesn't evaluate the bulk of your app on initial page load.
widgetHold and dyn can effectively stop large parts of your
app from being evaluated.
Which makes that initial render much faster.
(Come to think about it,
widgetHold and dyn may be a good candidate for code splitting entry points).

Anyway as we can see from the type signature,
in this case the `b ~ ()`, and the `a ~ User`.
Which leads us to authenticatedWidget:
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
Not the most exciting thing in the world,
but usually once this is done you can start making something cool.

I hope I helped you get trough that part faster,
and to see many cool reflex projects popping up.

PM me your cool projects.
