Title: Authentication in Reflex & Servant
Date: 2019-02-26 12:00
Category: tools
OPTIONS: toc:nil
Tags: haskell, programming, tools, reflex, frp
subreddit: haskell programming reflexfrp

In the previous [blog post]({filename}/fullstack-haskell-reflex-servant.md)
we saw reflex being setup for interaction with servant.
Although that covers the basics,
there are several more hurdles to overcome to get comfortable with Reflex.
I think most of these are encountered by building a simple login system.
So let's build something like:
```
													   |
                   	   +------------------------+	   |         -----
   +-----------+       |  	   .      -         |	   |        ( < < )
   | +--------+|       |	 .    -  .. .       |	   |         +-v-+ 
   | +--------+|       |    . .-- - + m..-.     |	   |       ----+-----
   | +--------+|    \  |      m# + .-..% -      |	   |           |
   | +--------+|     \ |      + .+ #.+...+      |	   |           /
   | login ----+------X|       .. -.- *         |	   |          / \
   +-----------+     / |       ..  ..-#+ +      |	   |         /   \
                    /  |         .  - -. .      |	   |	      Bill
                   	   |            . .         |	   |
					   |             -          |	   |
					   |                        |	   |
		     		   +------------------------+	   |
					             Awesome app		   |

```

From my own experience,
doing this with reflex and to a lesser extend servant-auth
is hard.
With this blog post I hope make setting up authentication easier,
considering the following pain points:

1. 'Switching screens' after login, requires [recursive do](https://wiki.haskell.org/MonadFix).
2. [Dealing with cookies](http://hackage.haskell.org/package/servant-auth-server-0.4.3.0/docs/Servant-Auth-Server.html)
	yourself is pretty hard, many pesky (security) details.
3. We need to render widgets based on the login result. This requires [widgetHold](https://hackage.haskell.org/package/reflex-dom-core-0.4/docs/Reflex-Dom-Widget-Basic.html#v:widgetHold)
   or 'dyn'.

I'm of course a world expert on login systems so do everything
exactly as I do, in production.
Trust me, I'm from the internet.
But seriously If you see something dubious which I'm not pointing out,
do contact me.
I'll happily rectify mistakes,
and put you on the [page of honour](/pages/page-of-honour.html) (if you want).

The full source for this 'simple login system'
is available on [github](https://github.com/jappeace/awesome-project-name/tree/auth),
I left some code out of this blog post for succinctness.

# API Endpoints
```haskell
type ServiceAPI = PublicAPI :<|> Auth '[Cookie, JWT] User :> AuthAPI

```
First we need to split our API into two types.
The `PublicAPI` and the `AuthAPI`.
Once the user is logged in he gets access to the `AuthAPI`,
trough cookies.
If he does not have a proper cookie, he'll get a [401 unauthorized](https://tools.ietf.org/html/rfc7235#section-3.1)
status code.

```haskell
type PublicAPI = "api" :> "1.0" :> "login" :> ReqBody '[JSON] User
								:> Post '[JSON] (AuthCookies NoContent)
```
This is our entire public api, we only expose the login endpoint.
The result is no content and cookies.
Note that we don't model status codes in servant.

```haskell
type AuthAPI =
           "api" :> "1.0" :> "me" :> Get '[JSON] User
      :<|> "api" :> "1.0" :> "users" :> Get '[JSON] [User]
      :<|> "api" :> "1.0" :> "message" :> ReqBody '[JSON] Message
						  :> Post '[JSON] [Message]
```
The AuthAPI is similar to the ServiceAPI was the name
from the [previous blog post]({filename}/fullstack-haskell-reflex-servant.md).
It only contained the users and message endpoints.
Now we've extended it with a `getme` endpoint.
The `getme` is a hack to do auto login with cookies.
It allows us to do a request on initial page load to see if we have cookies or not.
Technically we shouldn't have to do this,
but I believe reflex is sandboxed from browser cookies.
In any case it works for version `0.1`.

Next we'll implement these types into handlers:
```haskell
login :: ApiSettings -> User -> Handler (AuthCookies NoContent)
login settings user = if elem user users then do
	withCookies <- liftIO $
		acceptLogin cookies (jwtSettings settings) user
	pure $ maybe (clearSession cookies NoContent) (\x -> x NoContent)
		withCookies
  else throwAll err401 -- unauthorized
  where
    cookies = cookieSettings settings
```
We added a new login handler, which checks if the exists within the users list.
If the user is in the list, we use `acceptLogin` to create a JWT from the user.
The `ApiSettings` is just a data type with various configurations:
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
There is no good reason for using plain HTTP.

The max age is simply an auto sign out after a period,
in this case a year.
This is a bit more secure in that we won't trust
someone's login forever.

```haskell
authenticatedServer :: ApiSettings -> AuthResult User -> Server AuthAPI
authenticatedServer settings (Authenticated user) =
    (pure user :<|> pure users :<|> messages (connection settings))
authenticatedServer _ _ = throwAll err401 -- unauthorized
```
The authenticated server handles the endpoints for the authenticated API.
The only new one is `getMe`, which just returns the authenticated user
(from the JWT).
All authetnicated endpoints now have access to the user who was
authenticated. This user is just decoded from the JWT.

```haskell
server :: ApiSettings -> FilePath -> Server Webservice
server settings staticFolder =
	(login settings :<|> authenticatedServer settings)
	:<|> serveDirectoryFileServer staticFolder
```
The server function has to split our api on authenticated and public parts.
We still serve the static folder for testing.

```haskell
app :: ApiSettings -> FilePath -> Application
app settings staticFolder =
  serveWithContext webservice context $ server settings staticFolder
  where
    context = cookieSettings settings :. jwtSettings settings :. EmptyContext
```
This is the final boilerplate.
We now serve with context,
which does the decoding of the JWT from the Cookie.

This is probably not the way you want to do login, for the following reasons:

+ We don't handle passwords, period. (Authentication by trust is a thing?)
+ One shouldn't use JWT's for [sessions](http://cryto.net/~joepie91/blog/2016/06/13/stop-using-jwt-for-sessions/)

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
but I like using them
to increase my trust in this generation magic.
I still can't really believe the clients just work like this,
so as a bit of "life line" I add these type signatures.

# Reflex
Enough dirty hax, time for some reflex.
The reflex changes are rather small but dense:
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
Like I said it's pretty dense so let's go trough
it in parts.

```haskell
loginWidget :: (MonadWidget t m) => m (Event t User)
loginWidget = do
  pb <- getPostBuild
  autoLoginEvt <- getMe pb
  ...
  pure $ leftmost
	[ withSuccess autoLoginEvt
	...
```
First we get the [post build event](https://hackage.haskell.org/package/reflex-0.5/docs/Reflex-PostBuild-Class.html),
an event that fires after the widget is placed in the DOM.
We use that to greedily try and get a me.
If successful that is used as the resulting event.
Regardles of success, we also specify the login form with `userInput`:
```haskell
userInput :: (MonadWidget t m) => m (Dynamic t User)
userInput = ...

loginWidget :: (MonadWidget t m) => m (Event t User)
loginWidget = do
  ...
  user <- userInput
  buttonEvt <- button "login"
  postResultEvt <- postLogin (Right <$> user) buttonEvt
  ...
```
The `userInput` has remained the same as in the
[previous blog post]({filename}/fullstack-haskell-reflex-servant.md).
After the input we create a button,
which gives us a resulting `buttonEvt` event.
This event only fires if the button is pressed.
We use the `buttonEvt` to call `postLogin`.
As input we use the dynamic `user` from the form.
This gives us a `postResultEvt`,
an event that only fires on request completion.
Remember postLogin is a function generated from our api type signature:
```haskell
postLogin :: MonadWidget t m
          => Dynamic t (Either Text.Text User)
          -> Event t ()
          -> m (Event t (ReqResult () (AuthCookies NoContent)))
```

The `ReqResult` is a container for dealing with Http status codes,
connection errors and decoding issues [^clearcache].
[^clearcache]: What I'm doing in production is a clear cache refres on any
	4xx status code and decode/encoding errors.
	This automatically will fix any stale clients without any mental overhead.
	I just attach a monad to most api calls which does that.

Here we'll just flash a message on any error:
```haskell
loginWidget :: (MonadWidget t m) => m (Event t User)
loginWidget = do
  ...
  void $ flash postResult $ text . Text.pack . show . reqFailure
  pure $ leftmost
	[ ...
	, current user <@ withSuccess postResultEvt
	]
```
In case of failure the `postResultEvt` is 'flashed',
or shown briefly for a couple of seconds.
In case of success we tag the `postResultEvt` with the `user`
and use that as resulting event.

Note that `<@`, is the same as `<$`, except it works on behaviors:
The event gets the value of whatever the behavior is at the time of the event.
A behavior is something that always has a value, but which can
change at any moment.
Whereas an event is something that happens at a point in time with some
value.
A mouse position is an example of behavior,
whereas a mouse click is an event.
Our user form returns a dynamic user,
which is both a behavior as well as an event:
the event fires whenever the behavior changes value.

Next we move onto the function that ties everything together:
```haskell
reflex :: MonadWidget t m => m ()
reflex = do
  rec loginEvt <- elDynAttr "div" loginAttr loginWidget
      loginAttr <- holdDyn (Map.empty) $ hidden <$ loginEvt
  void $ holdEvent () loginEvt authenticatedWidget

```
This function will hide the `loginWidget` once the login event happens.
It will also put the `authenticatedWidget` on the DOM once the
login event happens.

`loginAttr` is used before it's assigned.
This is impossible in a normal do block,
however it is possible within a `rec` block.
The `rec` keyword is from [recursive do](https://wiki.haskell.org/MonadFix).
It allows referencing of variables 'higher' in a do block.
In our case we need to have `loginAttr` within the `elDynAttr` function.

I don't want to go into how `rec` works[^howitworks],
but I do want to make clear why it's needed.
The `loginAttr` dynamic we made depends on the `loginEvt` which in turn
is made by the `loginWidget`.
However to render the `loginWidget` we need to have `loginAttr`.
Which is a cycle[^breakcycle].

[^breakcycle]: I currently believe that
	cycle can be broken because we start with the prepostion that a
	dynamic and behavior always has a value
	even in this case we can just set it to empty, until the event fires,
	which is by definition after rendering.
	This also explains why you use a sampled value from a behavior in a `rec`
	block.

[^howitworks]: [This](http://hackage.haskell.org/package/base-4.12.0.0/docs/Control-Monad-Fix.html)
	explains how it works, note the thesis on value recursion too.

So the login widget resides in a `div` with dynamic attributes.
These attributes are set on the next line,
which starts out as an empty Map, no attributes.
Once the login event happens, it becomes the hidden map.
Setting style to `display:none`.

`holdEvent` is used to extract the user as a value from the event
and render `authenticatedWidget` as a new part of the DOM.
The `holdEvent` functions is a convenience function:
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

Note that `widgetHold` is slow [^dynSlow] because it modifies the DOM [^heretics].
From my own experience, it's much better to use [dynText](https://hackage.haskell.org/package/reflex-dom-core-0.4/docs/Reflex-Dom-Widget-Basic.html#v:dynText)
and [elDynAttr](https://hackage.haskell.org/package/reflex-dom-core-0.4/docs/Reflex-Dom-Widget-Basic.html#v:elDynAttr)
to modify the dom/layout.
However, `widgetHold` is really convenient to just get
values out of the events.
I also think that
the
parts inside a widgetHold function don't get evaluated until the event
occurs.
This is really convenient for login.
Now you don't have to evaluate the bulk of your app on initial page load.
`widgetHold` and `dyn` can effectively stop large parts of your
app from being evaluated.
Which makes that initial render much faster[^codeSplitting].

[^codeSplitting]: I still think reflex load times are too slow, especially on mobile.
	I know about the app option, but an app is just a dirty hack to get your crappy website to speed up.
	It's much better to have everything as a website, it results in less maintenance and better UX
	(if you can pull it off).
	Nobody should have to install anything in 2019.
	But the mobile web will remain slow for
	[good reasons](https://www.youtube.com/watch?v=4bZvq3nodf4)
	and if I knew how I'd happily help speeding up ghcjs and reflex.
	I think for example that widgetHold and dyn may be a good candidates for code
	splitting entry points.
	But I believe you'd need to make the compiler aware of that somehow.
	I also believe you probably don't need the entire haskell runtime immediatly,
	lazily loading exceptions would be good for example.
[^dynSlow]: And [dyn](https://hackage.haskell.org/package/reflex-dom-core-0.4/docs/Reflex-Dom-Widget-Basic.html#v:dyn) for that matter
[^heretics]: Even though some [reddit thread](https://www.reddit.com/r/javascript/comments/6115ay/why_do_developers_think_the_dom_is_slow/)
	disagrees with me thinking the DOM is slow and saying it's because of
	[layout trashing](https://gist.github.com/paulirish/5d52fb081b3570c81e3a).
	Heretics.

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
but once this is done you can start making something cool.
I hope I helped you get trough this ordeal fast,
and explain some of the finer reflex points.
Now I hope to see many cool reflex projects popping up.
PM me your cool projects.

# References
+ [Previous blog]({filename}/fullstack-haskell-reflex-servant.md)
+ [Reflex](https://hackage.haskell.org/package/reflex)
+ [Reflex dom](https://hackage.haskell.org/package/reflex-dom-core-0.4)
+ [Servant reflex](http://hackage.haskell.org/package/servant-reflex-0.3.3/candidate)
   (still a candidate, I already [pestered them](https://github.com/imalsogreg/servant-reflex/issues/23#issuecomment-467178912))
+ [MonadFix](https://wiki.haskell.org/MonadFix)
+ [Control.Monad.Fix](http://hackage.haskell.org/package/base-4.12.0.0/docs/Control-Monad-Fix.html)
+ [Recursive do language extension](https://downloads.haskell.org/~ghc/latest/docs/html/users_guide/glasgow_exts.html#the-recursive-do-notation)
