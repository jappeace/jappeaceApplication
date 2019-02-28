Title: Authentication in Reflex & Servant
Date: 2019-02-26 12:00
Category: tools
OPTIONS: toc:nil
Tags: haskell, programming, tools, reflex, frp
subreddit: haskell programming reflexfrp

In the previous [blog post]({filename}/fullstack-haskell-reflex-servant.md)
we saw interaction with servant in reflex.
Although that covers the basics,
there are several more hurdles to overcome to get comfortable with Reflex.
I think most of these are encountered by building a simple login system.
So let's build something like:
```
													
                   	   +------------------+
   +-----------+       |    .      -      |
   | +--------+|       | .    -  .. .     |
   | +--------+|       | . .-- - + m..-.  |
   | +--------+|    \  |   m# + .-..% -   |
   | +--------+|     \ |   + .+ #.+...+   |
   | login ----+------X|    .. -.- *      |
   +-----------+     / |    ..  ..-#+ +   |
                    /  |      .  - -. .   |
                   	   |         . .      |
					   |          -       |
					   |                  |
		     		   +------------------+
					        Awesome app	

```

I've experienced that this is hard for the first time.
With this blog post I hope that setting up authentication becomes easier,
considering the following pain points:

1. 'Switching screens' after login, requires [recursive do](https://wiki.haskell.org/MonadFix).
2. [Dealing with cookies](http://hackage.haskell.org/package/servant-auth-server-0.4.3.0/docs/Servant-Auth-Server.html)
	yourself is pretty hard, many pesky (security) details.
3. Rendering widgets inside FRP constructs requires [widgetHold](https://hackage.haskell.org/package/reflex-dom-core-0.4/docs/Reflex-Dom-Widget-Basic.html#v:widgetHold)
   or 'dyn'.

I'm of course a world expert on login systems, so in production do everything
exactly as I do.
Trust me, I'm from the internet.
But seriously If you see something dubious,
do contact me.
I'll happily rectify mistakes,
and put you on the [page of honour](/pages/page-of-honour.html) (if you want).

I left some code out of this blog post for succinctness.
But the full source
is available on [github](https://github.com/jappeace/awesome-project-name/tree/auth).

# API Endpoints
```haskell
type ServiceAPI = PublicAPI :<|> Auth '[Cookie, JWT] User :> AuthAPI

```
First we need to split our API into two types.
The `PublicAPI` and the `AuthAPI`.
Once the user is logged in he gets access to the `AuthAPI`.
We'll secure this with a JWT cookie.
If the user does not have a proper cookie,
he'll get a [401 unauthorized](https://tools.ietf.org/html/rfc7235#section-3.1)
status code.

```haskell
type PublicAPI = "api" :> "1.0" :> "login" :> ReqBody '[JSON] User
								:> Post '[JSON] (AuthCookies NoContent)
```
This is our entire public api, we only expose the login endpoint.
The result contains no content, but cookies.
Servant assumes all status codes are possible in every request.
Therefore we don't have to mention the 401 status code.

```haskell
type AuthAPI =
           "api" :> "1.0" :> "me" :> Get '[JSON] User
      :<|> "api" :> "1.0" :> "users" :> Get '[JSON] [User]
      :<|> "api" :> "1.0" :> "message" :> ReqBody '[JSON] Message
						  :> Post '[JSON] [Message]
```
The `AuthAPI` is similar to the `ServiceAPI`
from the [previous blog post]({filename}/fullstack-haskell-reflex-servant.md),
which only contained the `users` and `message` endpoints.
Now we've extended it with a `getme` endpoint.
The `getme` endpoint is a hack to do auto login with cookies.
It allows us to do a request on initial page load to see
if we have the cookies or not.
Technically we shouldn't have to do this trough a request,
but it works for version `0.1`.

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
We added a new login handler, which checks if the user
exists within the users list.
If the user is in the list, we use `acceptLogin` to create a JWT from the user.
`[A]cceptLogin` returns maybe a function which applies the JWT cookie.
In the success branch of maybe we apply this function to `NoContent` to get an `AuthCookies NoContent`.
The `Nothing` branch also produces `AuthCookies NoConent`,
but it sets the cookie values with `clearSession` resulting in nothing instead of a JWT.

The `ApiSettings` is just a data type with various configurations:
```haskell
data ApiSettings = ApiSettings
  { cookieSettings :: CookieSettings
  , jwtSettings    :: JWTSettings
  , connection     :: Connection
  }
```
You can't use connection like this in production,
it needs to be a [pool](http://hackage.haskell.org/package/resource-pool-0.2.3.2/docs/Data-Pool.html),
because servant is [fully concurrent](https://www.reddit.com/r/haskell/comments/4tq2q0/does_servant_run_on_multicore_cpu/).
You'll end up with data races if you use a plain connection.

For the `cookieSettings` I modified the defaults quite a bit:
```haskell
cookieConf =
  defaultCookieSettings
    { cookieIsSecure = NotSecure
    , cookieMaxAge = Just $ secondsToDiffTime $ 60 * 60 * 24 * 365
    , cookieXsrfSetting = Just $
        def { xsrfCookieName = Text.encodeUtf8 cookieName
            , xsrfHeaderName = Text.encodeUtf8 headerName
            }

    }
```
Cookies are set to `NotSecure` to allow it to work on HTTP.
This is required for local testing,
and avoids confusion about why your
cookies, and your entire login system, don't work locally.
You should simply disable HTTP in production anyway.
There is no good reason for using plain HTTP on a live website,
ever since [let's encrypt](https://letsencrypt.org/)
became a thing.

The max age is simply an auto sign out after a period,
a year in this case.
This is a bit more secure because we don't trust
someone's login forever.

XSRF settings are set to use the names from the Common XSRF module.
This ensures the requests from the frontend use the same names as
servant-auth expects.

```haskell
authenticatedServer :: ApiSettings -> AuthResult User -> Server AuthAPI
authenticatedServer settings (Authenticated user) =
    (pure user :<|> pure users :<|> messages (connection settings))
authenticatedServer _ _ = throwAll err401 -- unauthorized
```
The `authenticatedServer` handles the endpoints for the authenticated API.
The only new one is `getMe`, which just returns the authenticated user.
All authenticated endpoints now have access to the user who was
authenticated. This user is decoded from the JWT by servant-auth.

We get an `AuthResult` from servant-auth-server to work with.
If the user is authenticated, we give access to the API,
if not we return a 401 status code.
This is done manually, which means the 401 response is not mandatory.

```haskell
server :: ApiSettings -> FilePath -> Server Webservice
server settings staticFolder =
	(login settings :<|> authenticatedServer settings)
	:<|> serveDirectoryFileServer staticFolder
```
The server function now has to split our API on authenticated and public parts.
It's similar to the previous blog post.
We still serve the static folder for testing.

```haskell
app :: ApiSettings -> FilePath -> Application
app settings staticFolder =
  serveWithContext webservice context $ server settings staticFolder
  where
    context = cookieSettings settings :. jwtSettings settings :. EmptyContext
```
We now serve with context,
this is the servant-auth entry point for
decoding of the JWT from the Cookie.

This is probably not the way you want to do login on the sever side
for the following reasons:

+ We don't handle passwords, period. (Authentication by trust is a thing?)
+ One shouldn't use JWT's for [sessions](http://cryto.net/~joepie91/blog/2016/06/13/stop-using-jwt-for-sessions/)

Solving these issues is out of the scope of this article[^scope].
[^scope]: My articles tend to snowball anyway, for example invented how to do XSRF specifically for this article.
	I Don't want to cargo cult a bunch of XSRF vulnerable websites.

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

# Reflex
Time for some reflex.
The reflex changes are rather small but dense:
```haskell
loginWidget :: (MonadWidget t m) => m (Event t User)
loginWidget = do
  autoLoginEvt <- autoLogin
  formEvt <- loginForm
  pure $ leftmost [formEvt, autoLoginEvt]
```
This function performs either an auto login,
or shows a login form.
leftmost is a function that combines events,
by using the value of whichever fires.
If they fire both,
the element that occurs first in the list is used.
Hence the name leftmost.

Now let's dive into it's sub components,
first `autologin`:
```haskell
autoLogin :: (MonadWidget t m) => m (Event t User)
autoLogin = do
  pb <- getPostBuild
  withSuccess <$> getMe pb
```
We get the [post build event](https://hackage.haskell.org/package/reflex-0.5/docs/Reflex-PostBuild-Class.html),
an event that fires after the widget is placed in the DOM.
We use that to greedily to make the `getMe` request.
If successful that is used as the resulting event.
However the loginWidget makes a login form regardless of success:
```haskell
userInput :: (MonadWidget t m) => m (Dynamic t User)
userInput = ...

loginForm :: (MonadWidget t m) => m (Event t User)
loginForm = do
  user <- userInput
  buttonEvt <- button "login"
  postResult <- postLogin (Right <$> user) buttonEvt
  void $ flash postResult $ text . Text.pack . show . reqFailure
  pure $ current user <@ withSuccess postResult
```

The `userInput` has remained the same as in the
[previous blog post]({filename}/fullstack-haskell-reflex-servant.md).
After the `userInput` form we create a button,
which gives us a resulting `buttonEvt` event.
This event only fires if the button is pressed.
We use the `buttonEvt` to call `postLogin`.
As input we use the dynamic `user` from the `userInput` form.
This gives us a `postResultEvt`,
an event that only fires on request completion.
Remember `postLogin` is a function generated from our API type signature:

```haskell
postLogin :: MonadWidget t m
          => Dynamic t (Either Text.Text User)
          -> Event t ()
          -> m (Event t (ReqResult () (AuthCookies NoContent)))
```

The `ReqResult` is a container for dealing with HTTP status codes,
connection errors and decoding issues [^clearcache].
In case of failure the `postResultEvt` is 'flashed',
or shown briefly for a couple of seconds.
In case of success we tag the `postResultEvt` with the `user`
and use that as resulting event.

[^clearcache]: What I'm doing in production is a clear cache refres on any
	4xx status code and decode/encoding errors.
	This automatically will fix any stale clients without any mental overhead.
	I just attach a monad to most api calls which does that.


Note that `<@`, is the same as `<$`, except it works on behaviors:
The event gets the value of whatever the behavior is at the time of the event.
A behavior is something that always has a value, but which can
change at any moment.
Whereas an event is something that happens at a point in time with some
value.
A mouse position is an example of behavior,
whereas a mouse click is an event[^dynamics].

[^dynamics]: For completeness: 
	The `userInput` form returns a dynamic user,
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

Once the login event happens
this function will hide the `loginWidget` 
and put the `authenticatedWidget` on the DOM.
`loginAttr` is used before it's assigned however.
This is impossible in a normal do block,
but it is possible within a `rec` block.
The `rec` keyword is from [recursive do](https://wiki.haskell.org/MonadFix).
It allows referencing of variables 'higher' in a do block.
In our case we need to have `loginAttr` within the `elDynAttr` function.

I don't want to go into how `rec` works[^howitworks],
but I do want to make clear why it's needed.
There is a reference cycle[^breakcycle].
Look closely at `loginAttr`.
It depends on `loginEvt`.
Now look at how the `loginEvt` is made.
It comes from a div that requires `loginAttr`.
A cycle.
I don't know of any other way to solve this than using
recursive do.

[^breakcycle]: I currently believe that
	in reflex the
	cycle can be broken because we start with the preposition that a
	dynamic and behavior always have a value.
	Even in this case we can just set it to empty, until the event fires,
	which is by definition after rendering.
	This also explains why you use a sampled value from a behavior in a `rec`
	block.

[^howitworks]: [This](http://hackage.haskell.org/package/base-4.12.0.0/docs/Control-Monad-Fix.html)
	explains how it works, note the thesis on value recursion too.

So the login widget resides in a `div` with dynamic attributes.
These attributes are set on the next line,
which starts out as an empty Map, no attributes.
Once the login event happens, it becomes the `hidden`,
which sets the style to `display:none`.

`holdEvent` is used to extract the user as a value from the event
and render `authenticatedWidget` as a new part of the DOM.
The `holdEvent` functions is a convenience function for
[widgetHold](https://hackage.haskell.org/package/reflex-dom-core-0.4/docs/Reflex-Dom-Widget-Basic.html#v:widgetHold):
```haskell
widgetHold :: (DomBuilder t m, MonadHold t m)
  => m a
  -> Event t (m a)
  -> m (Dynamic t a) 

holdEvent :: (Dom.DomBuilder t m, MonadHold t m)
  => b
  -> Event t a
  -> (a -> m b)
  -> m (Dynamic t b)
holdEvent val evt fun =
  Dom.widgetHold (pure val) $ fun <$> evt
```
`widgetHold` will show the first given widget,
until the event happens which has a widget as value.
Then the widget within that event is put onto the DOM
instead of the original.
It's a bit like [sequence](http://hackage.haskell.org/package/base-4.12.0.0/docs/Data-Traversable.html#v:sequence)[^lookingfor]. 
In any case it returns the widget value as a dynamic.

[^lookingfor]: sequence doesn't work because event isn't foldable,
	and it will never be foldable because that breaks FRP semantics.

`holdEvent` however assumes we initially don't want to render
anything on the DOM.
Then it asks you to provide an event with any value and finally
a function that consumes the value to produce the widget.
It will execute the function and display the resulting widget
on the DOM instead of nothing.

The first argument of `holdEvent` is the default value.
The second argument is the event which we want to hold.
The final argument is the function we want to execute producing a widget.
The function keeps returning the default value
until the event fires for the first time,
then it will keep on displaying the fired event.

Note that `widgetHold` is slow [^dynSlow] because it modifies the DOM [^heretics].
It's much better to use [dynText](https://hackage.haskell.org/package/reflex-dom-core-0.4/docs/Reflex-Dom-Widget-Basic.html#v:dynText)
and [elDynAttr](https://hackage.haskell.org/package/reflex-dom-core-0.4/docs/Reflex-Dom-Widget-Basic.html#v:elDynAttr)
to modify the dom/layout.
However, `widgetHold` is really convenient to get
access to values within events.
I also think that the parts inside a widgetHold function
don't get evaluated until the event occurs.
This is really convenient for login.
Now you don't have to evaluate the bulk of your app on initial page load.
`widgetHold` can postpone evaluating large parts of your app.
Which makes that initial render much faster[^codeSplitting].

[^codeSplitting]: I still think reflex load times are too slow, especially on mobile.
	I know about the app option, but an app is just a dirty hack to get your crappy website to speed up.
	It's much better to have everything as a website, it results in less maintenance and better UX
	(if you can pull it off).
	Nobody should have to install anything in 2019.
	But the mobile web will remain slow for
	[good reasons](https://www.youtube.com/watch?v=4bZvq3nodf4)
	and if I knew how I'd happily help speeding up GHCJS and reflex.
	I think for example that widgetHold and dyn may be a good candidates for code
	splitting entry points.
	But I believe you'd need to make the compiler aware of that somehow.
	I also believe you probably don't need the entire Haskell runtime immediately,
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

# XSRF
To make servant reflex work nicely with servant-auth we need
to modify the requests a bit, servant reflex supports this with
`ClientOptions`:
```haskell
clientOpts :: ClientOptions
clientOpts = ClientOptions $ tweakReq
  where
    tweakReq r = do
      mayCookie <- findCookie cookieName
      return $ r & headerMod headerName .~ mayCookie -- forgive lenses
    headerMod d = xhrRequest_config . xhrRequestConfig_headers . at d

apiClients :: forall t m. (MonadWidget t m) => _
apiClients = clientWithOpts
	serviceAPI (Proxy @m) (Proxy @()) (constDyn url) clientOpts
```
The client options lives in the `JSM` monad and gives us an opportunity
to modify the `XHRRequest` how we want.
We make sure the names are the same by using the ones defined
in the common module.

# Conclusion
So there you have it. Authentication.
Not the most exciting thing in the world,
but once this is done you can start making something cool.
I hope I helped you get trough this ordeal fast,
and explain some of the finer reflex points.
Now I hope to see many cool reflex projects popping up.
PM me your cool projects.

# References
With the release of reflex `0.5` we now have updated docs!

+ [Source code](https://github.com/jappeace/awesome-project-name/tree/auth)
+ [Previous blog]({filename}/fullstack-haskell-reflex-servant.md)
+ [Reflex](https://hackage.haskell.org/package/reflex)
+ [Reflex dom](https://hackage.haskell.org/package/reflex-dom-core-0.4)
+ [Servant reflex](http://hackage.haskell.org/package/servant-reflex-0.3.4)
+ [MonadFix](https://wiki.haskell.org/MonadFix)
+ [Control.Monad.Fix](http://hackage.haskell.org/package/base-4.12.0.0/docs/Control-Monad-Fix.html)
+ [Recursive do language extension](https://downloads.haskell.org/~ghc/latest/docs/html/users_guide/glasgow_exts.html#the-recursive-do-notation)
+ [Reflex dom XHR](http://hackage.haskell.org/package/reflex-dom-core-0.4/docs/Reflex-Dom-Xhr.html)
