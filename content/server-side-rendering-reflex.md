Title: Reflex server side html rendering
Date: 2019-08-05 19:05
Category: tools
OPTIONS: toc:nil
Tags: haskell, programming, tools, servant, tutorial
subreddit: haskell programming reflexfrp

[Reflex]({filename}/fullstack-haskell-reflex-servant.md)
is a single page app framework written in Haskell compiled to JavaScript.
A major concern with reflex is the slow loading times,
this can be mediated however by doing server side rendering[^brag].
This blog post will discuss how to do that.

[^brag]: Going from 8 seconds in my own app to about 0.5 seconds.

![Bob doing SSR](/images/2019/bob-busy.jpeg)

The main idea is that we can share most of our single page app code 
on both the frontend as well as the backend.
The backend will create a static HTML page,
whereas the frontend will be a JavaScript client.
The browser can display this HTML content very fast without having to
interpret any JavaScript.
This is where all the speed gain comes from. [^server]
JavaScript developers call this technique [‘isomporhic’](https://medium.com/capital-one-tech/why-everyone-is-talking-about-isomorphic-universal-JavaScript-and-why-it-matters-38c07c87905)[^category]
JavaScript.
This blogpost will tell you how to do it with reflex.
I found it surprisingly easy, and the results very impressive.

[^server]: The server doesn't need to interpret JavaScript,
		Haskell can also target native as compile target.
		But even if it were running JavaScript you can still get
		speed gains by doing page caching.
[^category]:So it’s not isomorpism categorical terms, but JavaScript terms.

The [renderStatic](http://hackage.haskell.org/package/reflex-dom-core-0.5/docs/Reflex-Dom-Builder-Static.html#v:renderStatic)
function does the HTML rendering.
However to use it we need to 
jump trough some oddly-shaped,
[MTL](http://hackage.haskell.org/package/mtl) flavoured,
hoops[^hoops].
Doing this is not much harder than writing a regular reflex app.
Jumping trough these solves handling
browser native primitives,
such as XHR calls.
This is done
with help of [prerender](https://hackage.haskell.org/package/reflex-dom-core-0.5/docs/Reflex-Dom-Prerender.html#v:prerender).
Which is also the key to displaying authenticated content fast,
but we have to rewire the the initial app state to do that.
Fortunately this rewiring caused the initial state management to become more elegant
then it was in the
[authentication](https://jappieklooster.nl/authentication-in-reflex-servant.html)
post.


The source code is available in the 
[reference project](https://github.com/jappeace/awesome-project-name/tree/prerender).

[^hoops]: I figured out all this stuff on [stream](https://www.youtube.com/channel/UCQxmXSQEYyCeBC6urMWRPVw?view_as=subscriber)

# Strange MTL Hoops
We need to get rid of 
[WidgetMonad](https://hackage.haskell.org/package/reflex-dom-core-0.5/docs/src/Reflex.Dom.Old.html#MonadWidgetConstraints)
and replace it with the underlying builders.
We do this because the WidgetMonad has a nasty equality constraint
on GhcjsDomSpace that prevents the use of `renderStatic`:

```haskell
type MonadWidgetConstraints t m =
  ( DomBuilder t m
  , DomBuilderSpace m ~ GhcjsDomSpace -- <-- nasty constraint
  ...
  )

class MonadWidgetConstraints t m => MonadWidget t m
instance MonadWidgetConstraints t m => MonadWidget t m
```

A good strategy is replacing it with 
[DomBuilder](http://hackage.haskell.org/package/reflex-dom-core-0.5/docs/Reflex-Dom-Builder-Class.html#t:DomBuilder)
and add more constraints based on compiler requests.
MTL monads push outwards. That is to say,
the low level widgets determine the
constraints of the higher level widgets.

Unfortunately this is not the only place for that equality constraint.
The input and textarea widgets indirectly cause this constraint
too because of their return types.
For example in input:

```haskell
data TextInput t
   = TextInput { 
		... 
		, _textInput_builderElement 
		:: InputElement EventResult GhcjsDomSpace t  -- <-- nasty fundep
		}
```

There is no direct equality constraint,
but it gets asserted anyway because of
[functional dependencies](https://wiki.haskell.org/Functional_dependencies).
However I made an alternative to both these widgets in the Bulmex library.
In which I just
copied the [original](https://hackage.haskell.org/package/reflex-dom-core-0.5/docs/Reflex-Dom-Widget-Input.html)
and removed the constraining records from the result types.
They can be found
[here](https://hackage.haskell.org/package/bulmex-2.0.0/docs/Reflex-Bulmex-Input-Polymorphic.html).

# Prerender Parts

As mentioned before,
we intent to share most code between the client and the server.
In case of the client we use a JavaScript executable entry point
and get a GHCJS DOM environment.
In case of the server we use a servant endpoint as entry point and get a static
DOM builder environment.
When the GHCJS environment is accessed reflex needs to know what to do in the other environment and vice versa.
In the browser for example we’ll happily do XHR calls,
but when doing server side rendering we can simply ignore the request[^prerender].
This use case between environments is handled by [prerender](https://hackage.haskell.org/package/reflex-dom-core-0.5/docs/Reflex-Dom-Prerender.html):

```haskell
prerender :: m a
            -> (PrerenderClientConstraint js t (Client m) => Client m a)
            -> m (Dynamic t a) 
```

Depending on the entry point we use we get a different monad environment.
On the server we get a StaticDomBuilder environment (`m a`),
and in the JavaScript executable frontend we get a GHCJS DOM environment
(`Client m a`).
The first monad is the one being displayed on the server,
the second one has access to the GHCJS DOM environment.
The resulting value will be captured in a dynamic and can be used by the
shared code.

Now we can enhance all servant-reflex calls with prerender calls,
to make it do nothing in the static environment and do the XHR calls on the GHCJS dom environment.
For example the `postLogin` function from the [authentication blogpost](https://jappieklooster.nl/authentication-in-reflex-servant.html)
now looks like:

```haskell
postLogin :: (DomBuilder t m, Prerender js t m)
          => Dynamic t (Either Text.Text User)
          -> Event t ()
          -> m (Event t (ReqResult () (AuthCookies NoContent)))
postLogin dd yy = fmap switchDyn $ prerender (pure never) $ postLogin' dd yy
```

Here `postLogin'` is the generated endpoint from [servant-reflex](https://hackage.haskell.org/package/servant-reflex).
In case of a server StaticDomBuilder environment we do nothing never
`(pure never)`[^side],
in case of a GHCJS DOM environment we do the `postLogin'` function.
The fmap switchDyn part is to get rid of the dynamic,
which is unnecessary because we already return an event.
Similar code can be used for all generated endpoints[^points].

[^side]: I could imagine an alternative design:
	For example with a micro service setup where you just let the app decide what to call.
	You’d still need to use a different client on the server side though because it’s native code.
	But this can be done for with [servant-client](http://hackage.haskell.org/package/servant-client).
	You define your micro services around the 'servant' based REST contracts and let the app pull it in.

[^points]: It may also be possible to make a more generic function to do this,
		but I didn’t want to spend the time
		(I’ve been working on this for days).

# Backend servant endpoint
We need to add an endpoint to servant which will serve the
statically rendered HTML.
To add it to servant we make an authenticated entry point, which returns
[HTML](http://hackage.haskell.org/package/servant-fiat-content-1.0.0/docs/Servant-HTML-Fiat.html)
as a `ByteString`.
Because it's authenticated we can choose what to serve.
The authenticated app, if the user has the permission, or
the public site, if the user doesn't have that.
It looks like this:

```haskell
type Webservice = ...
      :<|> Auth '[Cookie, JWT] User :> (Get '[HTML] BS.ByteString)
      ...
```

In the previous blogpost we'd simply return a '401 unauthorized' status code.
Servant auth allows us to decide how to respond.
Which we can see in the implementation:

```haskell
renderHtmlEndpoint :: HeadSettings -> AuthResult User -> Handler BS.ByteString
renderHtmlEndpoint settings authRes = do
  fmap snd $ liftIO $ renderStatic $
    htmlWidget settings $ main $ IniState $ toMaybe authRes
```

The `main` function is the entry point for the reflex code.
We wrap it around in [`htmlWidget`](https://hackage.haskell.org/package/bulmex-2.0.0/docs/Reflex-Bulmex-Html.html#v:htmlWidget),
which has [`HeadSettings`](https://hackage.haskell.org/package/bulmex-2.0.0/docs/Reflex-Bulmex-Html.html#t:HeadSettings)
to load the JavaScript client as a simple [`URI`](https://hackage.haskell.org/package/network-uri-2.6.1.0/docs/Network-URI.html#t:URI).
The decision of what to show is completely pushed into the reflex code,
the only thing we do at server side is give it the `IniState`.
Next we'll discuss how the reflex app decides what to show in this environment.

# Reflex App
So now we have an `IniState` for the app,
we can read or write it to the DOM depending on environment.
I wrote a function for that.
On the app side this is done by [`writeReadDom`](https://hackage.haskell.org/package/bulmex-2.0.0/docs/Reflex-Bulmex-Html.html#v:writeReadDom):

```haskell
writeReadDom ::
    ( FromJSON a
    , ToJSON a
    , Dom.DomBuilder t m
    , Dom.Prerender js t m
    ) =>
    Text.Text -> a -> m (Dynamic t a)
```

The first value is the HTML id,
the second one is the app state as received by it’s respective endpoint.
The result behaves roughly like this:
```html
<!-- Static dom builder environment (server) --> 
	<script id="Text.Text" type="application/json">
		<?php encode a ?>
	</script>
<!-- GHCJS environment (browser) -->
	JSON.parse(document.getElementById("Text.Text"));
```
Of course the JavaScript will look completely different
and the result is put in a dynamic.
But the resulting behavior is the same, the server encodes the JSON
and the client will decodes it giving us the app state from the server
without additional requests.
Under the hood `prerender` figures out if we need to read or write.
All the client needs to do is provide said value,
in case of the [example app](https://github.com/jappeace/awesome-project-name/tree/prerender)
this is just a `Maybe User`.
The decision of what to display is all integrated within
the same reflex code like this:

```haskell
main :: (AWidget js t m) => IniState -> m ()
main iniState = do
  iniDyn <- writeReadDom "iniState" iniState
  rec loginEvt <- elDynAttr "div" loginAttr $ loginWidget iniDyn
      loginAttr <- holdDyn (Map.empty) $ hidden <$ loginEvt
  void $ holdEvent () loginEvt authenticatedWidget

authenticatedWidget :: (AWidget js t m) => User -> m ()
authenticatedWidget = ...

```

We always get an `IniState`,
but we make sure to first put it in `writeReadDom`.
This ensures that the `IniState` is the actual `IniState` from
the server and not the one we hardcoded
in the executable to make the compiler happy[^nohappy].
This `iniDyn` then gets used for the `loginWidget` which returns a
`loginEvt`.
If the event fires it has a user,
with which we can display the `authenticatedWidget`.
This is done with help of `holdEvent`.
Note that if no event is received, no authentication is done.
`loginAttr` will however become hidden as soon as there is a `loginEvt`,
hiding the login form.
The login widget itself looks like this now:

[^nohappy]: I don't know of a type safe way around this inital state problem.
          Note that the JavaScript loading by itself is also not type safe,
		  a `URI` is used.
		  With that in mind we can kind off consider this the 'edge'
		  of the system.
	

```haskell

loginWidget :: (AWidget js t m) => Dynamic t IniState -> m (Event t User)
loginWidget iniDyn = do
  pb <- getPostBuild
  formEvt <- loginForm
  pure $ leftmost [formEvt,
       noNothing $ updated $ userDyn,
       noNothing $ current userDyn <@ pb]
  where
    userDyn = unpackUser <$> iniDyn
```
Compared to the [previous blogpost](https://jappieklooster.nl/authentication-in-reflex-servant.html)
we got rid of `autoLogin` and use the `IniState` instead.
The post build event is used to sample the current value of `userDyn`.
This ensures a user event fires if there is one available.
That mechanism also works for static rendering.

# Conclusion
Loading times was one of reflex major weaknesses and this blog post discussed how to
mostly remove it.
It still takes time before the app becomes interactive,
but at least now useful information can be shown to the user instead
of a loading screen.
I'm really happy with the end result, and I think anyone with a reflex app should
seriously consider doing this.

# References

+ [Reference project github](https://github.com/jappeace/awesome-project-name/tree/prerender)
+ [Official reflex prerender docs](http://docs.reflex-frp.org/en/latest/reflex_dom_docs.html#prerendering-server-side-rendering)
+ [Prerender on hackage](https://hackage.haskell.org/package/reflex-dom-core-0.5/docs/Reflex-Dom-Prerender.html)
+ [Render static on hackage](https://hackage.haskell.org/package/reflex-dom-core-0.5/docs/Reflex-Dom-Builder-Static.html#v:renderStatic)

[^prerender]: Special thanks to lumie for pointing this out.
