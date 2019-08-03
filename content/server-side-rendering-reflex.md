Title: Reflex server side html rendering
Date: 2019-08-04 15:25
Category: tools
OPTIONS: toc:nil
Tags: haskell, programming, tools, servant, tutorial
subreddit: haskell programming reflexfrp
status: draft
Reflex is a single page app framework written in Haskell compiled to Javascript.
But it’s also capable of doing server side rendering in html.
This makes load times very fast[^brag] and allows the browser to display the app very quickly. 

[^brag]: In less than 0.5 seconds, including db lookups, on your phone

![Bob doing SSR](/images/2019/bob-busy.jpeg)

JavaScript developers would call this technique [‘isomporhic’](https://medium.com/capital-one-tech/why-everyone-is-talking-about-isomorphic-universal-javascript-and-why-it-matters-38c07c87905)[^category]
Javascript.
This blogpost will summerize my own experience and tell you how to do it with reflex.
I found it surpsingly easy.
We can share most of our reflex code to both spit out a static html variant,
as well as to create a javascript client from that.
We just need to tell reflex what to do with browser native primitives,
such as XHR calls.

[^category]:So it’s not isomorpism categorical terms, but javascript terms.
To begin with,
we have to jump trough some oddly-shaped,
mtl flavoured, hoops[^hoops].
Just to make renderStatic work for most code.
But it’s not much harder than writing a regular reflex app.
Then we need to deal with exceptions,
with help of [prerender](https://hackage.haskell.org/package/reflex-dom-core-0.5/docs/Reflex-Dom-Prerender.html#v:prerender).
In my case, the state management will be more elgant than the
[authentication](https://jappieklooster.nl/authentication-in-reflex-servant.html) post.

[^hoops]: I figured out all this stuff on [stream](https://www.youtube.com/channel/UCQxmXSQEYyCeBC6urMWRPVw?view_as=subscriber)

# Strange MTL Hoops
The first thing to do is get rid of the
[WidgetMonad](https://hackage.haskell.org/package/reflex-dom-core-0.5/docs/src/Reflex.Dom.Old.html#MonadWidgetConstraints)
everywhere in your app and replace them with the respective buidlers.
Start for example with DomBuilder and add more based on compiler requests.
The reason for that is that the WidgetMonad has a nasty equality constraint
on GhcjsDomSpace that prevents the use of staticRender:

```haskell
type MonadWidgetConstraints t m =
  ( DomBuilder t m
  , DomBuilderSpace m ~ GhcjsDomSpace -- <-- nasty constraint
  ...
  )

class MonadWidgetConstraints t m => MonadWidget t m
instance MonadWidgetConstraints t m => MonadWidget t m
```

In my own app I just made a new type allias for top level elements without
that particular equality constraint called `AWidget`.
That same constraint shows up in input and textarea because the return
values expose to much information.
For example in input:

```haskell
data TextInput t
   = TextInput { 
               ... 
               , _textInput_builderElement 
                :: InputElement EventResult GhcjsDomSpace t  -- <-- nasty fundep
               }
```

It doesn’t do the equality directly,
but get’s asserted anyway because of functional dependencies
(although it’s pretty deep in the reflex library).
They can be found
[here](https://github.com/jappeace/bulmex/blob/master/bulmex/src/Reflex/Bulmex/Input/Polymorphic.hs#L26).

# Prerender Parts

As mentioned before,
we intent to share most code between client and server.
In case of the client we use a javascript exeuctable entrypoint and get a GHCJSDOM environment.
In case of the server we get a static dom builder environment and use a servant endpoint as entrypoint.
In certain situations we must do something different depending on environment.
For example I didn’t want XHR request to happen on the server side[^side].

[^side]: I could imagine cases where people do want this to happen,
	for example with a microservice setup where you just let the app decide what to call.
	You’d still need to use a different client on the server side though because it’s native code.

We need to tell reflex what to do when we actually need to access the GHCJS environment. In the browser for expample we’ll hapily do XHR calls,
but when doing server side rendering we can simply ignore the request.
This is what prerender does[^prerender]:

```haskell
prerender :: m a
            -> (PrerenderClientConstraint js t (Client m) => Client m a)
            -> m (Dynamic t a) 
```

Depending on the entry we use we get a different monad environment.
On the server we get a StaticDomBuilder environment (`m a`),
and in the javascript executable frontend we get a GHCJS dom environemnt
(`Client m a`).

The first monad is the one being displayed on the server,
the second one has access to the GHCJS environment.
The result will be captured in a dynamic and can be used by the shared code.

Now we can replace all servant-reflex calls with prerender calls,
to make it do nothing in the static environment and do the ajax calls on the GHCJS dom enviroment.
For example the `postLogin` function from the [authentication blogpost](https://jappieklooster.nl/authentication-in-reflex-servant.html):

```haskell
postLogin :: (DomBuilder t m, Prerender js t m)
          => Dynamic t (Either Text.Text User)
          -> Event t ()
          -> m (Event t (ReqResult () (AuthCookies NoContent)))
postLogin dd yy = fmap switchDyn $ prerender (pure never) $ postLogin' dd yy
```

Here `postLogin'` is the generated endpoint from servant.
In case of server side rendering we do nothing never `(pure never)`,
in case of a browser context we do the `postLogin'` function.
The fmap switchDyn part is to get rid of the dynamic, which is unecisarry.
That code can be used for all endpoints[^points].

[^points]: It may also be possible to make a more generic function to do this,
		but I didn’t want to spend the time
		(I’ve been working on this for days).
		I’ve figure out all these steps by staring at hackage and a bit of help from chat.


# Backend & Initial app state

With all this in place we can do proper loading of the initial app state.
We can read it as a json blob from the document itself, the server ‘knows’ as an endpoint what to write there.
On the app side this is done by [`writeReadDom`](https://github.com/jappeace/bulmex/blob/a4b1bf1550d1fbddbdd131c619fd012cb93f2f2d/bulmex/src/Reflex/Bulmex/Load.hs#L27):


```haskell
writeReadDom ::
    ( FromJSON a
    , ToJSON a
    , Dom.DomBuilder t m
    , Dom.Prerender js t m
    ) =>
    Text.Text -> a -> m (Dynamic t a)
```

The first value is the unique id,
the second one is the appstate as received by it’s respective endpoint.
Under the hood prerender figures out if we need to write or read.
All the client needs to do is provide said value,
which in case of the example app is just login inforamtion:

```haskell
newtype IniState =
  IniState (Maybe User)
```

To add it to servant we make an authenticated entry point, which returns
[HTML](http://hackage.haskell.org/package/servant-fiat-content-1.0.0/docs/Servant-HTML-Fiat.html)
as a bytestring:

```haskell
type Webservice = ServiceAPI -- servant reflex rest api
      :<|> Auth '[Cookie, JWT] User :> (Get '[HTML] BS.ByteString)
      :<|> Raw -- Other stuff such as css
```

However if we don’t get authenticated we just show the public page instead of returning a 401,
which was usually the case before.
This then is also reflected in the IniState.
But if we do get authenticated we give th app a rich Inistate:

```haskell
renderHtmlEndpoint :: HeadSettings -> AuthResult User -> Handler BS.ByteString
renderHtmlEndpoint settings authRes = do
  fmap snd $ liftIO $ renderStatic $
    htmlWidget settings $ main $ IniState $ toMaybe authRes
```
The app itself is only modified by using writereaddom,
and the resulting dynamic app state.
For machine level precision I refer to the [source code](https://github.com/jappeace/awesome-project-name).

# Conclusion
Now reflex goes faster.
Loading times was one of reflex major weaknesses and this mostly removes it.
It still takes time before the app becomes interactive,
but at least now usefull information can be shown to the user instead
of a loading screen.


[^prerender]: Special thanks to lumie for pointing this out, saving me a bunch of time.
