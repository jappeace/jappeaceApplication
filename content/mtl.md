Title: A brief intro to MTL
Date: 2021-08-07 15:44
Category: technique
OPTIONS: toc:nil
Tags: haskell, programming, mtl
subreddit: haskell programming reflexfrp
Status: draft

Recently a blog post came out how to use the concrete
base [transformers](https://blog.cofree.coffee/2021-08-05-a-brief-intro-to-monad-transformers/).
I like the way it's written,
it's very thorough and brings back the idea of
transformers to a concrete example which I really like.
Although to me it looks like quite a low level approach.
I strongly feel you'll get more out of transformers by
opting into full MTL.

However I don't think I can find a terse description on how to
use MTL.
I learned this by staring at a lot of reflex code for days until it clicked.
So if `/u/zgkzy` would allow me,
I'll give a brief overview of doing this MTL style instead of
transformer style[^mtl-vs-transformers].
I'm not claiming this is a new idea or you should even adopt this style.
However I tend to structure my programs this way,
and I feel there is surprising little prose on this topic.
I will write down how it works by using words so people can read
instead of having to struggle with this for days.

Note that I also presented this more detailed in a
[video](https://www.youtube.com/watch?v=MPlrAe-XYMU&t=300s).

## <a id="intro"></a> The type variable `m`

We start start by introducing `m`,
`m` could've been named `monad` or `x`, 
but the community settled on using `m` for type variables
with the `Monad` constraint, so I will use this too.
Instead of having our type variable be
inside a concrete type, such as `Maybe a` or `[a]`,
we flip it around:

```haskell
moreMonad :: Monad m => m Int
moreMonad = return 5
```

This compiles because the `Monad` constraint on `m`
gives us the [`return`](https://hackage.haskell.org/package/base-4.15.0.0/docs/Control-Monad.html#v:return)
function.
What can we do with this `moreMonad` binding?
Well, we can pattern match on it:
```haskell
fiveTroughMaybe :: Int
fiveTroughMaybe = case moreMonad of
  Just x -> x
  Nothing -> 9
```
At this particular call site, GHC will give
`moreMonad` the type `Maybe`,
because it's the only possible solution.
GHC reasons backwards from pattern match up to the `case moreMonad of`
definition to figure this out.
It'll result in 5 because `return` is implemented as `Just` on maybe.
In the same module we can also pattern match on Either:

```haskell
fiveTroughEither :: Int
fiveTroughEither = case moreMonad of
  Right x -> x
  Left _y -> 9
```

Both `fiveTroughMaybe` and `fiveTroughEither` will result in `5`.
This is allowed in the same module because `moreMonad`
will only get assigned the type at the [call site](https://en.wikipedia.org/wiki/Call_site).
The compiler figures out the type of `moreMonad` by looking at how it's used per
call site.
This backwards 'figuring out', is normal for type variables.
If it fails you get a message like `Ambiguous type variable ‘m0’`,
which we'll see happen later on in this [blogpost](#shes-a-beauty).

[^mtl-vs-transformers]: Aren't transformers the same as mtl? No!
                        Back in the stoneages, people weren't sure what the right
                        approach would be to doing this and frankly still aren't.
                        The base is the [transformers package](https://hackage.haskell.org/package/transformers).
                        Although the [mtl package](https://hackage.haskell.org/package/mtl)
                        is most popular, there is also [mtl-tf](https://hackage.haskell.org/package/mtl-tf).
                        I'd like to point out as well that in technology, popularity means
                        nothing. Cobol used to be more popular then C up till the 1990's,
                        and is practically dead now.
                        JavaScript is also more popular then haskell, is it therefore better?

### Thought food

+ Can we always pattern match on every possible monad type for like we do with `Just` or `Either`? [^no-pattern-match]
+ 

[^no-pattern-match]: No it's not possible if a constructor isn't exposed. Reflex uses this with `dynamic t` to create a smart destructor.
## Transformers as constraints on `m`

With that brief introduction,
we can start applying this idea to the [blogpost](https://blog.cofree.coffee/2021-08-05-a-brief-intro-to-monad-transformers/).
A newtype is constructed to hold the entire stack like this:

```haskell
newtype AppM a = AppM { runAppM :: ExceptT String (State (M.Map VariableName Int)) a }
  deriving newtype (Functor, Applicative, Monad, MonadError String, MonadState (M.Map VariableName Int))
```

I call this `AppM` a concrete type because there is only one way to pattern match on it.
We're not allowed to pretend it's a `Maybe` for example.
This definition is used in the `assignIndexToVariables`:

```haskell
assignIndexToVariables :: AST VariableName -> Variables -> AppM (AST Int)
```

Instead of using the concrete type `AppM`,
we could use MTL type classes to describe what is needed.
These type classes will become constraints on `m`,
similarly to how `Monad` was a constraint on `m` in the [introduction](#intro).
That is to say, we want to have [`MonadError String`](https://hackage.haskell.org/package/mtl-2.2.2/docs/Control-Monad-Except.html#t:MonadError)
as replacement for `ExceptT String`,
and [`MonadState (M.Map VariableName Int)`](https://hackage.haskell.org/package/mtl-2.2.2/docs/Control-Monad-State-Lazy.html#t:MonadState)
as replacement for `State (M.Map VariableName Int)`.
This will change the type signature of `assignIndexToVariables` as follows:

```haskell
assignIndexToVariables ::
               MonadError String m
            => MonadState (M.Map VariableName Int) m
            => AST VariableName
            -> Variables
            -> m (AST Int)
```

So what's the difference?
The difference here is in running the code.
Both invocations are allowed now:

```haskell
main :: IO ()
main = do
 ...
 print $ flip evalState mempty $ runExceptT $ assignIndexToVariables ast vars
 print $ runExcept $ flip evalStateT mempty $ assignIndexToVariables ast vars
```

In effect we've told the compiler that at our function definition the order
of a monad stack doesn't matter.
Which makes sense because consider two monad stacks:
```haskell
a :: ExceptT String (State (M.Map VariableName Int)) a
b :: StateT (M.Map VariableName Int) (Except String) a
```
These describe the same capabilities.
In the transformer style, yes the compiler would say `a` and `b` should *not* compose.
Whereas an MTL style function definition would compose because no order has been specificied.

## On composition and constraints

Let's say `assignIndexToVariables` would need `moreMonad` for some reason:

```haskell
assignIndexToVariables ::
    MonadError String m =>
    MonadState (M.Map VariableName Int) m =>
    AST VariableName -> Variables -> m (AST Int)
assignIndexToVariables _ _ = do
   _z <- moreMonad 
   ...
```

This would just work, because both [`MonadError`](https://hackage.haskell.org/package/mtl-2.2.2/docs/Control-Monad-Except.html) and
[`MonadState`](https://hackage.haskell.org/package/mtl-2.2.2/docs/Control-Monad-State-Class.html#t:MonadState) imply `Monad` in their definitions.
Now let's add a [MacGyver](https://en.wikipedia.org/wiki/MacGyver) [^macgyver]
logging function as follows:

[^macgyver]: MacGyver is a TV character who was known to get out of
                precarious situations with absurdly little in terms of
                resources and a lot of improvisation, our logging function feels similar at this point.
                Another term that could've been used was Not Invented Here,
                but I liked MacGyver better.

```haskell
macGyverLog :: MonadIO m => String -> m ()
macGyverLog msg = liftIO $ putStrLn msg
```

The code is perfect. 
Y'know it may miss some things you'd expect from your regular
logging library such as code positions, time stamping, etc.
But that's why it's called `macGyverLog` and not `kitchenSinkLog`.
And if we did some introspection we may find our code
tends to look a lot more like `macGyverLog` then `kitchenSinkLog`,
so let's throw it into production:

```haskell
assignIndexToVariables ::
    MonadError String m =>
    MonadState (M.Map VariableName Int) m =>
    AST VariableName -> Variables -> m (AST Int)
assignIndexToVariables _ _ = do
   macGyverLog "Starting reading more monad"
   _z <- moreMonad 
   macGyverLog "End reading more monad"
   ...
```

This will cause the following type error:


```
src/Lib.hs:31:5: error:
    • Could not deduce (MonadIO m) arising from a use of ‘macGyverLog’
      from the context: (MonadError String m,
                         MonadState (M.Map VariableName Int) m)
        bound by the type signature for:
                   assignIndexToVariables :: forall (m :: * -> *).
                                             (MonadError String m,
                                              MonadState (M.Map VariableName Int) m) =>
                                             AST VariableName -> Variables -> m (AST Int)
        at src/Lib.hs:(26,1)-(29,46)
      Possible fix:
        add (MonadIO m) to the context of
          the type signature for:
            assignIndexToVariables :: forall (m :: * -> *).
                                      (MonadError String m,
                                       MonadState (M.Map VariableName Int) m) =>
                                      AST VariableName -> Variables -> m (AST Int)

```
The possible fix in this case is correct,
but we sort off have to know what a context is,
and a type signature to decipher that error message[^why-dont-they].
The compiler is saying in incomprohensible error speak that you need to
add a constraint `MonadIO m` like so:

```
assignIndexToVariables ::
    MonadIO m =>
    MonadError String m =>
    MonadState (M.Map VariableName Int) m =>
    AST VariableName -> Variables -> m (AST Int)
```

Now the reason I'm writing about this isn't that particular error message,
cryptic as it maybe, it's the next one:
```
src/Lib.hs:51:50: error:
    • No instance for (MonadIO Data.Functor.Identity.Identity)
        arising from a use of ‘assignIndexToVariables’
    • In the second argument of ‘($)’, namely
        ‘assignIndexToVariables ast vars’
      In the second argument of ‘($)’, namely
        ‘runExceptT $ assignIndexToVariables ast vars’
      In the second argument of ‘($)’, namely
        ‘flip evalState mempty
           $ runExceptT $ assignIndexToVariables ast vars’
   |
51 |     print $ flip evalState mempty $ runExceptT $ assignIndexToVariables ast vars
   |                                                  ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
```
Here you may start thinking, WTF. Rightfully so.
The reason it starts talking about identity is because evalState
*runs in identity*.
Look at the haddocks for [`evalState`](https://hackage.haskell.org/package/transformers-0.6.0.2/docs/Control-Monad-Trans-State-Lazy.html#v:evalState), then click on [`State`](https://hackage.haskell.org/package/transformers-0.6.0.2/docs/Control-Monad-Trans-State-Lazy.html#t:State).
The base monad is identity.
How do we fix this?
First we replace `Identity` with a gap by invoking `evalStateT`.
This will result in the following beauty <a id="shes-a-beauty"></a>:
```
src/Lib.hs:51:5: error:
    • Ambiguous type variable ‘m0’ arising from a use of ‘print’
      prevents the constraint ‘(Show
                                  (m0 (Either String (AST Int))))’ from being solved.
      Probable fix: use a type annotation to specify what ‘m0’ should be.
      These potential instances exist:
        instance (Show a, Show b) => Show (Either a b)
          -- Defined in ‘Data.Either’
        instance (Show k, Show a) => Show (M.Map k a)
          -- Defined in ‘Data.Map.Internal’
        instance Show a => Show (AST a) -- Defined at src/Lib.hs:21:13
        ...plus 18 others
        ...plus 9 instances involving out-of-scope types
        (use -fprint-potential-instances to see them all)
    • In a stmt of a 'do' block:
        print
          $ flip evalStateT mempty
              $ runExceptT $ assignIndexToVariables ast vars
      In the expression:
        do print
             $ flip evalStateT mempty
                 $ runExceptT $ assignIndexToVariables ast vars
           print (eitherFive, maybeFive)
      In the expression:
        let
          vars = S.fromList [...]
          ast
            = Node (Leaf "a") (Node (Leaf "b") (Node (Leaf "a") (Leaf "c")))
        in
          do print
               $ flip evalStateT mempty
                   $ runExceptT $ assignIndexToVariables ast vars
             print (eitherFive, maybeFive)
   |
51 |     print $ flip evalStateT mempty $ runExceptT $ assignIndexToVariables ast vars
   |     ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

src/Lib.hs:51:18: error:
    • Ambiguous type variable ‘m0’ arising from a use of ‘evalStateT’
      prevents the constraint ‘(Monad m0)’ from being solved.
      Probable fix: use a type annotation to specify what ‘m0’ should be.
      These potential instances exist:
        instance Monad (Either e) -- Defined in ‘Data.Either’
        instance Monad IO -- Defined in ‘GHC.Base’
        instance [safe] Monad m => Monad (ExceptT e m)
          -- Defined in ‘Control.Monad.Trans.Except’
        ...plus six others
        ...plus 16 instances involving out-of-scope types
        (use -fprint-potential-instances to see them all)
    • In the first argument of ‘flip’, namely ‘evalStateT’
      In the expression: flip evalStateT mempty
      In the second argument of ‘($)’, namely
        ‘flip evalStateT mempty
           $ runExceptT $ assignIndexToVariables ast vars’
   |
51 |     print $ flip evalStateT mempty $ runExceptT $ assignIndexToVariables ast vars
   |                  ^^^^^^^^^^

src/Lib.hs:51:51: error:
    • Ambiguous type variable ‘m0’ arising from a use of ‘assignIndexToVariables’
      prevents the constraint ‘(MonadIO m0)’ from being solved.
      Probable fix: use a type annotation to specify what ‘m0’ should be.
      These potential instances exist:
        instance [safe] MonadIO IO -- Defined in ‘Control.Monad.IO.Class’
        instance [safe] MonadIO m => MonadIO (ExceptT e m)
          -- Defined in ‘Control.Monad.Trans.Except’
        instance [safe] MonadIO m => MonadIO (StateT s m)
          -- Defined in ‘Control.Monad.Trans.State.Lazy’
        ...plus 11 instances involving out-of-scope types
        (use -fprint-potential-instances to see them all)
    • In the second argument of ‘($)’, namely
        ‘assignIndexToVariables ast vars’
      In the second argument of ‘($)’, namely
        ‘runExceptT $ assignIndexToVariables ast vars’
      In the second argument of ‘($)’, namely
        ‘flip evalStateT mempty
           $ runExceptT $ assignIndexToVariables ast vars’
   |
51 |     print $ flip evalStateT mempty $ runExceptT $ assignIndexToVariables ast vars
   |                                                   ^^^^^^^^^^^^^^^^^^^
```

> Oh ghci, I love it when you talk to me this way.

All these `m0` occurences are because we introduced the gap,
it has no idea what the base monad is at this point.
Which is what we want.
Let's solve it all in one move:

```haskell
    print =<< flip evalStateT mempty (runExceptT $ assignIndexToVariables ast vars)
```

The base monad is now IO instead of identity,
which solves everything.
hlint told me to write it like this which makes it a bit more readable

```haskell
    print =<< evalStateT (runExceptT $ assignIndexToVariables ast vars) mempty
```

If you like you can get the generated transformer stack out with a type hole

```
    let x :: _
        x = assignIndexToVariables ast vars
    print =<< evalStateT (runExceptT x) mempty
```

which results into an error
```
src/Lib.hs:51:14: error:
    • Found type wildcard ‘_’
        standing for ‘ExceptT
                        String (StateT (M.Map VariableName Int) IO) (AST Int)’
      To use the inferred type, enable PartialTypeSignatures
```
You can copy paste that type signature in your code to get a functioning program
back out.
No idea why you'd do that, no-one cares about transformer details.

[^why-dont-they]: The thing that baffles me is that they're already managing to print your own
            code, they figure out the right constraint,
            why don't they just give the example like I'm doing
            here with the message "try doing this:".


## Reinterpreting IO or the MTL style

I don't do this particular part on a regular basis.
In most situations `MonadIO` is fine.
However it doesn't allow us to reinterpret effects that are
using `IO`.
As an example of reinterpretation,
consider a test you where would want to meaure how often your
`mcGyverLog` function is being called.
This section will show you how to do that.

First we start by mtl-izing our IO based code.
How can we rewrite `mcGyverLog` so that it doesn't use `IO` explicetly?
If we look at the source the function that needs IO is `putStrLn`.
It's type signature is `String -> IO ()`.
We want that `IO` to be an `m`, 
so we introduce a new typeclass for our [not invented here](https://hackage.haskell.org/package/monad-logger-0.3.36/docs/Control-Monad-Logger.html#t:MonadLogger)
log:

```haskell
class (Monad m) => NotInventedHereLog m where
    nihLog :: String -> m ()
```

We already what implementation `m` has if it's an
`IO` instance:

```haskell
instance NotInventedHereLog IO where
    nihLog :: String -> IO ()
    nihLog = putStrLn
```

To make our previous example work we need to replace our
function definition with `MonadIO` to `NotInventedHereLog` and replace the `macGyverLog`
calls with `nihLog`:

```haskell
assignIndexToVariables2 ::
  NotInventedHereLog m =>
  MonadError String m =>
  MonadState (M.Map VariableName Int) m =>
  AST VariableName -> Variables -> m (AST Int)
assignIndexToVariables2 ast variables = forM ast $ \var -> do
    nihLog "start more monad"
    _z <- moreMonad
    ...
```

Running this will give us the following type error:
```
src/Lib.hs:54:52: error:
    • No instance for (NotInventedHereLog
                         (StateT (M.Map VariableName Int) (ExceptT [Char] IO)))
        arising from a use of ‘assignIndexToVariables2’
    • In the second argument of ‘($)’, namely
        ‘assignIndexToVariables2 ast vars’
      In the first argument of ‘runExceptT’, namely
        ‘(flip evalStateT mempty $ assignIndexToVariables2 ast vars)’
      In the second argument of ‘(=<<)’, namely
        ‘runExceptT
           (flip evalStateT mempty $ assignIndexToVariables2 ast vars)’
   |
54 |     print =<< runExceptT (flip evalStateT mempty $ assignIndexToVariables2 ast vars)
```
This looks like scary, maybe this is even the one type error I can't
solve?
After all, I've been waiting on that one for over 4 years now, still have managed to solve them all, somehow.
But no,
this is known as the [n^2-instances problem](https://lexi-lambda.github.io/blog/2017/04/28/lifts-for-free-making-mtl-typeclasses-derivable/),
which sounds impressive, but the solution is deceptively simple:
```haskell
instance (NotInventedHereLog m) => NotInventedHereLog (StateT s m) where
  nihLog = lift . nihLog
```
This code says:
If you're a StateT and your base monad is already a `NotInventedHereLog`,
you are also a `NotInvnetedHereLog` by using lift.
By providing this instance we're generating lift calls over any StateT
for all occurences of niLog.


Moving on we get the same error for ExceptT:
```
src/Lib.hs:54:52: error:
    • No instance for (NotInventedHereLog (ExceptT [Char] IO))
        arising from a use of ‘assignIndexToVariables2’
    • In the second argument of ‘($)’, namely
        ‘assignIndexToVariables2 ast vars’
      In the first argument of ‘runExceptT’, namely
        ‘(flip evalStateT mempty $ assignIndexToVariables2 ast vars)’
      In the second argument of ‘(=<<)’, namely
        ‘runExceptT
           (flip evalStateT mempty $ assignIndexToVariables2 ast vars)’
   |
54 |     print =<< runExceptT (flip evalStateT mempty $ assignIndexToVariables2 ast vars)

```
The solution is pretty much the same:
```
instance (NotInventedHereLog m) => NotInventedHereLog (ExceptT e m) where
  nihLog = lift . nihLog
```

With these two additional instances the example code compiles again.
However we still don't know how to reinterpret this purely,
and have introduced code duplication.
This duplication can be removed with the default mechanism described in
alexis king her [blogpost](https://lexi-lambda.github.io/blog/2017/04/28/lifts-for-free-making-mtl-typeclasses-derivable/).

We need to introduce a newtype to attach an instance for NotInventedHereLog on.
This type will collect the logged messages,
turns out that the WriterT monad does exactly what we want.
So the pure code will look like this:

```
newtype NihLogT m a = MkNihLogT {
        runNihLog :: WriterT [String] m a
    } deriving (Functor, Applicative, Monad, MonadTrans, MonadWriter [String])

instance Monad m => NotInventedHereLog (NihLogT m) where
  nihLog msg = tell [msg]
```

The instance simply tells the message, which `mappends` it to the list.
To run this code we use `runNihLog`:
```haskell
    let pureCode :: (Either String (AST Int),  [String])
        pureCode = runWriter $ runNihLog $ runExceptT (flip evalStateT mempty $ assignIndexToVariables2 ast vars)
    print pureCode
```
With this you can write property tests on `assignIndexToVariables2`.
For example you could assert that an AST of size 20 should at least emit 40 log messages.
Obviously this isn't limited to tests,
you can also add a newtype that has a connection pool to send
of the messages to some database for example.

I'll tap out here.
This was supposed to be a short addendum on someone else their blogpost.
I dumped all my knowledge at this point and extended it a bit on several
points (like defaults mechanism from alexes king)

## Links

+ A functional example is available [here](https://github.com/jappeace/mtl-src/blob/master/src/Lib.hs)
+ Original [blogpost](https://blog.cofree.coffee/2021-08-05-a-brief-intro-to-monad-transformers/)
+ OH WOOT https://github.com/lexi-lambda/mtl-style-example
+ [presentation](https://www.youtube.com/watch?v=MPlrAe-XYMU&t=300s)
