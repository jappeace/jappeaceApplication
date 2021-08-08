Title: A brief intro to MTL
Date: 2021-08-07 15:44
Category: technique
OPTIONS: toc:nil
Tags: haskell, programming, mtl

Recently a blog post came out which I quite like,
it describes how to use the concrete
base [transformers](https://blog.cofree.coffee/2021-08-05-a-brief-intro-to-monad-transformers/).
It's very thorough and brings back the idea of
transformers to a concrete example.
Although it looks like quite low level.
I strongly feel you'll get more out of transformers by
opting into full MTL.

Furthermore I don't think I can find a terse description on how to
use MTL.
I feel there is surprising little prose on this topic.
I learned this myself by staring at [reflex](https://hackage.haskell.org/package/reflex)
code for days until it clicked.
Which isn't very ergonomic,
so I'll give a brief overview of doing this MTL style instead of
transformer style[^mtl-vs-transformers].
I will write down how it works with words, so people can read
rather then struggling with it for days like I had to do.
Obviously I'm not claiming this is a new idea or you should even adopt
this style for your own projects.
This isn't an endorsement, but a description.

If you like video presentations, I also presented on MTL in a
[video format](https://www.youtube.com/watch?v=MPlrAe-XYMU&t=300s).

## <a id="intro"></a> The type variable `m`

We start by introducing `m`.
Which could've been named `monad` or `x`,
but the community settled on using `m` for type variables
with the `Monad` constraint, so I will use this too.
Normally we use type variable's in concrete types,
for example `Maybe a` or `[a]`.
However, Instead of having our type variable 
inside a concrete type, we can flip it around:

```haskell
moreMonad :: Monad m => m Int
moreMonad = return 5
```

This compiles because the `Monad` constraint on `m`
gives us the [`return`](https://hackage.haskell.org/package/base-4.15.0.0/docs/Control-Monad.html#v:return)
function.
After you're convinced this is a valid definition,
let's use it.
What can we do with this `moreMonad` binding?
Well, we can pattern match on it:

```haskell
fiveTroughMaybe :: Int
fiveTroughMaybe = case moreMonad of
  Just x -> x
  Nothing -> 9
```

GHC will give
`moreMonad` the type `Maybe`
at this particular call site,
because it's the only possible solution.
GHC reasons backwards from the pattern match up to the `case moreMonad of`
definition to figure out the type.
The way I describe this backwards reasoning in my head is:
"pretending you have a `Maybe` which makes it becomes true".
So `fiveTroughMaybe `results in 5 because `return` is implemented as `Just`
on the `Maybe` type's Monad instance.

This is valid.
You should convince yourself it's valid.
To convince yourself I'm not lying 
paste this code into GHCI before continuing,
because it's getting stranger from here on.

Continuing now with the same module we can also pattern match on `Either`:

```haskell
fiveTroughEither :: Int
fiveTroughEither = case moreMonad of
  Right x -> x
  Left _y -> 9
```

Both `fiveTroughMaybe` and `fiveTroughEither` will result in `5`.
This is allowed in the same module because `moreMonad`
will only get assigned the type at the [call site](https://en.wikipedia.org/wiki/Call_site).
The compiler figures out the type of `moreMonad` by looking at usage per
call site.
This backwards 'figuring out' is normal for type variables.

[^mtl-vs-transformers]: Aren't transformers the same as mtl? No!
                        Back in the stone ages, people weren't sure what the right
                        approach would be to doing this, and frankly people still aren't sure.
                        The base package is the [transformers package](https://hackage.haskell.org/package/transformers).
                        Although the [mtl package](https://hackage.haskell.org/package/mtl)
                        is most popular, there is also [mtl-tf](https://hackage.haskell.org/package/mtl-tf).
                        [Polysemy](https://hackage.haskell.org/package/polysemy) also builds on top of transformers.
                        I think the subject of best approach is a subject for debate.
                        I'd like to point out as well that in technology, popularity means
                        nothing. Cobol used to be more popular then C up till the 1990's,
                        and is practically dead now.
                        JavaScript is also more popular then Haskell, is it therefore better?

### (optional) mastery exercise

+ Can we always pattern match on every possible monad type like we just did with `Just` or `Either`? Can we always get the value out without being in the same monad? (answer in footnote [^no-pattern-match])
<!-- + GHC figures out here the types on callsite, what mechanism can be used 
to flip the reasoning direction of GHC?
-- RankNTypes? Existentials?
-->

[^no-pattern-match]: No it's not possible if a constructor isn't exposed. Reflex uses this with [`Dynamic t`](https://hackage.haskell.org/package/reflex-0.8.1.0/docs/Reflex-Dynamic.html#t:Dynamic)
                     to create a smart destructor.
                     In reflex you can only use a value in a dynamic by putting an entire monadic action inside the dynamic, and then using [`dyn`](https://hackage.haskell.org/package/reflex-dom-core-0.6.2.0/docs/Reflex-Dom-Widget-Basic.html#v:dyn) to get it out (for reflex-dom at least, sdl uses something different).
                     I'm highlighting this because I feel this is a powerful idea.

## Transformers as constraints on `m`

With that brief introduction,
we can start applying this idea to the '["A Brief Intro to Monad Transformers" blogpost](https://blog.cofree.coffee/2021-08-05-a-brief-intro-to-monad-transformers/)'
that inspired me to write this.
In that blogpost,
a newtype is constructed to hold the entire monad transformer stack like this:

```haskell
newtype AppM a = AppM {
    runAppM :: ExceptT String (State (M.Map VariableName Int)) a
  }
  deriving newtype (Functor, Applicative, Monad, MonadError String,
                    MonadState (M.Map VariableName Int))
```

I call this `AppM` a concrete type because there is only one way to pattern match on it.
We're not allowed to pretend it's a `Maybe` for example.
This definition is used in the `assignIndexToVariables` function:

```haskell
assignIndexToVariables :: AST VariableName -> Variables -> AppM (AST Int)
```

Instead of using the concrete type `AppM`,
we could use MTL type classes to describe what is needed.
These type classes will become constraints on `m`,
similarly to how `Monad` was a constraint on `m` in the [introduction](#intro).
That is to say, we want to have [`MonadError String`](https://hackage.haskell.org/package/mtl-2.2.2/docs/Control-Monad-Except.html#t:MonadError)
as replacement for `ExceptT String` [^why-different-name],
and [`MonadState (M.Map VariableName Int)`](https://hackage.haskell.org/package/mtl-2.2.2/docs/Control-Monad-State-Lazy.html#t:MonadState)
as replacement for `State (M.Map VariableName Int)`.
Doing 
this will change the type signature of `assignIndexToVariables` as follows:

[^why-different-name]: Why MonadError and ExceptT don't share their names?
                       It's explained [here](https://www.reddit.com/r/haskell/comments/3ded39/why_cant_we_have_an_eithert_in_transformers/ct4mnk1/).

```haskell
assignIndexToVariables ::
               MonadError String m
            => MonadState (M.Map VariableName Int) m
            => AST VariableName
            -> Variables
            -> m (AST Int)
```
So what's the difference?
First of all it's a lot more verbose.
In trade for this verbosity, 
we can use the monad stack in any order
at the call site.
Both invocations of running this code are now allowed:

```haskell
main :: IO ()
main = do
 ...
 print $ flip evalState mempty $ runExceptT $
    assignIndexToVariables ast vars
 print $ runExcept $ flip evalStateT mempty $
    assignIndexToVariables ast vars
```

This wasn't possible in the original code.
We've told the compiler that the *order*
of a monad stack doesn't matter.
Which makes sense because consider two monad stacks:

```haskell
at :: Char -> ExceptT String (State (M.Map VariableName Int)) Int
bt :: Int -> StateT (M.Map VariableName Int) (Except String) String
```

These describe the same capabilities,
however the compiler would say `a` and `b` should *not* compose.
That is to say it's impossible to write:

```haskell
ct :: Char -> _ String
ct = at >=> bt
```
We don't know what goes at `_` for `ct` because `at` and `bt`
have concrete types.
We can write this composition however if
these signatures are defined in MTL style:
```haskell
am :: MonadError String m
  => MonadState (M.Map VariableName Int) m
  => Char -> m Int
bm :: MonadState (M.Map VariableName Int) m
  => MonadError String m
  => Int -> m String

cm :: MonadState (M.Map VariableName Int) m
    => MonadError String m
    => Char -> m String
cm = am >=> bm
```

Because both `bm` and `am` don't specify an order
the MTL style function definition would compose.

This section described the core idea of MTL.
I've made an [reference project](https://github.com/jappeace/mtl-src/blob/master/src/Lib.hs)
so the reader can verify the truth of my claims.
In the following sections we're going to extend MTL
using some type error driven development.
This may all sound like madness if you don't try it out with a compiler.
And I feel understanding the errors are a large part of understanding MTL.
The type errors are not easy to decipher.

## On composition and constraints

[^value-function]: Aren't values just functions with 0 arguments?
                   Especially in haskell were it may not have been evaluated yet.

Say we want to use `moreMonad` from the [introduction](#intro) in `assignIndexToVariables`.
I call `moreMonad` a tightly constrained binding.
It's only allowed to use what `Monad` typeclass provides.
`assignIndexToVariables` on the other hand is less tightly constrained,
since it has `Monad` by implication, and also everything in `MonadError` and `MonadState`.
So let's use it:

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
In mtl style, the functions with *tighter* constraints can be used in *more* situations without any refactoring.

Now we're going to do the complete opposite.
The most lax constraint possible is `MonadIO`,
which gives access to arbitrary `IO` trough the `liftIO` function.
I'm not casting judgement on if this is good or bad,
I just want to show what happens.
So let's add a [MacGyver](https://en.wikipedia.org/wiki/MacGyver) [^macgyver]
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
cryptic as it may be, it's the next one:
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
First we replace `Identity` with a gap by invoking `evalStateT` instead of `evalState` (note the `T`).
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

<!-- Oh ghci, I love it when you talk to me this way. -->

All these `m0` occur because we introduced the gap,
it has no idea what the base monad is at this point.
Which is what we want.
Let's solve it all in one change:

```haskell
    print =<< flip evalStateT mempty
        (runExceptT $ assignIndexToVariables ast vars)
```

Note that we replaced the application of `$` to a bind `=<<`.
The base monad is now IO instead of identity,
which solves everything.
hlint told me to write it like this which makes it a bit more readable

```haskell
    print =<< evalStateT
        (runExceptT $ assignIndexToVariables ast vars) mempty
```

If you like you can get the generated transformer stack out with a type hole

```haskell
    let x :: _
        x = assignIndexToVariables ast vars
    print =<< evalStateT (runExceptT x) mempty
```

which results into an error describing the hole:
```
src/Lib.hs:51:14: error:
    • Found type wildcard ‘_’
        standing for ‘ExceptT
                        String (StateT (M.Map VariableName Int) IO) (AST Int)’
      To use the inferred type, enable PartialTypeSignatures
```
Adding that type signature back into the type hole will result in a functioning
program.

[^why-dont-they]: The thing that baffles me is that they're already managing to print your own
            code, they figure out the right constraint,
            why don't they just give the example like I'm doing
            here with the message "try doing this:".


## Reinterpreting IO or the MTL style

In most situations `MonadIO` is fine.
However it doesn't allow us to reinterpret effects that are
using `IO`.
an example of reinterpretation is 
a test you where would want to measure how often your
`mcGyverLog` function is being called.
This section will show you how to do that.

First we start by MTL-izing our IO based code.
How can we rewrite `mcGyverLog` so that it doesn't use `IO` explicetly?
The function that needs IO is `putStrLn`.
It's type signature is `String -> IO ()`.
We want that `IO` to be an `m`, 
so we introduce a new typeclass for our [not invented here](https://hackage.haskell.org/package/monad-logger-0.3.36/docs/Control-Monad-Logger.html#t:MonadLogger)
log:

```haskell
class (Monad m) => NotInventedHereLog m where
    nihLog :: String -> m ()
```

We already know what implementation `m` has if it's an
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
By providing this instance we're generating lift calls over any `StateT`
for all occurences of `niLog`.


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
Alexis King her [blogpost](https://lexi-lambda.github.io/blog/2017/04/28/lifts-for-free-making-mtl-typeclasses-derivable/).


For the pure interpretation
we need to introduce a newtype which is used to attach an instance for `NotInventedHereLog`.
This type's instance will collect the logged messages,
turns out that the `WriterT` monad does exactly what we want.
So the pure code will look like this:

```
newtype NihLogT m a = MkNihLogT {
        runNihLog :: WriterT [String] m a
    } deriving (Functor, Applicative, Monad, MonadTrans, MonadWriter [String])

instance Monad m => NotInventedHereLog (NihLogT m) where
  nihLog msg = tell [msg]
```
The instance simply [`tell`](https://hackage.haskell.org/package/mtl-2.2.2/docs/src/Control.Monad.Writer.Class.html#tell)'s the message, which `mappends` it to the list.
To run this code we use `runNihLog`:
```haskell
    let pureCode :: (Either String (AST Int),  [String])
        pureCode = runWriter $ runNihLog $ runExceptT (flip evalStateT mempty $ assignIndexToVariables2 ast vars)
    print pureCode
```
With this you can write property tests on `assignIndexToVariables2`.
For example you could assert that an AST of size 20 should at least emit 40 log messages.
Obviously this isn't limited to tests or purity,
you could also add a newtype that has a connection pool to send
the messages to some database for example.

I'll tap out here.
This was supposed to be a short addendum on someone else their blogpost.
I think I dumped all my knowledge at this point on MTL and extended it a bit on
several points (like defaults mechanism from Alexes King).

Let me know if you have any strong opinions on this style
(love it or hate it, I'd like to know!)
or of course if you need any help using it.
I'm interested in effect systems in general.
Also let me know about your favorite effect system that
I didn't acknowledge (there were something like 2 or 3 right?).

## Links

+ Original [blogpost](https://blog.cofree.coffee/2021-08-05-a-brief-intro-to-monad-transformers/)
+ A functional example is available [here](https://github.com/jappeace/mtl-src/blob/master/src/Lib.hs)
+ Full test [example](https://github.com/lexi-lambda/mtl-style-example)
+ [A video presentation on the exact same topic.](https://www.youtube.com/watch?v=MPlrAe-XYMU&t=300s)
