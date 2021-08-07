Title: Use MTL instead of transformers
Date: 2021-08-07 15:44
Category: technique
OPTIONS: toc:nil
Tags: haskell, programming, mtl
subreddit: haskell programming reflexfrp
Status: draft

Recently a blog post came out how to use the concrete
base [transformers](https://blog.cofree.coffee/2021-08-05-a-brief-intro-to-monad-transformers/).
This approach is correct, and will result in functioning programs.
Although to me it looks like quite a low level approach.
Using transformers directly will work,
but I strongly feel you'll get more out of them by
opting into full MTL.

In here I'll give a brief overview of doing this MTL style instead of
transformer style.
I'm not claiming this is a new idea or you should even adopt this style.
It's just a way I tend to structure my program,
and I learned it by using reflex instead of reading.
I feel there is surprising little information on this topic.
Here I will write down how it works by using words so people can read
instead of having to struggle with this for days.

Let's start from the [blogpost](https://blog.cofree.coffee/2021-08-05-a-brief-intro-to-monad-transformers/),
where a newtype is constructed to hold the entire stack like this:

```haskell
newtype AppM a = AppM { runAppM :: ExceptT String (State (M.Map VariableName Int)) a }
  deriving newtype (Functor, Applicative, Monad, MonadError String, MonadState (M.Map VariableName Int))
```

In truth, we don't care about this definition,
we just want to use it.
As does the author of the blogpost so he gives an example
function with the following type signature.

```
assignIndexToVariables :: AST VariableName -> Variables -> AppM (AST Int)
```

What I'm pitching here, is that instead of using the concrete `AppM`
You describe what you want instead with the MTL type classes on top of a
polymorhic variable `m`.
That is to say, we want to have `MonadError String` as anologue to `ExceptT String`,
and `MonadState (M.Map VariableName Int)` as analogue to `State (M.Map VariableName Int)`.
```
assignIndexToVariables ::
    MonadError String m =>
    MonadState (M.Map VariableName Int) m =>
    AST VariableName -> Variables -> m (AST Int)
```

So what's the difference?
The difference here is in running the code.
Both invocations are allowed now:

```haskell
main :: IO ()
main =
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

## The smallest `m`.

If you're confused about why monad stacks can figure out their
underlying types by simply running it consider the following example:

```haskell
moreMonad :: Monad m => m Int
moreMonad = pure 5

eitherFive :: Int
eitherFive = case moreMonad of
  Right x -> x
  Left _y -> 9

maybeFive :: Int
maybeFive = case moreMonad of
  Just x -> x
  Nothing -> 9
```

Both `maybeFive` and `eitherFive` will result in `5`.
Since `pure` for `Maybe` is a `Just`, and for `Either` it's `Right`.
The compiler figures out the type of `moreMonad` by looking at how it's used,
since we pattern matched on an `Either` in `eitherFive` it can only possibly be `Either`,
*in that situation*.
But in other situations, like `maybeFive` it can still be something else.
I once held a [presentation](https://www.youtube.com/watch?v=MPlrAe-XYMU&t=300s) about this as well
that goes into detail explaining everything.


## On composition and constraints

Let's say for some reason our would need `moreMonad`
for some reason:

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


Now let's add a MacGyver logging function as follows:

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
but sort off have to know what a context is, and a type signature to
decipher that error message[^why-dont-they].
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
This will result in the following beauty:
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
You can simply copy paste that in your code to get a functioning program
back out.
No idea why you'd do that, no-one cares about transformer details.

[^why-dont-they]: The thing that baffles me is that they're already managing to print your own
            code, they figure out the right constraint,
            why don't they just give the example like I'm doing
            here with the message "try doing this:".


## Reinterpreting IO or the MTL style

I have to say I don't do this particular part on a regular basis.
I'm fine with `MonadIO` in general,
However it doesn't allow us to reinterpret effects that are
using `IO`.
At least not without passing an explicit stub in,
which would defeat the purpose of using mtl in the first place.
So if you ever feel the need for reinterpreting these effects,
such as when writing unit tests, you can fallback to this section.

Let's begin with the cause of our IO.
`macGyverLog`.
How can we rewrite this so that it doesn't use IO?
If we look at the source the function that needs IO is `putStrLn`.
It's typesignature is `String -> IO ()`,
so what if we just put it in a typeclass like mtl is doing?

```haskell
class (Monad m) => MacGyverLog m where
    macGyverLog :: String -> m ()
```
And we know how to figure this out for `IO` in specific:
```haskell
instance MacGyverLog IO where
    macGyverLog :: String -> IO ()
    macGyverLog = putStrLn
```



I already hear the readers cringe realizing we secretly infected
our program with IO.
Can't we be pure?!
YES.
We have to just dive deeper in to the mtl rabbit hole.

## Links

+ A functional example is available [here](https://github.com/jappeace/mtl-src/blob/master/src/Lib.hs)
+ Original [blogpost](https://blog.cofree.coffee/2021-08-05-a-brief-intro-to-monad-transformers/)
+ OH WOOT https://github.com/lexi-lambda/mtl-style-example
+ [presentation](https://www.youtube.com/watch?v=MPlrAe-XYMU&t=300s)
