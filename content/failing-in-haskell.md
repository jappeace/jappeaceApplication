TITLE: Failing in Haskell
DATE: 2022-02-24
CATEGORY: technique
Tags: haskell, work
OPTIONS: toc:nil

![mtl-header](images/2022/failure.png)

I saw some of my intelligent
colleagues muck up error handling.
Not only were they failing, they were failing WRONG [^anti-patterns].
This frustrates me because doing failing correctly
in Haskell is quite easy,
so why were they doing it wrong?
I believe no-one has addressed with an opinion.
Plenty people describe the
[various](http://www.randomhacks.net/2007/03/10/haskell-8-ways-to-report-errors/)
[ways](https://www.stackbuilders.com/blog/errors-and-exceptions-in-haskell/)
you can [fail](https://wiki.haskell.org/Handling_errors_in_Haskell).
But none give opinions on why certain ways are better then others.
I'll give my opinion,
maybe someone more clever then me will then correct me.
In here I'll describe the correct and only way to fail
in Haskell.
In essence, this is an answer to Eric Kidd's
[plea for consistency](http://www.randomhacks.net/2007/03/10/haskell-8-ways-to-report-errors/)
[^15-years-ago].
These are the properties we want from failure:

[^anti-patterns]: Wrong as in they were following some of the ani patterns listed below.
[^15-years-ago]: This is 15 years ago, ah well, better late then never I suppose.

1. Preciseness, vague errors are bad.
2. Locality, we need to know where errors come from.
3. Recover ability, crashing shouldn't be the only option.
4. Change ability, introduction of new error cases should be easy.

We want all these properties to make debugging easier.
This allows you to solve complicated bugs within minutes.
Effectively we structure the program in such a way
that it tells what goes wrong, where and why.
This isn't magic. It takes effort, but not magic at all.

I'll describe how to achieve above properties with some
example code for both pure and side effectful code.
However these principles apply to other languages as well [^other-langs].
We'll also go over some anti patterns and discuss their mediation.

[^other-langs]: Haskell is really good at doing failure, ironically.
                Originally I called this post failing in haskell
                but when I was writing this I realied I do the exact same thing in Java or PHP.
                It just takes more work in Java or PHP to get it to tell you what's going on.

# Pure
Haskell programmers prefer so called 'pure' code.
Therefore we start with the pure case.
Ideally pure code fail management looks like this:
```haskell
newtype BindingError = NotFound Text

bind :: Map Text Double -> Text -> Either BindingError Double
bind env argA = maybe (Left (NotFound argA)) Right $ Map.lookup argA env

data DivideErrors = DivideBinding BindingError
                  | DivisionByZero Double Double
                  | DivNegativeDivision Double Double
                  | DivNumberThreeIsBad Double Double

divide :: Map Text Double -> Text -> Text -> Either DivideErrors Double
divide env argA argB = do
    valA <- first DivideBinding $ bind env argA
    valB <- first DivideBinding $ bind env argB
    when (valB == 0) $ Left $ DivisionByZero valA valB
    when (valB < 0) $ Left $ DivNegativeDivision valA valB
    when (valA == 3.0 || valB == 3.0) $ Left $ DivNumberThreeIsBad valA valB
    pure $ valA / valB
```

Here we introduce the divide function.
Which takes an environment map, looks up the values of said
map, and then performs division after doing some checks.
This is done in the `Either` monad.
Errors are emitted by using the `Left` constructor,
which is the error branch according to `Either`'s
monad instance.

This code will tell you exactly what went wrong
if something goes wrong.
Locality in this case can be improved a little by adding
a different constructor for each `bind` call,
but in this case I'd argue locality is close enough.
The developer has the complete opportunity to recover from these errors,
a simple pattern match would suffice.
Furthermore if business demands yet another weird constraint,
such as `NumberFourIsBad`, pattern matches at call sites will emit
a [`-Wincomplete-patterns` warning](https://downloads.haskell.org/~ghc/latest/docs/html/users_guide/using-warnings.html#ghc-flag--Wincomplete-patterns).

If we need to compose these errors in a larger program we can
simply wrap previous errors in a bigger sumtype,
consider the following function `plusDiv` which emulates `(a + b) / c`
```haskell
data PlusErrors = PlusBinding BindingError
                | PlusNoZeroResults
                | PlusNumberThreeIsBad Double Double

plus :: Map Text Double -> Text -> Text -> Either PlusErrors Double
plus env argA argB = do
    valA <- first PlusBinding $ bind env argA
    valB <- first PlusBinding $ bind env argB
    when (valA == 3.0 || valB == 3.0) $
      Left $ PlusNumberThreeIsBad valA valB
    let res = valA + valB
    when (res == 0) $ Left PlusNoZeroResults
    pure $ valA / valB

data PlusDivErrors = PDPlusError PlusErrors
                   | PDDivErrors DivideErrors
                   | PDBind BindingError

-- | (a + b) / c
plusDiv :: Map Text Double
        -> Text -> Text -> Text
        -> Either PlusDivErrors Double
plusDiv env argA argB argC = do
  res <- first PDPlusError $ plus env argA argB
  cres <- first PDBind $ bind env argC
  first PDDivErrors $
    divide (Map.fromList [("one", res), ("two", cres)]) "one" "two"
```

The higher 'level' function `plusDiv` absorbs all errors with extra constructors,
similarly to a tree.
Once more this tells us exactly what part of the computation failed,
if any.
In this case, the [first](https://hackage.haskell.org/package/bifunctors-5/docs/Data-Bifunctor.html#v:first)
function is being used like
[lift](https://hackage.haskell.org/package/transformers-0.6.0.2/docs/Control-Monad-Trans-Class.html#v:lift):
Transforming the function it wraps to run in the top environment.

In certain cases you can use [Data.Validation](https://hackage.haskell.org/package/validation-1.1.2/docs/Data-Validation.html)
instead of `Either`.
Which can collect more then one error.
But usage of that is out of the scope of this blogpost.

# IO and exceptions
I think a good rule of thumb is to use exceptions when
you expect the program to stop.
This should only be done within a monad stack that has IO as base,
because exceptions are part of the `IO` 'contract'[^throw].
This contract extends to any transformer stack that has `IO` as base.
For convenience I'll write out the example in plain `IO` however:

```haskell
data DivideException where
    MkDivideException :: HasCallStack =>
        DivideFailures -> DivideException 

instance Show DivideException where
    show (MkDivideException errors) =
        renderExceptionWithCallstack errors "MkDivideException"

renderExceptionWithCallstack ::
        (HasCallStack, Show a) => a -> String -> String
renderExceptionWithCallstack errors valueConstructor = "(" <> valueConstructor <> " $ "
      <> show errors
      <> "/*"
      <> prettyCallStack callStack
      <> " */)"

throwDivide :: HasCallStack => Either DivideFailures a -> IO a
throwDivide = either (throwIO . MkDivideException) pure 

myEnv :: Map Text Double 
myEnv = Map.fromList [("zero", 0.0), ("one", -1.0),
                      ("two", 2.0), ("three", 3.0)]

main :: IO ()
main = do 
    result1 <- throwDivide $ divide myEnv "one" "two"
    result2 <- throwDivide $ divide myEnv "one" "three" -- throws
    print (result1 , result2)
```

[^throw]: See the throw [anti pattern](#throw)

All this boilerplate attaches the callstack to our exception.
Note that all we did to the pure code error handling is put it
in the exception itself, so this freely composes.
This idea of attaching callstacks to your exceptions is
explained further in [this blogpost]

## MTL
I recently blogged about [mtl]({filename}/mtl.md),
so I'll briefly cover how to modify this code into mtl style as well:

```haskell
throwDivide :: (HasCallStack, MonadIO m)  => ExceptT DivideFailures a -> m a
throwDivide = either (liftIO . throwIO . MkDivideException) pure . runExceptT

myDBFunc :: (MonadError DivideFailures m, MonadDB m) => m Double
myDBFunc = do
    myEnv <- getMyEnv
    divide myEnv "one" "two"

main :: IO ()
main = do 
    result1 <- runDB $ throwDivide $ myDBFunc 
    print result1 
```
throwDivide works quite similarly as in the previous example but now
it works with any transformer stack based on `IO.


# Anti patterns
Now I'll cover several anti patterns I've seen and discuss what to do differently.
Keep in mind some of these are in the wild,
for example [aeson](https://hackage.haskell.org/package/aeson-2.0.3.0/docs/Data-Aeson.html#v:eitherDecode)
famously exposes a `String` for errors,
which is problematic for a library as I'll discuss right now.

## Text in left branch of Either
For example both of these would contain text in the left branch:
```haskell
y :: Either Text a
x :: Either String a
```

The problem is that we break the recover ability
property.
There is no way to make a closed pattern match
on every string.

What should be done instead is creating a sumtype:
```haskell
data YErrors = YErrorOne
             | YErrorTwo Double

y :: Either YErrors a
```
This allows the developer to pattern match on the result
This way client code can pattern match on all possible
branches, and if the additional errors get introduced
the compiler notifies them through the
incomplete pattern warning.

## `throw`
Never use `throw`. It allows throwing of exceptions in pure code.
This is very wrong because the code ends up behaving like a null
pointer in java due to Haskell's non strict evaluation.
In other words, this breaks the locality of errors.

Consider for example:

```haskell
myInts :: [Int]
myInts = []

data EmptyException = MkEmptyException
    deriving Exception

myHead :: [Int] -> Int
myHead [] = throw $ MkEmptyException
myHead (x : y) = x

main :: IO ()
main = do
   let x = myHead myInts
   print $ [0..5] <> myInts
   print x
```

It fails on that last line.
Even though the error was at
the x binding.

Much better is to use throwIO,
modifying the above example:
```haskell
myInts :: [Int]
myInts = []

data EmptyException = MkEmptyException
    deriving Exception

myHead :: [Int] -> IO Int
myHead [] = throwIO $ MkEmptyException
myHead (x : y) = pure x

main :: IO ()
main = do
   x <- myHead myInts
   print $ [0..5] <> myInts
   print x
```
This will indeed fail on the first line.
We lost purity,
in this case `Either` could also have been used as
described above which is even better.
Furthermore we can use the `HasCallStack`
trick as well.

## Generic app exceptions
I'm talking about something like this:
```haskell
data AppException = MkAppException Text
    deriving Exception
```
This is bad because it ends up being thrown
at many places with no good way of recovering,
furthermore the error may be vague,
depending on what's being put in the Text field.

It's better to define a custom exception per situation.
For example:
```haskell
data DatabaseNotFound = MkDatabaseNotFound UUID
    deriving Exception

data AwsResourceNotFound = MkAwsResourceNotFound UUID
    deriving Exception
```
These are precise and type safe, we no longer can be vague because
you have to provide a UUID and tell what you're talking about to throw.

However, you'll likely already have this generic app
error being called from 400 places.
Fortunately we can recover some locality by rewriting
the exception [in a GADT](https://maksbotan.github.io/posts/2021-01-20-callstacks.html#capturing-stacks):

```haskell
data AppException where
    MkAppException :: HasCallStack => Text -> AppException 
    deriving Exception
```

## Squashing errors

This can occur when using bind on Maybe for example
```haskell
x :: Maybe Int
y :: Maybe Int

z :: Maybe Int
z = do
   x' <- x
   y' <- y
   pure $ x' + y'
```
`Nothing` will not tell us why z didn't work out.
This is wrong because it breaks preciseness.
instead we should do something like this:

```haskell
x :: Maybe Int
y :: Maybe Int

data ZFailures = NoX
               | NoY

z :: Either ZFailures Int
z = do
  x' <- maybe (Left NoX) Right $ x
  y' <- maybe (Left NoY) Right $ y
   pure $ x' + y'
```

This will tell you exactly what went wrong,
while retaining most of the power of bind.

# Haskell specific tools

+ Make sure to compile your program with `-Wall`,
  This enable the exhaustiveness checker.
+ The [stack trace](https://github.com/waddlaw/haskell-stack-trace-plugin) plugin
  seems like a good idea for applications that can handle the change in perforamnce.
  But for development it's good in any case.
+ [Data.Validation](https://hackage.haskell.org/package/validation-1.1.2/docs/Data-Validation.html)
  should be preferred over Either when possible,
  because it allows collecting of multiple errors.

# Conclusion

I'm an expert on failure now.


# References

+ https://github.com/waddlaw/haskell-stack-trace-plugin
+ https://www.stackbuilders.com/blog/errors-and-exceptions-in-haskell/
+ https://wiki.haskell.org/Handling_errors_in_Haskell
+ http://www.randomhacks.net/2007/03/10/haskell-8-ways-to-report-errors/
+ https://maksbotan.github.io/posts/2021-01-20-callstacks.html
+ https://hackage.haskell.org/package/validation-1.1.2/docs/Data-Validation.html

