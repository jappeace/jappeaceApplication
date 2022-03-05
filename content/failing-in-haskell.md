TITLE: Failing in Haskell
DATE: 2022-02-27
Modified: 2022-03-05 03:40
CATEGORY: technique
Tags: haskell, work
OPTIONS: toc:nil

![mtl-header](images/2022/failure.png)

Recently I encountered some dubious error handling code.
Not only was it failing, it was failing WRONG [^anti-patterns].
This frustrates me because doing failing correctly
in Haskell is quite easy,
so why was it implemented wrongly?
I believe no-one has addressed failing with an opinion.
Plenty of people describe the
[various](http://www.randomhacks.net/2007/03/10/haskell-8-ways-to-report-errors/)
[ways](https://www.stackbuilders.com/blog/errors-and-exceptions-in-haskell/)
[you](https://www.fpcomplete.com/haskell/tutorial/exceptions/)
[can](https://www.schoolofhaskell.com/school/starting-with-haskell/basics-of-haskell/10_Error_Handling)
[fail](https://wiki.haskell.org/Handling_errors_in_Haskell).
But none give opinions on why certain ways are better than others.
Therefore I shall share my failing expertise,
and describe the correct way to fail in Haskell.
In essence, this is an answer to Eric Kidd's
[plea for consistency](http://www.randomhacks.net/2007/03/10/haskell-8-ways-to-report-errors/)
[^15-years-ago].
These are the properties we want from failure:

[^anti-patterns]: Wrong as in they were following some of the ani patterns listed below.
[^15-years-ago]: This is 15 years ago, ah well, better late then never I suppose.

1. Preciseness, vague errors are bad.
2. Locality, we need to know where errors come from.
3. Recoverability, the program should be able to recover after an error.
4. Change ability, introduction of new error cases should be easy.

We want all these properties to make debugging easier.
This allows you to solve complicated bugs within minutes.
We structure the program so that it tells what goes wrong,
where and why.
This isn't magic. It takes effort, but not magic at all.

I'll describe how to achieve the above properties with some
example code for both pure and side effectful code.
However these principles apply to other languages as well [^other-langs].
We'll also go over some anti patterns and discuss their mediation.

[^other-langs]: When I was writing this I realized I do the exact same program structuring
                in Java or PHP.
                It is more work in Java or PHP, but possible and saves so much debugging time.

# Pure
Haskell programmers prefer so called 'pure' code.
By which we mean immutable memory computations[^memory].
Therefore we start with the pure case.
Ideally pure code fail management looks like this:
```haskell
newtype LookupError = NotFound Text

lookup :: Map Text Double -> Text -> Either LookupError Double
lookup env argA = maybe (Left (NotFound argA)) Right $ Map.lookup argA env

data DivideErrors = DivideLookup LookupError
                  | DivisionByZero Double Double
                  | DivNegativeDivision Double Double
                  | DivNumberThreeIsBad Double Double

divide :: Map Text Double -> Text -> Text -> Either DivideErrors Double
divide env argA argB = do
    valA <- first DivideLookup $ lookup env argA
    valB <- first DivideLookup $ lookup env argB
    when (valB == 0) $ Left $ DivisionByZero valA valB
    when (valB < 0) $ Left $ DivNegativeDivision valA valB
    when (valA == 3.0 || valB == 3.0) $ Left $ DivNumberThreeIsBad valA valB
    pure $ valA / valB
```

[^memory]: Accessing memory is for no particular reason considered pure,
           although one wonders if this could be changed
           to make managing memory bounds easier.

Here we introduce the divide function.
Which takes an environment map,
looks up the values of said map,
and then performs division after doing some checks.
We introduce the environment as a convenient common
"this can fail" lookup.
But business may give us other in-variants
such as `DivNumberThreeIsBad`,
which also neatly fits in this "pattern".
Errors are emitted by using the `Left` constructor,
which is the error branch according to `Either`'s
`Monad` instance.

This code will tell you exactly what went wrong
if something goes wrong.
Locality in this case can be improved a little by adding
a different constructor for each `lookup` call,
but in this case I'd argue locality is close enough.
The developer has the opportunity to recover from these errors,
a simple pattern match would suffice.
Furthermore if business demands yet another weird constraint,
such as `NumberFourIsBad`, pattern matches at call sites will emit
a [`-Wincomplete-patterns`](https://downloads.haskell.org/~ghc/latest/docs/html/users_guide/using-warnings.html#ghc-flag--Wincomplete-patterns) warning.

If we need to compose these errors in a larger program we can
simply wrap previous errors in a bigger sumtype,
consider the following function `plusDiv` which emulates `(a + b) / c`
```haskell
data PlusErrors = PlusLookup LookupError
                | PlusNoZeroResults
                | PlusNumberThreeIsBad Double Double

plus :: Map Text Double -> Text -> Text -> Either PlusErrors Double
plus env argA argB = do
    valA <- first PlusLookup $ lookup env argA
    valB <- first PlusLookup $ lookup env argB
    when (valA == 3.0 || valB == 3.0) $
      Left $ PlusNumberThreeIsBad valA valB
    let res = valA + valB
    when (res == 0) $ Left PlusNoZeroResults
    pure $ valA / valB

data PlusDivErrors = PDPlusError PlusErrors
                   | PDDivErrors DivideErrors
                   | PDLookup LookupError

-- | (a + b) / c
plusDiv :: Map Text Double
        -> Text -> Text -> Text
        -> Either PlusDivErrors Double
plusDiv env argA argB argC = do
  res <- first PDPlusError $ plus env argA argB
  cres <- first PDLookup $ lookup env argC
  first PDDivErrors $
    divide (Map.fromList [("one", res), ("two", cres)]) "one" "two"
```

The higher 'level' function `plusDiv` absorbs all errors from other functions
with extra constructors.
This isn't the most ergonomic approach because variables
have to go through the environment map.
In a real program you could factor out this function.
For example with an `innerPlus :: Double -> Double -> Either PlusErrors Double`.
This adds additional type safety as well,
because the `Double` type is "smaller" then a
`Text` type in both memory and cardinality.
However I think for an example or early prototype it's good.
Once more this tells us exactly what part of the computation failed,
if any.
In this case, the [first](https://hackage.haskell.org/package/bifunctors-5/docs/Data-Bifunctor.html#v:first)
function is being used like
[lift](https://hackage.haskell.org/package/transformers-0.6.0.2/docs/Control-Monad-Trans-Class.html#v:lift):
Transforming the function it wraps to run in the "higher level" `Either` environment.
Some may even call this a natural transformation. 

In certain cases you can use [Data.Validation](https://hackage.haskell.org/package/validation-1.1.2/docs/Data-Validation.html)
instead of `Either`.
Which can collect more then one error.
But usage of that is out of the scope of this blogpost.

# `IO` and exceptions
It's more difficult to recover from an exception than it is from a pure error value.
As a rule of thumb you can use exceptions when you expect the program to stop.
For example when you can't find a critical resources from the database.
Another consideration is the value of the error.
If it's important that an error be handled correctly,
then exceptions should be avoided.
However, this should only be done within a monad stack that has `IO` as base,
because exceptions are part of the `IO` 'contract'[^throw].
This contract extends to any transformer stack that has `IO` as base.
For convenience however, I'll write out the example in plain `IO`:

```haskell
data DivideException where
    MkDivideException :: HasCallStack =>
        DivideErrors -> DivideException

deriving instance Exception DivideException

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

throwDivide :: HasCallStack => Either DivideErrors a -> IO a
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
We also put the entire pure error type
directly into the exception.
It composes.
This is why pure error handling is preferable,
but if you don't have time to do this, exceptions like those described above are good too.
This idea of attaching call stacks to your exceptions is
explained further in [this blogpost](https://maksbotan.github.io/posts/2021-01-20-callstacks.html)

If that's to much work, the [`error`](https://hackage.haskell.org/package/base/docs/Prelude.html#v:error)
call also has a stack trace,
although its type allows vagueness unfortunately.
Also make sure to attach it to IO,
or the [nullpointer](#throw) anti pattern may occur.
I think the [`errorIO`](https://hackage.haskell.org/package/extra-1.7.10/docs/Control-Exception-Extra.html#v:errorIO)
function is a good idea instead of `error`
because it avoids that nullpointer scenario entirely.
Don't try to catch errors,
use exceptions if you need to catch.

## MTL
I recently blogged about [mtl]({filename}/mtl.md),
so I'll briefly cover how to modify this code into mtl style as well:

```haskell
throwDivide :: (HasCallStack, MonadIO m)  => ExceptT DivideFailures a -> m a
throwDivide meow = do
    res <- runExceptT meow
    case res of 
      Left err -> liftIO $ throwIO $ MkDivideException err
      Right res -> pure res

myDBFunc :: (MonadError DivideFailures m, MonadDB m) => m Double
myDBFunc = do
    myEnv <- getMyEnv
    divide myEnv "one" "two"

main :: IO ()
main = do 
    result1 <- runDB $ throwDivide $ myDBFunc 
    print result1 
```

`throwDivide` works quite similarly as in the previous example but now
it works with any transformer stack based on `IO`.
This works because we pretend the `ExceptT` exists at the call site,
which makes it come true.
This is explained thoroughly in the previous [blog post]({filename}/mtl.md).

# Anti patterns
Now I'll cover several anti patterns I've seen and
discuss what to do differently.
Some of these patterns occur in the wild,
for example [aeson](https://hackage.haskell.org/package/aeson-2.0.3.0/docs/Data-Aeson.html#v:eitherDecode)
famously exposes a `String` for errors,
which is problematic for a library.
The next section explains why.

## Text in left branch of Either
These examples contain text in the left branch:
```haskell
y :: Either Text a
x :: Either String a
```

The problem is that we break the recoverability
property.
There is no way to make a closed pattern match on a string.
Instead we should create a sumtype for possible errors:
```haskell
data YErrors = YErrorOne
             | YErrorTwo Double

y :: Either YErrors a
```
This way client code can pattern match on all possible
branches, and if the additional errors get introduced
the compiler notifies the developer through
[`-Wincomplete-patterns` warning](https://downloads.haskell.org/~ghc/latest/docs/html/users_guide/using-warnings.html#ghc-flag--Wincomplete-patterns).

## `throw`
`throw` allows throwing exceptions in pure code.
This is very wrong because the code ends up behaving like a null
pointer in Java, due to Haskell's non strict evaluation.
In other words, this breaks the locality of errors.
Consider for example:
```haskell
myInts :: [Int]
myInts = []

data EmptyException = MkEmptyException
    deriving (Exception, Show)

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
Even though the error was at the `x` binding.
Much better is to use [`throwIO`](https://hackage.haskell.org/package/base/docs/Control-Exception.html#v:throwIO),
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
in this case `Either` could have been used as
described above, which is even better.
Alternatively we can use the `HasCallStack`
trick, if we stick with exceptions.

## Generic app exceptions
I'm talking about something like this:
```haskell
data AppException = MkAppException Text
    deriving Exception
```
This is bad because it ends up being thrown
at many places with no good way of recovering.
Exceptions are already difficult to recover from,
but if they're re-used a lot,
it becomes even more difficult.
Now you'd have to pattern match on the `Text`
to handle the correct error and hope no one
re-uses that particular error text, ever!
Furthermore the error may be vague,
depending on what's being put in the `Text` field.

It's better to define a custom exception per situation.
For example:
```haskell
data DBUserNotFound = MkDBUserNotFound { userId :: UUID }
    deriving Exception

data AwsImgResourceNotFound = MkImgResourceNotFound {name :: Text, awsId :: UUID }
    deriving Exception
```
These are precise and type safe, we no longer can be vague because
you have to provide a `UUID`.
This forces the throw site to capture this `UUID` from
the environment, making the error precise trough type safety.

However, you'll likely already have generic app
exception being called from different 400 places.
Fortunately we can recover some of the locality property by rewriting
the exception [in a GADT](https://maksbotan.github.io/posts/2021-01-20-callstacks.html#capturing-stacks)
and using the `HasCallStack` trick.

## Squashing errors
This can occur when using bind `>>=` on `Maybe`, for example:
```haskell
x :: Maybe Int
y :: Maybe Int

z :: Maybe Int
z = do
   x' <- x
   y' <- y
   pure $ x' + y'
```
`Nothing` won't tell us why `z` failed.
This is wrong because it breaks preciseness.
instead we should restructure like so:

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
It's fine to use `Maybe` if there is a single
error case,
but often this isn't the case.

# Conclusion

The preference relation of failing is like this:

```
pure error handling > exceptions *with* stack traces > errorIO calls
```

Anything else is most likely *wrong* and should be avoided.
Structure your programs so that it tells what goes wrong,
where and why.
I think most software would benefit from doing this,
and these concepts are easy.
Let [me know](mailto:hi@jappie.me) if you disagree, or need help with this.

# References

+ https://github.com/waddlaw/haskell-stack-trace-plugin
+ https://www.stackbuilders.com/blog/errors-and-exceptions-in-haskell/
+ https://wiki.haskell.org/Handling_errors_in_Haskell
+ http://www.randomhacks.net/2007/03/10/haskell-8-ways-to-report-errors/
+ https://maksbotan.github.io/posts/2021-01-20-callstacks.html
+ https://hackage.haskell.org/package/validation-1.1.2/docs/Data-Validation.html

