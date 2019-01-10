Title: Lens into wrapped newtypes
Date: 2018-12-30 21:34
Category: technique
OPTIONS: toc:nil
subreddit: haskell programming
Tags: haskell, programming, tools, lens

![Categorical representation of the NT iso](/images/2018/nt-iso.svg)

> All newtypes are isomorphisms
> 
> <br />
>
> -- My mother

[Control.Lens.Wrapped](http://hackage.haskell.org/package/lens-4.17/docs/Control-Lens-Wrapped.html)
uses the isomorphism property to introduce a type class `Wrapped`.
Let's explore use cases, because after all, it doesn't appear to do much at first glance.
What's the point of formalizing wrapping and unwrapping of types?

Instance boilerplate will be reduced in this blog post.
In my use case this will include orphan instances.
Furthermore, I believe that this technique will make using newtypes more attractive.

# Newtype
Consider the following common code in
a [fullstack haskell webapp]({filename}/fullstack-haskell-reflex-servant.md):

```haskell
data Login = Login
	{ email :: Text
	, password :: Text
	}
```

Looks good? No of course not.
This common code is shared between both client and server,
therefore we should be pendantic about these record field.
We wrap common occurrences such as Text in newtypes
so we don't accidentally mix up the fields.
This would be a better representation:

```haskell
newtype Email = Email { unEmail :: Text }
newtype Password = Password { unPassword :: Text }

data Login = Login
	{ email :: Email 
	, password :: Password
	}
```

Of course this extra safety comes at the cost of more boilerplate,
but these few lines add a lot of safety:
Mixing of email and password becomes less likely,
from frontend input to the backend.
This is analogous to having integration tests on user input fields,
AJAX calls, HTTP endpoints, and database insertion.
That's a lot of safety for two extra lines,
therefore we accept this trade and move on.

# Database
Now we can put the fields of login directly into our
user table schema for the database:

```haskell
-- database
data UserT f = User 
	{ email :: C f Email
	, password :: C f Password
	}
	-- etc beam boilerplate...
```

Although this is what we want, it doesn't compile,
we need to tell beam how to get the right underlying type
so it can produce the right queries and schema:

```haskell
-- backend orphanage
instance HasSqlEqualityCheck PgExpressionSyntax Email
```
This instance allows us to use the [`==.`](http://hackage.haskell.org/package/beam-core-0.7.2.2/docs/Database-Beam-Query.html#v:-61--61-.)
operator on beam expressions directly.
We can now compare the column email with a client email.
I don't know why an instance is needed for this, but the
compiler wanted it whenever I used that operator.
```haskell
-- backend orphanage
instance HasSqlValueSyntax PgValueSyntax Email where
  sqlValueSyntax = sqlValueSyntax . unEmail
```
Here we use `unEmail` to the underlying type their sqlValuesyntax.

```haskell
-- backend orphanage
instance FromBackendRow Postgres Email
```
I believe this tells beam we want to be able to use a postgres database
on email.
We don't have to instantiate sqlite instances
(beam can do multiple backends).

```haskell
-- backend orphanage
instance FromField Email where
  fromField a b = Email <$> fromField a b
```
[FromField](https://hackage.haskell.org/package/postgresql-simple-0.5.4.0/docs/Database-PostgreSQL-Simple-FromField.html#t:FromField)
is a type class from [PgSimple](https://hackage.haskell.org/package/postgresql-simple-0.5.4.0/docs/Database-PostgreSQL-Simple.html).
Here, we're telling the compiler to just use the from field from the underlying type,
and once it's done we can wrap it back into an email.
```haskell
-- backend orphanage
instance HasDefaultSqlDataTypeConstraints PgColumnSchemaSyntax Email
```
This is a constraint coming from beam migrate,
if you want to have automatic schema generation,
or be able to step trough various schemas you need this.
```haskell
-- backend orphanage
instance HasDefaultSqlDataType PgDataTypeSyntax Email where
  defaultSqlDataType proxy = defaultSqlDataType $ unEmail <$> proxy
```
This is also needed for migrations.
Here we're removing the newtype from the proxy to tell it to use the
underlying type.
Note that a proxy isn't holding any data,
those functions will never be executed,
in case of proxies we're just interested in type.

All of this is repeated for Password to,
and any other newtypes you want:
```haskell
-- backend orphanage
instance HasSqlEqualityCheck PgExpressionSyntax Password
instance HasSqlValueSyntax PgValueSyntax Password where
  sqlValueSyntax = sqlValueSyntax . unPassword
instance FromBackendRow Postgres Password
instance FromField Password where
  fromField a b = Password <$> fromField a b
instance HasDefaultSqlDataTypeConstraints PgColumnSchemaSyntax Password
instance HasDefaultSqlDataType PgDataTypeSyntax Password where
  defaultSqlDataType proxy = defaultSqlDataType $ unPassword <$> proxy
```

I put these instances into an orphanage
(dedicated file for orphan instances)
because I want to put the newtypes directly into the database.
However I don't want our common code to be dependent on beam,
that would mean the frontend JavaScript suddenly would pull
in beam as a dependency for no reason.
We'll eliminate the need for these orphans later.

What did we gain?
The ability to put these newtypes in the database,
what did we lose?
Well we now have a lot of extra boilerplate to content with.

# Wrapped
Let's kill the boilerplate!

```haskell
-- common
newtype Email = Email { unEmail :: Text } deriving Generic
newtype Password = Password { unPassword :: Text } deriving Generic

instance Wrapped Email
instance Wrapped Password
```
If you can provide an [Iso'](http://hackage.haskell.org/package/lens-4.17/docs/Control-Lens-Iso.html#t:Iso-39-),
then you instantiate the Wrapped type class.
If a newtype has derived generic we get an instance for free by just declaring it, and using the default.
This is possible because generic knows about constructors.

```haskell
-- backend orphanage
wrappedSqlValueSyntax  :: (Wrapped a, HasSqlValueSyntax b (Unwrapped a)) => a -> b
wrappedSqlValueSyntax  = sqlValueSyntax . view _Wrapped'

fromWrappedField :: (Wrapped a, FromField (Unwrapped a)) => FieldParser a
fromWrappedField a b = review _Wrapped' <$> fromField a b

wrappedDefaultSqlDataType :: (Wrapped a, HasDefaultSqlDataType b (Unwrapped a)) => Proxy a -> Bool -> b
wrappedDefaultSqlDataType proxy = defaultSqlDataType $ view _Wrapped' <$> proxy
```
These functions pull out the essence of wrapping.
If `a` is wrapped, we can speak about it's unwrapped form (which is why we need the type class).
If `a` his unwrapped form for example implements `FromField`, we can make a `FieldParser` for it.

```haskell
-- backend orphanage
instance HasSqlEqualityCheck PgExpressionSyntax Email
instance HasSqlValueSyntax PgValueSyntax Email where
  sqlValueSyntax = wrappedSqlValueSyntax
instance FromBackendRow Postgres Email
instance FromField Email where
  fromField = fromWrappedField
instance HasDefaultSqlDataTypeConstraints PgColumnSchemaSyntax Email
instance HasDefaultSqlDataType PgDataTypeSyntax Email where
  defaultSqlDataType = wrappedDefaultSqlDataType

instance HasSqlEqualityCheck PgExpressionSyntax Password
instance HasSqlValueSyntax PgValueSyntax Password where
  sqlValueSyntax = wrappedSqlValueSyntax
instance FromBackendRow Postgres Password
instance FromField Password where
  fromField = fromWrappedField
instance HasDefaultSqlDataTypeConstraints PgColumnSchemaSyntax Password
instance HasDefaultSqlDataType PgDataTypeSyntax Password where
  defaultSqlDataType = wrappedDefaultSqlDataType
```
The instances themselves do the same thing over and over,
they wrap or unwrap types to get the underlying interesting value.
Here this is obvious by having the instances of both
Email and Password point to the same functions.

At first glance, this implementation does not look better.
However we now can clearly see that the wrapping is indeed
the same operation because the instances all point toward the same
functions.
The FromField instance for Password is implemented with fromWrappedField,
so does the Email instance.
This is possible because both Email and Password have instantiated the
Wrapped instance.

This change is a lot better if you consider that there is no more
logic being repeated here.
Which means there are no more logic bugs in the repetition.
The boilerplate is now in it's purest form: Dumb repetition.
By itself I wouldn't consider the current state to be that bad anymore.
However, these are still orphans,
which can cause [bad problems](https://stackoverflow.com/questions/3079537/orphaned-instances-in-haskell).
We should kill all orphans!

# A general instance
Can't we make a generalized instance that does all of this wrapping
for all newtypes?
My first attempt was rather crazy looking back.
I wanted to create the ultimate orphan.
a polymorphic instance that was kept in check by constraints
such as `Wrapped` and the fact underlying types would have
these beam instances implemented.
I attempted this but did got very far, I wasn't very sure what was
going on anymore with the type errors I got out of that.
But after a bit of searching I realized that what I attempted
to do was ridiculous and dangerous.
Of course that wouldn't work, now all previous and future `Wrapped`
newtypes would have to be able to be fit into postgres or fail.
This piece of code would break all existing libraries that would've
had a `Wrapped` newtype.
No this was an absurd idea.

Rather than solving the problem for all newtypes, 
I stepped back, and added yet another newtype:
```haskell
-- wrapped.hs
newtype DBFieldWrap a = DBFieldWrap
  { _unField :: a
  } deriving (Generic, Show)

instance Wrapped a => Wrapped (DBFieldWrap a)
instance (Wrapped a, BeamBackend be,
          BackendFromField be (DBFieldWrap a),
          FromBackendRow be (Unwrapped a)
         ) =>
         FromBackendRow be (DBFieldWrap a)
```
That final instance shows the core idea.
We add the wrapped restriction on `a`,
which allows us to speak about the unwrapped form of `a`.
The unwrapped type of `Email` would be Text.
Beam has already made a FromField instance for Text,
so we're done.

```haskell
instance ( IsSql92ExpressionSyntax be
         , Wrapped a
         , HasSqlEqualityCheck be (Unwrapped a)
         ) =>
         HasSqlEqualityCheck be (DBFieldWrap a)

instance (Wrapped a, FromField (Unwrapped a)) => FromField (DBFieldWrap a) where
  fromField a b = review (_Wrapped' . _Wrapped') <$> fromField a b
```
The [review function](http://hackage.haskell.org/package/lens-4.17/docs/Control-Lens-Review.html#v:review)
just calls the constructor.
We have to call two `_Wrapped'`s with it because we need to put 
it in the `Email` or `Password`, and then we need to put it into
the `DBField`.
```haskell
instance (Wrapped a, HasSqlValueSyntax be (Unwrapped a)) =>
         HasSqlValueSyntax be (DBFieldWrap a) where
  sqlValueSyntax = sqlValueSyntax . view (_Wrapped' . _Wrapped')
```
The view function is just an alias for `^.`, a getter.
We get the result of wrapping twice.

```haskell
instance ( IsSql92ColumnSchemaSyntax be
         , Wrapped a
         , HasDefaultSqlDataTypeConstraints be (Unwrapped a)
         ) =>
         HasDefaultSqlDataTypeConstraints be (DBFieldWrap a)

instance ( IsSql92DataTypeSyntax be
         , Wrapped a
         , HasDefaultSqlDataType be (Unwrapped a)
         ) =>
         HasDefaultSqlDataType be (DBFieldWrap a) where
  defaultSqlDataType proxy =
    defaultSqlDataType $ view (_Wrapped' . _Wrapped') <$> proxy
```
Apparently you can
`fmap` into a proxy to get the right type out.
Even though a proxy has no data, it will change type.

This does exactly the same thing as the independent functions
did in case of the orphanage,
the only difference is that they wrap twice.
We essentially tell the type checker to look for the beam instance
two levels deeper.
We tell it by using restrictions on the instances (the stuff before `=>`).

Now we can insert our newtypes directly into database
without having to implement all those Beam instances:

```haskell
-- database
data UserT f = User 
	{ email :: C f (DBFieldWrap Email)
	, password :: C f (DBFieldWrap Password)
	}
	-- etc beam boilerplate...
```

Adding an additional newtype for the database is now
easy:

```haskell
-- common
newtype DateOfBirth = DateOfBirth { unEmail :: Day } deriving Generic
instance Wrapped DateOfBirth 

data UserT f = User 
	{ email :: C f (DBFieldWrap Email)
	, password :: C f (DBFieldWrap Password)
	, dob :: C f (DBFieldWrap DateOfBirth)
	}
```

Note that this technique doesn't just work for beam instances,
one could do the same for Aeson,
or any other library that requires many instances on newtypes.
The `Wrapped` instance can be re-used.

# Conclusion
So what did we gain?

- We no longer have orphans!
- Boilerplate has been reduced to just the wrapped instance.

What did we lose?

- Unfortunately we need to unwrap and wrap at the call sites (beam queries).
- To use this we need to depend on lens.
- The beam schema is a little bit more verbose.

Because all those negative points are really small,
I'm calling this a win.
The sources can be found on [github](https://github.com/jappeace/dbfield) and [hackage](https://hackage.haskell.org/package/beam-newtype-field).
