Title: Lens into wrapped newtypes
Date: 2018-12-24 12:00
Category: technique
OPTIONS: toc:nil
Tags: haskell, programming, tools, lens

![Categorical representation of the NT iso](/images/2018/nt-iso.svg)

> All newtypes are isomorphisms
> 
> <br />
>
> -- My mother

[Control.Lens.Wrapped](http://hackage.haskell.org/package/lens-4.17/docs/Control-Lens-Wrapped.html)
uses this property to introduce a typeclass `Wrapped`.
Let's explore usecases, because after all, it doesn't appear to do much at first glance.
What's the point of formalizing wraping and unwrapping of types?

In this blog post we'll explore a method of reducing instance boilerplate for newtypes.
In my particular use case it'll also eleminate orphan instances.
I believe that this technique will make using newtypes more attractive.

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
therefore we should be pendatic about these record field.
We wrap common occurences such as Text in newtypes
so we don't accidantly mix up the fields.
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
We won't be able to mix an email or password anymore on login,
all the way from frontend input to backend the backend.
This is analogous to having integration tests on user input fields,
ajax calls, http endpoints, and database insertion.
That's a lot of safety for two extra lines of newtypes,
therefore we accept this trade and move on.

# Database
Ok so now we can put the fields of login directly into our
user table:

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
-- orphanage

instance HasSqlEqualityCheck PgExpressionSyntax Email
```
This instacne allows us to use the [`==.`](http://hackage.haskell.org/package/beam-core-0.7.2.2/docs/Database-Beam-Query.html#v:-61--61-.)
operator on beam expressions directly.
We can now compare the column email with a client email.
Don't know why an instance is needed for this, but the
wanted it whenever I used that operator.
```haskell
instance HasSqlValueSyntax PgValueSyntax Email where
  sqlValueSyntax = sqlValueSyntax . unEmail
```

here we use unEmail to get rid of the newtype, to use the underlying's type 
sqlValuesyntax.

```haskell
instance FromBackendRow Postgres Email
```
I believe this tells beam we want to be able to use a postgress database
on email.
We dont' have to instantiate sqllite instances
(beam can do multiple backends).

```haskell
instance FromField Email where
  fromField a b = Email <$> fromField a b
```
[FromField](https://hackage.haskell.org/package/postgresql-simple-0.5.4.0/docs/Database-PostgreSQL-Simple-FromField.html#t:FromField)
is a typeclass from PgSimple.
Here, we're telling the compiler to just use the from field from the underlying type,
and once it's done we can wrap it back into an email.
```haskell
instance HasDefaultSqlDataTypeConstraints PgColumnSchemaSyntax Email
```
This is a constraint comming from beam migrate,
if you want to have automatic schema generation,
or be able to step trough various schemas you need this.
(I want to blog about this too,
but I'm unhappy with how it currently works,
so I'm holding of on that untill I find time to fix it).
```haskell
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
because I want to put the newtypes directly into the datbase.
However I don't want our common code to be dependend on beam,
that would mean the frontend javascript suddenly would pull
in beam as a dependency for no reason.
We'll eleminate the need for these orphans later.

What did we gain?
The ability to put these newtypes in the database,
what did we lose?
Well we now have a lot of extra boilerplate to content with.

# Wrapped
Let's kill the boilerplate!

If you can provide an [Iso'](http://hackage.haskell.org/package/lens-4.17/docs/Control-Lens-Iso.html#t:Iso-39-),
then you can give an instance for Wrapped type class.
In our case, if a newtype has derived generic we get an instance for free.
But to refresh our memories let's eximanie how to get an Iso:

```haskell
iso :: (s -> a) -> (b -> t) -> Iso s t a b 
```
That looks complicated, luckeliy the structure we want is `Iso'`,
so we can ignore that one and use the simpler version
(which I just invented by doing logical substition):
```haskell
Iso' = Iso s s a a
emailIso :: Iso' Email Text
```
Which we can define for Email:
```haskell
emailIso = iso unEmail Email 
```
So we're creating a function that can do both wrapping and unwrapping.
Indeed the withIso function provides a mechanism to get access to both functions.

The instances themselve do the same thing over and over,
they wrap or unwrap types to get the underlying interesting value.
Just like the pretty picture I put on top of this post.
Here we start using the wrapped module,
just to share the logic:
```haskell
-- common

newtype Email = Email { unEmail :: Text } deriving Generic
newtype Password = Password { unPassword :: Text } deriving Generic

instance Wrapped Email
instance Wrapped Password
```
This implements the Wrapped instance, this is possible because we
derived generic.

```haskell
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

-- backend orphanage
wrappedSqlValueSyntax  :: (Wrapped a, HasSqlValueSyntax b (Unwrapped a)) => a -> b
wrappedSqlValueSyntax  = sqlValueSyntax . view _Wrapped'

fromWrappedField :: (Wrapped a, FromField (Unwrapped a)) => FieldParser a
fromWrappedField a b = review _Wrapped' <$> fromField a b

wrappedDefaultSqlDataType :: (Wrapped a, HasDefaultSqlDataType b (Unwrapped a)) => Proxy a -> Bool -> b
wrappedDefaultSqlDataType proxy = defaultSqlDataType $ view _Wrapped' <$> proxy
```

At first glance, this does not look better.
However we now can clearly see that the wrapping is indeed
the same functionality as the instances all point toward the same
functions.
The FromField instance for Password is implemented with fromWrappedField,
so does the Email instance.
This is possible because both Email and Password have instantiated the
Wrapped instance.

This change is a lot better if you consider that there is no more
logic being repeated here.
The boilerplate is now in it's purest form.
By itself I wouldn't consider the current state to be that bad anymore.
However, these are still orphans,
which can cause [bad problems](https://stackoverflow.com/questions/3079537/orphaned-instances-in-haskell).
We should kill all orphans!

# A general instance
Can't we make a generlized instance that does all of this wrapping
for all newtypes?
My first attempt was rather crazy looking back.
I wanted to create the ultimate orphan.
a polymorphic instance that was kept in check by constraints
such as wrapped and the fact underlying types would have
these beam instances implemented.
I attempted this but did got very far, I wasn't very sure what was
going on anymore with the type errors I got out of that.
But after a bit of searching I realized that what I attempted
to do was ridicioulus and dangerous.
Of course that wouldn't work, now all previous and future *Wrapped*
newtypes would have to be able to be fit into postgres or fail.
This piece of code would break all existing libraries that would've
had a *Wrapped* newtype.
No this was an absurd idea.

Rather than solving the problem for all newtypes, 
I stepped back, and added yet another newtype:
```haskell
-- wrapped.hs
newtype DBFieldWrap a = DBFieldWrap
  { _unField :: a
  } deriving (Generic, Show)

instance Wrapped a => Wrapped (DBFieldWrap a)

instance (Wrapped a, FromField (Unwrapped a)) =>
         FromBackendRow Postgres (DBFieldWrap a)
```
This instance shows the core idea.
We add the wrapped restriction on `a`,
which allows us to speak about the unwrapped form of a.
The unwraped type of `Email` would be Text.
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
the DBField (in this case I'd rather not but I'm bound to the
definitions of the typeclass).
```haskell
instance (Wrapped a, HasSqlValueSyntax be (Unwrapped a)) =>
         HasSqlValueSyntax be (DBFieldWrap a) where
  sqlValueSyntax = sqlValueSyntax . view (_Wrapped' . _Wrapped')
```
The view function is just an alias for `^.`, eg a getter.
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
Right so this final one is interesting, appearantly you can
fmap into a proxy to get the right type out.
Even though a proxy is nothing.

This does excatly the same thing as the independent functions
did in case of the orphanage,
the only difference is that they wrap twice.
We essentially tell the type checker to look for the beam instance
two levels deeper.
This can be done because we have a 
restriction on this newtype is that the underlying type
has to implement wrapped.

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

So what did we gain?
+ We no longer have orphans!
+ Boilerplate has been reduced to just the wrapped instance,
What did we lose?
+ Unfortunatily we need to unwrap and wrap at the callsites.
+ Beam schema is yet bit more ugly.

I'm calling this a win.
The sources can be found on [github](https://github.com/jappeace/dbfield) and [hackage](https://hackage.haskell.org/package/beam-newtype-field).

# Conclusion
Newtypes seem like a construct with little use.
The prime reason for using them is to increase typesafety,
If we have a 'phone number' and an 'email' value that both have 
type text, we'd better make a newtype for both of them so we don't
accidently mix them.
That is how I was personally introduced to them.

I've never gotten excited about Isomorphisms either, 
so having these two dubious features interect and such
a way where they create so much expressive value
is amazing:
We killed a bunch of orphans,
We significantly reduced boilerplate
and we did so by grasping rather simple tools.
