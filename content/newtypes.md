Title: Lens into wrapped newTypes
Date: 2018-12-24 12:00
Category: focus
OPTIONS: toc:nil
Tags: haskell, programming, tools, lens
status: draft

![Categorical representation of the NT iso](/images/2018/nt-iso.svg)

> All newtypes are isomorphisms
> 
> - My mother

Any newtype can change into it's underlying
representation and back into it's self no problem.
We just need the constructor.

There is a package in the lens library, [Control.Lens.Wrapped](http://hackage.haskell.org/package/lens-4.17/docs/Control-Lens-Wrapped.html)
which uses this fact to introduce a typeclass wrapped.
If you're newtype has derived generic we get an instance for free.
Let's start using it and explore it's usefullness, because after all,
it doesn't appear to do much at first glance.

# Newtype
Consider the following code in our common folder of 
a [fullstack haskell webapp]({filename}/fullstack-haskell-reflex-servant.md):

```haskell
data Login = Login
	{ email :: Text
	, password :: Text
	}
```

Looks good? No of course not.
This common code is shared 
between both client and server, therefore we should be pendatic
about these record field.
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
but these few lines add a lot of safety.
So we accept this trade and move on.

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
instance HasSqlValueSyntax PgValueSyntax Email where
  sqlValueSyntax = sqlValueSyntax . unEmail
instance FromBackendRow Postgres Email
instance FromField Email where
  fromField a b = Email <$> fromField a b
instance HasDefaultSqlDataTypeConstraints PgColumnSchemaSyntax Email
instance HasDefaultSqlDataType PgDataTypeSyntax Email where
  defaultSqlDataType proxy = defaultSqlDataType $ unEmail <$> proxy

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

We put these instances into an orphanage
(dedicated file for orphan instances)
because we want to put the newtypes directly into the datbase.
However we don't want our common code to be dependend on beam,
that would mean our frontend javascript suddenly would pull
in a bunch of database related code for no reason.
We'll eleminate the need for these orphans later.

What did we gain?
The ability to put these newtypes in the database,
what did we lose?
Well we now have a lot of extra boilerplate to content with.

# Wrapped
Let's kill the boilerplate!
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

-- backend orphanage
wrappedSqlValueSyntax  :: (Wrapped a, HasSqlValueSyntax b (Unwrapped a)) => a -> b
wrappedSqlValueSyntax  = sqlValueSyntax . view _Wrapped'

fromWrappedField :: (Wrapped a, FromField (Unwrapped a)) => FieldParser a
fromWrappedField a b = review _Wrapped' <$> fromField a b

wrappedDefaultSqlDataType :: (Wrapped a, HasDefaultSqlDataType b (Unwrapped a)) => Proxy a -> Bool -> b
wrappedDefaultSqlDataType proxy = defaultSqlDataType $ view _Wrapped' <$> proxy

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

At first glance, this does not look better.
However we now can clearly see that the wrapping is indeed
the same functionality as the instances all point toward the same
function.

This change is a lot better if you consider that there is no more
logic being repeated here.
The boilerplate is now in it's purest form.
By itself I wouldn't consider this to be so bad anymore, 
but remember, these are still orphans.
Which can cause [bad problems](https://stackoverflow.com/questions/3079537/orphaned-instances-in-haskell).
We should strife towards death to orphans!

# A general instance
Can't we make a generlized instance that does all of this wrapping
for all newtypes?

Yes, we can! Depending on perspective.
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
This isntance shows the core idea, we add the wrapped
restriction on `a`, which allows us to speak about the
unwrapped form of a, eg, the actuall underlying type 
of for example `Email`, which is Text.
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
