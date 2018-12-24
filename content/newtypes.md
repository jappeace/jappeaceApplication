Title: Lens into wrapped newTypes
Date: 2018-12-24 12:00
Category: focus
OPTIONS: toc:nil
Tags: haskell, programming, tools, lens
status: draft

Consider the following piece of code in our common folder of 
our fullstack webapp:

```haskell
data Login = Login
	{ email :: Text
	, password :: Text
	}
```

# Newtype
Looks good? No of course not, our common code is shared 
between both client and server, we should be incredibly pendatic
about the record field and wrap everything in newtypes so we don't
accidantly mix up our Text fields.
This would be a better representation:

```haskell
newtype Email = Email { unEmail :: Text }
newtype Password = Password { unPassword :: Text }

data Login = Login
	{ email :: Email 
	, password :: Password
	}
```

# Database
Ok so now we can put this login directly into our user table

```haskell
-- database
data UserT f = User 
	{ email :: C f Email
	, password :: C f Password
	}
	-- etc beam boilerplate...
```

Although this is what we want, it doesn't compile, we need
to tell beam how to get the right underlying type it
knows how to produce sql for:

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

We put these instances into an orphanage (dedicated file for orphan instances)
because we want to put the newtypes directly into the datbase.
However we don't want our common code to be dependend on beam,
that would mean our frontend javascript suddenly would pull
in a bunch of database related code for no reason.
We'll eleminate the need for these orphans later.

# Wrapped
That's a lot of boilerplate for every single newtype, just to get the
right to insert it into the database.
The instances themselve appear to do the same thing over and over,
they wrap or unwrap types to get the underlying interesting value.
In an attempt to at least make the logic shared we used the
Wrapped module from Control.Lens:

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

This does not look better.
However we now can clearly see that the wrapping is indeed
the same functionality as the instances all point toward the same
function.

# A general instance
Can't we make a generlized instance that does all of this wrapping
for all newtypes?
I attempted this but did got very far, I wasn't very sure what was
going on anymore with the type errors I got out of that.
But after a bit of searching I realized that what I attempted
to do was ridicioulus and dangerous,
I wanted to create an orphan instance, only restricted by the wrapped
constraint, that would magically allow everything to be put inside database.

Of course that wouldn't work, now all previous and future *Wrapped*
newtypes would have to be able to be fit into postgres or fail.

The solution of my boilerplate orphan instance problem was
introducing yet another newtype:

```haskell
-- wrapped.hs
newtype DBFieldWrap a = DBFieldWrap
  { _unField :: a
  } deriving (Generic, Show)

instance Wrapped a => Wrapped (DBFieldWrap a)

instance (Wrapped a, FromField (Unwrapped a)) =>
         FromBackendRow Postgres (DBFieldWrap a)

instance ( IsSql92ExpressionSyntax be
         , Wrapped a
         , HasSqlEqualityCheck be (Unwrapped a)
         ) =>
         HasSqlEqualityCheck be (DBFieldWrap a)

instance (Wrapped a, FromField (Unwrapped a)) => FromField (DBFieldWrap a) where
  fromField a b = review (_Wrapped' . _Wrapped') <$> fromField a b

instance (Wrapped a, HasSqlValueSyntax be (Unwrapped a)) =>
         HasSqlValueSyntax be (DBFieldWrap a) where
  sqlValueSyntax = sqlValueSyntax . view (_Wrapped' . _Wrapped')

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



Newtypes seem like a construct with little use.
The prime reason for using them is to increase typesafety,
If we have a 'phone number' and an 'email' value that both have 
type text, we'd better make a newtype for both of them so we don't
accidently mix them.
That is how I was personally introduced to them.

From an algoritmic perspective they're obviously useless.
Another algoritmicly useless construct is isomorphism.
An isomorphism is a change, that stays the same.
A different representation, but no information is lost or gained.

Newtypes are isomorphisms.
They can be changed into the newtype trough the constructor,
and unpacked again with pattern matching on te constructor.

