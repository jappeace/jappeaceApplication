Title: Type level translations
Date: 2019-07-03 20:30
Category: tools
OPTIONS: toc:nil
Tags: translate, haskell, 
status: draft

People claim langauge is a mere value, nonsense I say!
Obviously langauge is a type, sentences are types,
and their translations are mere inhabitations of said types.
In other words, if you don't translate,
your program won't compile [^extract].
This is very usefull for my startup idea,
and it also allows me to share components while not having
to share my entire langauge sumtype.
Anyway this is the concrete api I want:

[^extract]:
	Program messages as types can be extracted
	from said program, and thus converted to something like gettext format.
	Then you could use TH to implement them.
	I have no such requirement so I won't.
	This is much better than the [i18n](http://hackage.haskell.org/package/i18n-0.4.0.0/docs/Data-Text-I18n.html)
	package which weaves a monad
	to the entire program just to collect messages that are known at compile time.

```haskell

sent :: Text.Text
sent = _t @"hello world" @English -- hello world

sent_nl :: Text.Text
sent_nl = _t @"hello world" @Dutch -- hallo wereld

sent_cn :: Text.Text
sent_cn = _t @"hello world" @Chinese -- 你好，世界

```

I'm using datakinds, neatly explained [here](http://ponies.io/posts/2014-07-30-typelits.html).


This is faster and more type safe:

```haskell
data Variable (l :: Symbol) val = Var val
data Literal (l :: Symbol) = Lit

class Language lang lit where
  _i18n :: lang -> Literal lit -> Text.Text

data English = English

-- english is default
instance KnownSymbol lit => Language English lit where
  _i18n _ = Text.pack . symbolVal

data Dutch = Dutch
instance Language Dutch "This date cannot be parsed" where
  _i18n _ _ = "Deze tijd kan niet worden verwerkt"

f :: Text.Text
f = _i18n Dutch $ Lit @"This date cannot be parsed"

f3 :: Text.Text
f3 = _i18n Dutch $ Lit @"This date couldn't be parsed"

f2 :: Text.Text
f2 = _i18n English $ Lit @"This date cannot be parsed"
```

Gives the error:

```
src/Raster/Common/Lang/Types.hs:30:6: error:
    • No instance for (Language Dutch "This date couldn't be parsed")
        arising from a use of ‘_i18n’
    • In the expression: _i18n Dutch
      In the expression:
        _i18n Dutch $ Lit @"This date couldn't be parsed"
      In an equation for ‘f3’:
          f3 = _i18n Dutch $ Lit @"This date couldn't be parsed"
   |
30 | f3 = _i18n Dutch $ Lit @"This date couldn't be parsed"

```


Okay so what if we want to change the return type?
We need to do this to allow variables to be inserted 
in the types we define,
eg we say for example. "Current time %time o clock",
where we replace %time with the current time.

Enter type families:

```haskell
class Language lang lit where
  type Result lang lit
  _i18n :: lang -> Literal lit -> Result lang lit

data English = English

-- english is default
instance KnownSymbol lit => Language English lit where
  type Result English lit = Text.Text
  _i18n _ = Text.pack . symbolVal

data Dutch = Dutch
instance Language Dutch "This date cannot be parsed" where
  type Result Dutch "This date cannot be parsed" = Text.Text
  _i18n _ _ = "Deze tijd kan niet worden verwerkt"

instance Language Dutch "Hello world" where
  type Result Dutch "Hello world" = Int
  _i18n _ _ = 3
f :: Text.Text
f = _i18n Dutch $ Lit @"This date cannot be parsed"

f3 :: Int
f3 = _i18n Dutch $ Lit @"Hello world"

f2 :: Text.Text
f2 = _i18n English $ Lit @"This date cannot be parsed"
```

This encoding introduces a lot of boilerplate,
but it's still not even powerfull enough.
We still need to find a way to insert
variables into our type.

```haskell
one = _i18n "I'm walking to " :<> Var String :<> ", to get some bread" $ "bakery"
two = _i18n "I'm walking to " :<> Var String :<> ", to get some bread" $ "supermarket"
```
In otherwords, the _i18n function figures out we need to
return a function.

Servant does [this](http://hackage.haskell.org/package/servant-0.16.0.1/docs/Servant-API.html#t:HasLink).
I just copy whatever spells servant does
and put it in a different context.


# Type level printf
I feel this is the 'killer' idea, combining
this type level magic with printf capabilites.
The issue is that the [symbols](http://hackage.haskell.org/package/symbols)
package only does ascii.
And if you want 

My little api has problems.
It's a bit of a pain to add variables with Var.
To make things simpler I'm going to combine it with typelevel
printf functionality.

This will also allow us to 'move' te variable in the translation.
Here is a full example in idris:
https://www.youtube.com/watch?v=fVBck2Zngjo&feature=youtu.be

We don't have to do the final part of lifting a value into types.
But we can do the application of functions on types, which
has already been worked out by:
https://kcsongor.github.io/symbol-parsing-haskell/
https://github.com/kcsongor/symbols/blob/master/src/Data/Symbol/Examples/Printf.hs

So the final part is combining this with tranlsation.


For making variables:
http://hackage.haskell.org/package/type-level-sets


Honarary mention to geekforgeeks for helping me figure out how to
build a balanced tree from a sorted [list](https://www.geeksforgeeks.org/sorted-linked-list-to-balanced-bst/)
(Idk why I had so much trouble with this,
it seems like a school excersize but for
some reason I kept thinking about fibonaci and natural
growth when studying the original structure, I think
it's possible to do this with natural growth (e) and a fold,
I just can't quite put my finger on how.
). (Dunno why they muck around with a linked list though,
it seems way faster with a vector, I believe my implementation is just
linear time.
)


# printf safe
The printf safe package has a api taht doesn't do the symbol interpertation
and therefore works with utf8,
so I attempted to use that.

What I wanted was a type family kindoff like this:

```haskell
type family Msg lang (a :: [Format]) :: [Format]
type instance Msg English a = a
type instance Msg Dutch ("okay" % "") = "ok" % ""
type instance Msg Dutch ("Give me " % Int $ "pizzas") = "geef me " % Int % "pizzas"
```

This alows the moving of the types in the strings.
For example:
```haskell
type instance Msg Dutch ("Give me " % Int $ "pizzas") = "geef me zoveel pizzas: " % Int 
```
I'd have to put a constraint in place to ensure the resulting function would get the same positional order in the
arguments,
otherwise using a different langauge could cause compile errors.
Or get rid of positional application all together somehow, type level sets? I hear purescript does that sortoff.


However I ran into a snag as the compoiler doesn't allow type family applications within
type [families](https://gitlab.haskell.org/ghc/ghc/issues/3485).
Giving me this error:

```
src/Raster/Common/Lang/Tdsl.hs:114:15: error:
    • Illegal type synonym family application in instance: "okay" % ""
    • In the type instance declaration for ‘Msg'’
    |
114 | type instance Msg' Dutch ("okay" % "") = "ok" % ""
    |               ^^^^
```

I'm pretty sure GHC is just being to strict here (or it may not be implmented).


# The easy way out

Okay I'll settle for the original idea but I'll 'borrow' the type [safe printf api](http://hackage.haskell.org/package/printf-safe-0.1.0.1/docs/Text-Printf-Safe.html).
It's pretty much what I want.

So we construct the function first, and then do translation later.
This means our type class will push a langauge argument down untill it encounters 
symbols, individual symbols need to be translated
which is unfurtonate because now you'll get these little
translation chuncks,
it's very re-usable but not in the good kind
because often you need to change these little parts per langauge depending on context.
However, this api is significantly better than what I'm doing now,
and I believe better than anything out there.
Just because there is practically 0 cost to doing inernationalization this way.
