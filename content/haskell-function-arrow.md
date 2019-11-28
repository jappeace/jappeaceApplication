TITLE: Haskell function arrow
DATE: 2019-10-17 21:30
CATEGORY: tools
Tags: build-tools, haskell
OPTIONS: toc:nil
Status: draft

>  "function type constructor", and while it does have some special syntax,
>   there's not that much special about it.
>
>   \- hammar on stackoverflow


Not being special has a bunch of interesting implications:

1. We can instantiate typclasses for it.
2. The functions in said typeclasses wil also implement these typeclasses themselves.
3. There are more of those typeclasses then there are functions.

# Syntax
The fact that functions are not special is very confusing for the beginner
haskeller.
In every other langauge we make a big deal out of functions.
For example javascript:

```javascript
function add(x){
    return x+1;
}
```

I count at least a couple pieces of syntax here, the function keywords,
the paranthesis for arguments, the naming part, the brackets for
code blocks and finally the return keyword.

Ironically the functional programming langauge haskell,
has little concern for functions.
In haskell you'd do:
```haskell
add :: Int -> Int
add = 1 +
```

Which can be rewritten into:
```haskell
add :: (->) Int Int
add = 1 +
```
The syntax I'm counting is naming the function,
that equal thing, and indicating the type[^canleaveout].
Javascript makes a much bigger deal out of functions than
haskell by mere 'piece of syntax count'
(although one may argue this is done a bit aribtrarly).

[^canleaveout]: We could even leave out the type signature, but I think part of writing
              haskell is writing type signatures.

The other pieces are reasuable.
Function arrow it'self is using infix by default,
but so is the + operation, and that's user definable.
Type operators is a [langauge extension](https://ocharles.org.uk/posts/2014-12-08-type-operators.html),
and are used in the wild with
[servant](https://jappieklooster.nl/pragmatic-haskell-simple-servant-web-server.html#a-minimal-servant)
for example.
Function arrow just get's special treatment in that it doesn't need that extension.

Even composition, the thing we try to strive for in haskell!
It's just a function elequantly defined by:
```haskell
(<|) :: ((->) ((->) b c) ((->) ((->) a b) ((->) a c)))
(<|) = (.)
```
I also simplified the type signature a bit so everyone can follow.
Besdes, I think elm programmers prefer the triangle.


# Instances
Function become even more crazy once you start looking into the instances.
It's pretty hard to find them because there is no
hackage page for `(->)` listing them all.
Google is of no help either because it doesn't think
functions are special, just like the langague.

## Functor

## Applicative

## Monad

## Reader
```haskell
add :: Reader Int Int
add = 1 +
```
In fact it's the same [thing](http://hackage.haskell.org/package/mtl-2.2.2/docs/Control-Monad-Reader.html).
This is exploited by the lens library
for the [view](http://hackage.haskell.org/package/lens-4.18.1/docs/Control-Lens-Getter.html#v:view)
function.

# Resources

https://stackoverflow.com/questions/9136421/where-can-i-read-up-on-the-haskell-operator

https://www.quora.com/What-is-in-Haskell-How-can-this-be-a-functor-and-a-monad-What-does-it-actually-do-and-mean
