TITLE: The Haskell 20AI Language Report
DATE: 2026-05-25 12:00
CATEGORY: haskell
Tags: haskell, GHC2024, language-report, AI
OPTIONS: toc:nil

The Haskell 2010 report is showing its age.
GHC2024 ships extensions that Haskell 2010 never dreamed of.
The committee process hasn't produced a Haskell 2020.
So I did what any reasonable person would do:
I wrote the Haskell 20AI Language Report myself,
assisted by a mass of silicon hallucinating about lazy evaluation.

This is the authoritative\* reference for Modern Haskell
as spoken by humans and large language models alike in the year 20AI.

\*not authoritative at all

## Chapter 1: Introduction

### 1.1 Program Structure

A Haskell program is a collection of *modules* (Chapter 5).
The module `Main` must export `main :: IO ()`,
unless you are writing a library,
in which case you instead export a type class
that nobody can figure out how to instantiate
and a Template Haskell splice that does it for them.

In Haskell 20AI, every `.hs` file begins with:

```haskell
{-# LANGUAGE GHC2024 #-}
```

This single pragma replaces the 30-line extension block
that your project accumulated between 2016 and 2023.
If you still maintain a per-file extension list longer than 5 lines,
the compiler is entitled to emit a warning
of severity `judgmental`.

### 1.2 The Haskell Kernel

Haskell is built on a small kernel
into which all surface syntax desugars.
That kernel is System FC,
and you are not expected to understand it.
Even people who work on GHC do not fully understand it.
This is fine.
Haskell has always been a language
where the type checker knows more than you do.

### 1.3 Values and Types

Every value in Haskell has a type.
In Haskell 20AI, thanks to `DataKinds`,
every type also has a kind,
every kind also has a sort,
and at some point you run out of words
and GHC silently unifies everything with `Type`.

```haskell
type family ProgrammerConfidence (n :: Nat) :: Type where
  ProgrammerConfidence 0 = Panic
  ProgrammerConfidence 1 = UnsafeCoerce
  ProgrammerConfidence n = GADTWithTooManyParameters n
```

### 1.4 Namespaces

Haskell has two namespaces: values and types.
GHC2024 enables `ExplicitNamespaces`,
so you can now write `import M (type T, pattern P)`.
This is useful when your type and your constructor
have the same name,
which happens roughly 100% of the time
in any codebase that uses `newtype`.

## Chapter 2: Lexical Structure

### 2.1 Layout

The off-side rule determines block structure
using indentation.
This is superior to braces,
except when you paste code into a chat window
and the LLM strips all your leading whitespace.
In Haskell 20AI, the specification notes that
tools that interact with Haskell source
MUST preserve indentation,
on pain of producing programs that mean something
entirely different from what was intended.

### 2.2 Identifiers

Identifiers begin with a lowercase letter.
Type names begin with an uppercase letter.
Operators are sequences of symbol characters.
This is unchanged from Haskell 2010.

What *is* new is the convention
that any identifier longer than 30 characters
is a sign that you are writing Java
and should reconsider your life choices.

### 2.3 Comments

```haskell
-- This is a line comment.
{- This is a block comment. -}
-- | This is a Haddock comment, and you should write more of them.
```

The Haskell 20AI report officially recognizes a fourth kind:

```haskell
-- TODO: fix this later
```

This comment is permanent.
It will outlive you, your project, and the heat death of the universe.

## Chapter 3: Expressions

### 3.1 Lambda Abstractions

Haskell 2010 gives us:

```haskell
\x -> x + 1
```

GHC2024 enables `LambdaCase`, which gives us:

```haskell
\case
  Nothing -> "gone"
  Just x  -> "here: " <> show x
```

And its younger sibling `\cases` for multi-argument matching:

```haskell
\cases
  True  True  -> "both"
  True  False -> "left"
  False True  -> "right"
  False False -> "neither"
```

This replaces the ancient pattern
of writing a `where`-bound helper function
named `go` or `helper` or `f'` or `theThing`.
Those names may now be retired with full honors.

### 3.2 Case Expressions

Case expressions in Haskell 20AI work exactly as in Haskell 2010,
with the added note that the Haskell 20AI report
**strongly discourages** wildcard patterns.
Write out all your cases.
Yes, all of them.
The compiler warns you when you miss one.
That warning is a *gift*.

```haskell
-- Bad (Haskell 2010 vibes):
describeBool :: Bool -> String
describeBool True = "yes"
describeBool _    = "no"

-- Good (Haskell 20AI vibes):
describeBool :: Bool -> String
describeBool True  = "yes"
describeBool False = "no"
```

### 3.3 Do Notation

Do notation is syntactic sugar for monadic bind.
This hasn't changed.
What has changed is that in 20AI,
beginners learn `do` notation first
and only discover `>>=` exists
when they read a blog post from 2013
titled "Monads are just monoids in the category of endofunctors,
what's the problem?"

### 3.4 Type Applications

GHC2024 includes `TypeApplications`,
allowing you to explicitly specify type arguments:

```haskell
show @Int 42
read @Double "3.14"
mempty @[Int]
```

This is enormously useful for resolving ambiguity,
and also for writing code that looks like
you are scolding the type at it: `show @Int`.

## Chapter 4: Declarations and Bindings

### 4.1 Type Signatures

Every top-level binding MUST have a type signature.
This is not technically enforced by the language,
but it is enforced by code review,
by `-Wall`, and by the Haskell 20AI report.

```haskell
-- The compiler can infer this type.
-- You should write it anyway.
-- Your future self will thank you.
-- Your coworkers will thank you.
-- The LLM reading your code will thank you.
addOne :: Int -> Int
addOne x = x + 1
```

### 4.2 GADTs

GHC2024 enables `GADTs`,
the most important extension in the history of type systems
that you can actually use in production code.

```haskell
data Expr (a :: Type) where
  LitInt  :: Int -> Expr Int
  LitBool :: Bool -> Expr Bool
  Add     :: Expr Int -> Expr Int -> Expr Int
  If      :: Expr Bool -> Expr a -> Expr a -> Expr a
```

With GADTs, the type checker becomes your co-pilot.
It knows things. It will tell you things.
You just have to listen.

Note that `GADTs` implies `MonoLocalBinds`,
which means let-bound variables
may not be as polymorphic as you expect.
This is the price of power.
The Haskell 20AI report considers it fair.

### 4.3 Deriving Strategies

GHC2024 enables `DerivingStrategies`.
You must now say *how* you want your instances derived:

```haskell
data User = User
  { userName :: Text
  , userAge  :: Int
  }
  deriving stock (Show, Eq, Ord)
  deriving newtype (FromJSON)     -- only if it's a newtype
  deriving anyclass (Hashable)
  deriving (ToJSON) via (GenericJSON User)
```

This replaces the old system
where GHC would pick a strategy for you
using an algorithm that was technically deterministic
but felt like a coin flip.

### 4.4 Kind Signatures

`StandaloneKindSignatures` lets you write:

```haskell
type Proxy :: Type -> Type
data Proxy a = Proxy
```

And with `DataKinds`, types themselves get promoted to kinds:

```haskell
type family Append (xs :: [a]) (ys :: [a]) :: [a] where
  Append '[]       ys = ys
  Append (x ': xs) ys = x ': Append xs ys
```

If you find yourself writing kind signatures for your kind signatures,
you have gone too far.
Step away from the terminal.
Touch grass.
Then come back and keep going,
because honestly that's pretty cool.

### 4.5 Role Annotations

GHC2024 enables `RoleAnnotations`,
which let you control
when `coerce` is allowed on your types:

```haskell
type role Set nominal
```

This prevents someone from coercing `Set Name` to `Set Email`
and wondering why the ordering is wrong.
The Haskell 20AI report considers `RoleAnnotations`
a fundamental safety mechanism
and wonders why it took this long
to put it in a language edition.

## Chapter 5: Modules

### 5.1 Module Structure

```haskell
module MyModule
  ( -- * Types
    MyType(..)
    -- * Functions
  , doThing
  , doOtherThing
  ) where
```

The export list is optional.
Omitting it exports everything.
This is almost never what you want.
The Haskell 20AI report recommends
always writing an explicit export list,
so that your module's API
is a deliberate choice
rather than a terrifying accident.

### 5.2 Import Declarations

GHC2024 inherits `ImportQualifiedPost` from GHC2021:

```haskell
-- Haskell 2010:
import qualified Data.Map as Map

-- Haskell 20AI:
import Data.Map qualified as Map
```

The `qualified` keyword moves after the module name.
This reads more naturally
and looks better when you have a long import list.
There is no technical difference.
The war about which style is better
will continue until the heat death of the universe.

### 5.3 Disambiguating Record Fields

GHC2024 enables `DisambiguateRecordFields`.
If two modules export a field called `name`,
the compiler can figure out which one you mean
from context:

```haskell
import Module.User (User(..))
import Module.Company (Company(..))

-- GHC knows which 'name' you mean:
getUserName :: User -> Text
getUserName user = name user
```

This is a small quality of life improvement
that eliminates a large amount of frustration.

## Chapter 6: Predefined Types and Classes

The Prelude remains largely unchanged from Haskell 2010.
`String` is still `[Char]`.
Everyone still uses `Text` instead.
The Haskell 20AI report acknowledges this situation
and officially shrugs.

The numeric tower remains.
`fromIntegral` is still everywhere.
The report recommends explicit conversion functions:

```haskell
-- Know what you're converting.
-- Name it.
-- Don't let fromIntegral hide a lossy conversion.
intToWord8 :: Int -> Word8
intToWord8 = fromIntegral  -- at least it's documented now
```

## Chapter 7: Basic Input/Output

`IO` is still the monad for side effects.
`putStrLn` still prints to stdout.
`getLine` still reads from stdin.

What has changed is that in 20AI,
the overwhelming majority of I/O
is done through some effect system or MTL stack.
But when you trace the call chain all the way down,
it's `IO` at the bottom.
It's always `IO` at the bottom.

```haskell
main :: IO ()
main = do
  putStrLn "Hello from Haskell 20AI"
  putStrLn "The year is uncertain"
  putStrLn "But the types are strong"
```

## Chapter 8: Foreign Function Interface

The FFI is unchanged from Haskell 2010.
You can still call C functions.
You can still be called from C.
`ForeignFunctionInterface` is enabled in GHC2024.

The Haskell 20AI report notes
that in practice,
most "foreign" calls these days
are HTTP requests to REST APIs,
which is technically a foreign function interface
if you squint hard enough.

## Chapter 9: The Complete GHC2024 Extension Set

For reference, `GHC2024` enables the following extensions.
Extensions **new** compared to GHC2021 are marked with a star:

| Extension | New in GHC2024 |
|---|---|
| BangPatterns | |
| BinaryLiterals | |
| ConstrainedClassMethods | |
| ConstraintKinds | |
| **DataKinds** | yes |
| DeriveDataTypeable | |
| DeriveFoldable | |
| DeriveFunctor | |
| DeriveGeneric | |
| DeriveLift | |
| DeriveTraversable | |
| **DerivingStrategies** | yes |
| **DisambiguateRecordFields** | yes |
| DoAndIfThenElse | |
| EmptyCase | |
| EmptyDataDecls | |
| EmptyDataDeriving | |
| ExistentialQuantification | |
| ExplicitForAll | |
| **ExplicitNamespaces** | yes |
| FieldSelectors | |
| FlexibleContexts | |
| FlexibleInstances | |
| ForeignFunctionInterface | |
| **GADTs** | yes |
| GADTSyntax | |
| GeneralisedNewtypeDeriving | |
| HexFloatLiterals | |
| ImplicitPrelude | |
| ImportQualifiedPost | |
| InstanceSigs | |
| KindSignatures | |
| **LambdaCase** | yes |
| **MonoLocalBinds** | yes |
| MonomorphismRestriction | |
| MultiParamTypeClasses | |
| NamedFieldPuns | |
| NamedWildCards | |
| NumericUnderscores | |
| PatternGuards | |
| PolyKinds | |
| PostfixOperators | |
| RankNTypes | |
| RelaxedPolyRec | |
| **RoleAnnotations** | yes |
| ScopedTypeVariables | |
| StandaloneDeriving | |
| StandaloneKindSignatures | |
| StarIsType | |
| TraditionalRecordSyntax | |
| TupleSections | |
| TypeApplications | |
| TypeOperators | |
| TypeSynonymInstances | |

That is 48 extensions, enabled with one pragma.
The Haskell 20AI report considers this a triumph
of engineering over bureaucracy.

## Chapter 10: Extensions NOT in GHC2024 (But You'll Want Anyway)

The following extensions are commonly used
but did not make the GHC2024 cut.
The Haskell 20AI report documents them here
for the sake of honesty:

- `OverloadedStrings` -- because `String` is `[Char]` and you know it
- `OverloadedRecordDot` -- `user.name` instead of `name user`
- `DuplicateRecordFields` -- every type has a `name` field
- `TypeFamilies` -- you were going to enable it anyway
- `MultiWayIf` -- like guards but inline
- `BlockArguments` -- `when condition do` instead of `when condition $ do`
- `RecordWildCards` -- controversial, but useful

These were omitted from GHC2024 either because
they change the meaning of existing code,
or because the committee had strong opinions,
or because nobody could agree
and tabling the issue was the only unanimous vote.

## Chapter 11: Specification of Derived Instances

Deriving instances works as in Haskell 2010,
extended by `DerivingStrategies` (section 4.3).
The compiler can derive `Eq`, `Ord`, `Show`, `Read`,
`Enum`, `Bounded`, `Ix`, `Functor`, `Foldable`,
`Traversable`, `Data`, `Typeable`, `Generic`, and `Lift`.

For anything else, you write it by hand
or use `anyclass` deriving
or `DerivingVia`.
`DerivingVia` is not in GHC2024
but the Haskell 20AI report believes in your ability
to add one more pragma to your file.

## Chapter 12: Compiler Pragmas

```haskell
{-# LANGUAGE GHC2024 #-}           -- the only one you need
{-# OPTIONS_GHC -Wall #-}          -- catch warnings
{-# OPTIONS_GHC -Wno-unused-do-bind #-}  -- if you must
{-# INLINE myFunction #-}          -- for performance
{-# SPECIALIZE myFunction :: Int -> Int #-}  -- for more performance
```

The Haskell 20AI report recommends `-Wall` in every module
and `-Werror` in CI.
Warnings are the compiler trying to help you.
Let it help you.

## Appendix A: A Brief History of Haskell Language Editions

| Year | Edition | Extensions | Mood |
|---|---|---|---|
| 1998 | Haskell 98 | 0 | Optimistic |
| 2010 | Haskell 2010 | 1 (FFI) | Cautious |
| 2021 | GHC2021 | 40 | Ambitious |
| 2024 | GHC2024 | 48 | Let's Go |
| 20AI | Haskell 20AI | 48 + vibes | Inevitable |

## Appendix B: How to Upgrade

1. Replace your extension block with `{-# LANGUAGE GHC2024 #-}`
2. Set `default-language: GHC2024` in your `.cabal` file
3. Run `cabal build`
4. Fix the three warnings about `MonoLocalBinds` changing inference
5. Marvel at how little broke
6. Wonder why you didn't do this sooner

## Appendix C: Acknowledgments

The Haskell 20AI Language Report was written
by a human and an AI
who both think lazy evaluation is a good idea.

This report is not affiliated with
the Haskell Foundation, the GHC Steering Committee,
or any institution that takes itself seriously.
It is, however, affiliated with
the eternal desire to write beautiful programs
that compile on the first try
and do exactly what you meant.

*Haskell 20AI: the types are strong and the evaluation is lazy.*
