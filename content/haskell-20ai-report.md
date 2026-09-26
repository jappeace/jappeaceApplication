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
that nobody can figure out how to instantiate.
(In Haskell 2010 you'd use a Template Haskell splice
to generate the instances. In Haskell 20AI you ask the AI.
See Chapter 9.)

In Haskell 20AI, your `.hs` files begin with... nothing.
No `LANGUAGE` pragma at all.
You set `default-language: Haskell20AI` once in your `.cabal` file
and every module gets 55 extensions for free.

The 30-line extension block
that your project accumulated between 2016 and 2023?
Gone. Deleted. Composted.
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

## Chapter 9: The Deprecation of Template Haskell and GHC.Generics

This is the most controversial chapter
and the Haskell 20AI report makes no apologies for it.

Template Haskell and `GHC.Generics` are **soft-banned**.

Not removed from the compiler. Not deleted from Hackage.
You can still enable `TemplateHaskell` and use `GHC.Generics`
if you want to.
But Haskell 20AI does not enable them by default,
officially discourages their use,
and provides a better alternative:
you ask the AI to write the code.

### 9.1 The Case Against Template Haskell

Template Haskell was always a hack.
A glorious, powerful, stage-polymorphic hack,
but a hack nonetheless.

It breaks parallel compilation.
It breaks cross-compilation.
It makes your build times unpredictable.
It produces error messages
that reference generated code you cannot see.
It requires `TemplateHaskell` and `QuasiQuotes`,
neither of which made it into any language edition,
because even the committee knew
something was off.

The main use of Template Haskell was code generation:
deriving JSON instances, generating lenses,
creating database schemas, writing boilerplate.
In the year 20AI,
that is what AI code generation does.
It does it at development time, not compile time.
The generated code is visible, reviewable, and editable.
It doesn't break `-j8`.

```haskell
-- Haskell 2010 (Template Haskell era):
$(deriveJSON defaultOptions ''User)
$(makeLenses ''AppState)

-- Haskell 20AI (vibe coding era):
-- You asked the AI to write these.
-- It wrote them. You reviewed them.
-- They're right there in the file.
-- You can read them. You can modify them.
-- Your build doesn't recompile the entire universe
-- because someone changed a type.
instance ToJSON User where
  toJSON User{..} = object
    [ "name" .= userName
    , "age"  .= userAge
    ]
instance FromJSON User where
  parseJSON = withObject "User" $ \obj -> do
    userName <- obj .: "name"
    userAge  <- obj .: "age"
    pure User{..}
```

### 9.2 The Case Against GHC.Generics

`GHC.Generics` was the type-safe alternative to Template Haskell.
Instead of generating code at compile time,
you write a generic representation of your datatype
and then write generic functions over that representation.

The problem is that `GHC.Generics` code is unreadable.
`Rep`, `M1`, `K1`, `(:*:)`, `(:+:)` --
these are not identifiers,
they are the cry of a type system in pain.
Compile times for generics-heavy code
can rival Template Haskell.
Error messages are worse.

The Haskell 20AI report recognizes that
the problem TH and Generics both solve --
"I don't want to write boilerplate" --
has a simpler solution in 20AI:
tell the AI what you want and it writes the boilerplate.
The boilerplate is now visible source code.
It compiles fast.
It produces readable error messages.
When it's wrong, you can see that it's wrong.

### 9.3 What Replaces Them

Vibe coding.

Not as a joke.
As a serious engineering practice.

The macro system of Haskell 20AI
is a programmer sitting next to an LLM
saying "write me the `ToJSON` instance for this type"
and the LLM writing it.
The programmer reviews it.
It goes into the source tree.
It is checked by the type checker.
It is tested by the test suite.

This has several advantages
over compile-time code generation:

1. **Visibility**: the generated code is in the file, not hidden behind a splice
2. **Debuggability**: when it's wrong, you can read it
3. **Build performance**: no staged compilation, no recompilation cascades
4. **Cross-compilation**: works everywhere, because it's just Haskell
5. **Simplicity**: no need to learn the TH or Generics API

The Haskell 20AI report acknowledges
that this is a radical position.
It also notes that most Haskell programmers
are already doing this
and just haven't admitted it yet.

## Chapter 10: The Complete Haskell 20AI Extension Set


Haskell 20AI starts from GHC2024 and goes further.
The committee was too cautious. We are not.
The following extensions are all enabled by default in Haskell 20AI.

Extensions inherited from **GHC2024** are unmarked.
Extensions **new in GHC2024** (compared to GHC2021) are marked with a star.
Extensions **new in Haskell 20AI** (beyond GHC2024) are marked with two stars.

| Extension | Origin |
|---|---|
| BangPatterns | GHC2021 |
| BinaryLiterals | GHC2021 |
| **BlockArguments** | **20AI** |
| ConstrainedClassMethods | GHC2021 |
| ConstraintKinds | GHC2021 |
| *DataKinds* | *GHC2024* |
| DeriveDataTypeable | GHC2021 |
| DeriveFoldable | GHC2021 |
| DeriveFunctor | GHC2021 |
| DeriveGeneric | GHC2021 |
| DeriveLift | GHC2021 |
| DeriveTraversable | GHC2021 |
| *DerivingStrategies* | *GHC2024* |
| **DerivingVia** | **20AI** |
| *DisambiguateRecordFields* | *GHC2024* |
| DoAndIfThenElse | GHC2021 |
| **DuplicateRecordFields** | **20AI** |
| EmptyCase | GHC2021 |
| EmptyDataDecls | GHC2021 |
| EmptyDataDeriving | GHC2021 |
| ExistentialQuantification | GHC2021 |
| ExplicitForAll | GHC2021 |
| *ExplicitNamespaces* | *GHC2024* |
| FieldSelectors | GHC2021 |
| FlexibleContexts | GHC2021 |
| FlexibleInstances | GHC2021 |
| ForeignFunctionInterface | GHC2021 |
| *GADTs* | *GHC2024* |
| GADTSyntax | GHC2021 |
| GeneralisedNewtypeDeriving | GHC2021 |
| HexFloatLiterals | GHC2021 |
| ImplicitPrelude | GHC2021 |
| ImportQualifiedPost | GHC2021 |
| InstanceSigs | GHC2021 |
| KindSignatures | GHC2021 |
| *LambdaCase* | *GHC2024* |
| *MonoLocalBinds* | *GHC2024* |
| MonomorphismRestriction | GHC2021 |
| MultiParamTypeClasses | GHC2021 |
| **MultiWayIf** | **20AI** |
| NamedFieldPuns | GHC2021 |
| NamedWildCards | GHC2021 |
| NumericUnderscores | GHC2021 |
| **OverloadedRecordDot** | **20AI** |
| **OverloadedStrings** | **20AI** |
| PatternGuards | GHC2021 |
| PolyKinds | GHC2021 |
| PostfixOperators | GHC2021 |
| RankNTypes | GHC2021 |
| **RecordWildCards** | **20AI** |
| RelaxedPolyRec | GHC2021 |
| *RoleAnnotations* | *GHC2024* |
| ScopedTypeVariables | GHC2021 |
| StandaloneDeriving | GHC2021 |
| StandaloneKindSignatures | GHC2021 |
| StarIsType | GHC2021 |
| TraditionalRecordSyntax | GHC2021 |
| TupleSections | GHC2021 |
| **TypeFamilies** | **20AI** |
| TypeApplications | GHC2021 |
| TypeOperators | GHC2021 |
| TypeSynonymInstances | GHC2021 |

That is 55 extensions, enabled with one pragma.
GHC2024 gave us 48. Haskell 20AI adds 7 more
because the committee was right to be cautious
and we are right to be reckless.

## Chapter 11: Why the 20AI Extensions Were Promoted

GHC2024 omitted seven extensions
that every working Haskell programmer enables anyway.
Haskell 20AI promotes them to default status.
Here is why each one earned its place:

- `OverloadedStrings` -- `String` is `[Char]` and everyone uses `Text`.
  The fiction that string literals produce `[Char]` by default
  is a historical accident that costs every project one pragma.
  Haskell 20AI ends the charade.
- `OverloadedRecordDot` -- `user.name` instead of `name user`.
  Every other language on earth uses dot syntax for field access.
  Haskell held out for 34 years. That's long enough.
- `DuplicateRecordFields` -- every data type has a field called `name`,
  or `id`, or `value`. This extension lets them coexist.
  Combined with `DisambiguateRecordFields` (already in GHC2024),
  the record situation is finally livable.
- `TypeFamilies` -- if you have `GADTs` and `DataKinds` enabled
  (and in GHC2024 you do),
  you are going to want type families.
  Pretending otherwise is an exercise in self-deception.
- `MultiWayIf` -- guards are great. Inline guards are greater.
  `if | x > 0 -> "positive" | otherwise -> "non-positive"`
  reads better than a chain of `if-then-else`.
- `BlockArguments` -- `when condition do` instead of `when condition $ do`.
  The dollar sign before `do` is noise.
  `BlockArguments` removes it.
  Your code becomes quieter and easier to read.
- `RecordWildCards` -- controversial, yes.
  But `RecordWildCards` is enabled in roughly 80% of Haskell codebases.
  The committee can debate it. Haskell 20AI ships it.
- `DerivingVia` -- the most powerful deriving strategy,
  and inexplicably absent from GHC2024
  even though `DerivingStrategies` is included.
  Haskell 20AI corrects this oversight.

## Chapter 12: Specification of Derived Instances

Deriving instances works as in Haskell 2010,
extended by `DerivingStrategies` (section 4.3).
The compiler can derive `Eq`, `Ord`, `Show`, `Read`,
`Enum`, `Bounded`, `Ix`, `Functor`, `Foldable`,
`Traversable`, `Data`, `Typeable`, `Generic`, and `Lift`.

For anything else, you write it by hand
or use `anyclass` deriving
or `DerivingVia`.
`DerivingVia` was inexplicably absent from GHC2024.
Haskell 20AI corrects this. It's enabled by default.

## Chapter 13: Compiler Pragmas

```haskell
{-# OPTIONS_GHC -Wall #-}          -- catch warnings
{-# OPTIONS_GHC -Wno-unused-do-bind #-}  -- if you must
{-# INLINE myFunction #-}          -- for performance
{-# SPECIALIZE myFunction :: Int -> Int #-}  -- for more performance
```

Notice what's missing: no `LANGUAGE` pragma.
Haskell 20AI is the default language.
Set `default-language: Haskell20AI` in your `.cabal` file
and every module gets all 55 extensions for free.
No per-file pragma block. No copy-paste. No forgetting one.

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
| 20AI | Haskell 20AI | 55 | Inevitable |

## Appendix B: How to Upgrade

1. Set `default-language: Haskell20AI` in your `.cabal` file
2. Delete every `{-# LANGUAGE ... #-}` pragma that Haskell20AI covers (that's 55 of them)
3. Run `cabal build`
4. Fix the three warnings about `MonoLocalBinds` changing inference
5. Fix the one warning about `OverloadedStrings` changing a literal's type
6. Marvel at how little broke
7. Delete the now-empty pragma blocks from every file
8. Feel the weight lift off your shoulders
9. Wonder why you didn't do this sooner

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
