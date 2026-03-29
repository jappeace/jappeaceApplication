Title: Announcement: HSMin - more AI-friendly Haskell code!
Date: 2026-04-01 09:00
Category: announcement
OPTIONS: toc:nil
Tags: haskell, joke

Code? 
Reading code is such a 2025 activity,
now we just vibe!
We should adapt our software 
such that it's easier to read for machines.
Tokens burn down trees, y'know.
So we should minify!

For example, consider this program:
```haskell
module Main where

main :: IO ()
main = putStr $ unlines $ hexagons 12 17

hexagons :: Int -> Int -> [String]
hexagons xRepeat yRepeat =
  yRepeat `times` [xRepeat `times` "/ \\_"
                  ,xRepeat `times` "\\_/ "]
  where
    n `times` l = concat (replicate n l)
```

That's the same as this one-liner:
```haskell
module Main where{main::IO ();main= putStr$unlines$hexagons 12 17;hexagons::Int->Int->[String];hexagons xRepeat yRepeat= yRepeat`times`[xRepeat`times`"/ \\_",xRepeat`times`"\\_/ "] where{times n l= concat (replicate n l)}}
```

Much better!
I've never seen a module in my life that couldn't be a one-liner.
You can now one-line too with [HSMin](https://hackage.haskell.org/package/hsmin-0.1.0)!
Happy vibing :)
