Title: Mysql persistent support for haskell on windows
Date: 2023-07-01 15:44
Category: tools
OPTIONS: toc:nil
Tags: haskell, programming, tools, reflex, frp, servant
subreddit: haskell programming reflexfrp
Status: draft

This is a small instruction blogpost, mostly for myself
on how to get mysql support for oprograms on windows.

Specifically we get mysql-persistent support,
with the help of the pure mysql bindings written in haskell.

There technically is cross compilation support
for windows, [somewhere](https://github.com/input-output-hk/nix-hs-hello-windows).
It's not implemented in [mainstream nixpkgs](https://github.com/NixOS/nixpkgs/issues/36200)
at the time of writing.
So future jappie, it's better to drop your nix hope and follow these steps.

These steps are for *native* windows support. No WSL bullshit.


# Install ghcup

Link: https://www.haskell.org/ghcup/#ghcup-instructions-win

Previous versions asked about using [chocolaty](https://chocolatey.org/), do NOT use chocolatey.
It's like apt and causes major issues for haskell releases.
Similar to how you shouldn't use apt to manage haskell dependencies.

If you want package management for haskell use nix.
That's the only package manager that seems to work.
But it doesn't work on native windows.

Anwser the other questions:

```
   default path "C:\"
```
It doesn't matter what path is used for ghcup.

```
   cabal to C:\cabal
```
Agaain doesn't matter, putting everything in top-level C makes it easy to find.

```
   HLS N
```
I don't use windows for editing.

```
   stack N
```
We don't use stack as it's just another layer of complexity on top of cabal

```
   msys2 Y 
```
This is mandatory to build the required system dependencies somewhat easily.

Note that you need to re-open the terminal for the new programs,
eg `cabal` and `ghcup` to register on the `$PATH`.

# Build by hand on with msys2

next you need to open up a msys2 terminal (which is different from a powershell).
for git we can intsall with pacman in a msys64 terminal:

```
pacman -S git
```

There are two spells, I'm not sure which one made it do the work:
```
cabal user-config -a "extra-prog-path: %HOME%\.ghcup\bin, %HOME%\AppData\Roaming\cabal\bin, C:\\ghcup\msys64, C:\\ghcup\msys64\mingw64\bin, C:\\ghcup\msys64\user\bin" -a "extra-include-dirs: C:\\ghcup\msys64\mingw64\include" -a "extra-lib-dirs: C:\\ghcup\msys64\mingw64\lib" -f init
```
and then I also did this:

```
    $Env:Path += ";C:\ghcup\msys64\mingw64\bin"
    $Env:Path += ";C:\ghcup\msys64\usr\bin"
```

After these steps it worked.

## cabal.project
You've to tell cabal to use the right mysql packages:
```
packages: .

allow-newer: wire-streams:bytestring, binary-parsers:bytestring

source-repository-package
    type: git
    location: https://github.com/chordify/persistent
    tag: cf735587e590369e62168dacc2c3c2411493ae6d
    subdir: persistent-mysql-haskell

source-repository-package
    type:git
    location: https://github.com/jappeace/mysql-haskell
    tag: 8f95f0a4749b888eba96173378acbede3955ab60

source-repository-package
    type:git
    location: https://github.com/naushadh/word24
    tag: 1cc234d53923c270e888fdeac868c34306c43c70

source-repository-package
    type:git
    location: https://github.com/naushadh/word24
    tag: 1cc234d53923c270e888fdeac868c34306c43c70

```
