Title: Fullstack Haskell: Reflex & Servant
Date: 2018-09-24 17:30
Category: tools
OPTIONS: toc:nil
Tags: haskell, programming, tools, database
subreddit: haskell programming
status: draft

In the [pragmatic haskell](/pragmatic-haskell.html) series, we saw how to setup a simple webserver with database.
But at some point you still need a frontend.
If it were 2005 you may have been able to get away with just [blaze](http://hackage.haskell.org/package/blaze-html).
But we are in 2018+, and [javascript is a problem](https://wiki.haskell.org/The_JavaScript_Problem).
In this blog post we will explore how to deal with javascript trough reflex and GHCJS.

# Preperation
First we need to setup another environment.
This time we'll double down on nix because reflex does that too and fighting build tools is no fun.
These commands are mostly from [here](https://github.com/reflex-frp/reflex-platform/blob/develop/docs/project-development.md#creating-the-project)
```shell
mkdir backend
mv !(backend) backend
mkdir -p common frontend
git submodule add https://github.com/reflex-frp/reflex-platform
reflex-platform/try-reflex
```

that last commnad will take ages, don't worry, it's supposed to.
Now we have to define the nix environment.
First let's generate the cabal files:

```shell
(cd common && cabal init) && (cd backend && cabal init) && (cd frontend && cabal init)
```

This will ask you a bunch of questions, using the default seems to work well.
We need to edit them anyway, both frontend and backend need to depend on common.
Frontend also needs to depend on reflex-dom.
Common needs to expose an (empty) module Common, which just contains the line `module Common where`.
This should build.
For reference [these chagnes](https://github.com/jappeace/awesome-project-name/commit/3b7aa85b1b2bfe27348bb5ac035bd08a36d0fcfb)
worked.

## Getting the old system running
Right so let's first get the pragmatic haskell post code running again.

# Conclusion


# Complete sources
The complete sources can be found on [github](https://github.com/jappeace/awesome-project-name/tree/reflex), and below.

## default.nix
```nix
(import ./reflex-platform {}).project ({ pkgs, ... }: {
packages = {
	common = ./common;
	backend = ./backend;
	frontend = ./frontend;
};

shells = {
	ghc = [\"common\" \"backend\" \"frontend\"];
	ghcjs = [\"common\" \"frontend\"];
};
})

```
