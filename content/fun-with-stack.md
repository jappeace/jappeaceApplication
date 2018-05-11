Title: Fun with stack: Haskell dependency management
Date: 2018-03-21 13:30
Date: 2017-03-06 22:00
Category: tools
OPTIONS: toc:nil
Tags: stack, haskell, programming
subreddit: haskell programming
status: draft

Working at Daisee, Jappie uses a lot of Haskell programming.
Although Haskell is obviously as amazing as the [stereotype asserts](https://www.reddit.com/r/haskell/comments/zpff3/larry_wall_you_should_probably_know_about_it/),
the tooling can be a bit challenging.

One's understanding start with the fact that there is not one unified package
manager. Well there is, but only experts use
[cabal](https://www.haskell.org/cabal/) directly.
The issue is that often dependencies don't quite match from the
[Hackage](https://hackage.haskell.org/) repository,
which cabal uses for the source code.
So installing one extra dependency may cause you hours of trying to fit them
all together.

This is were [stack](https://www.stackage.org/) stepped in.
Stack guarantees that at some point in time all the provided packages in their
repository will build together, by essentially hosting their alternative to
[Hackage](https://hackage.haskell.org/), called
[Stackage](https://www.stackage.org/).

To do this the stackage repository doesn't host as much software as hackage
does.
So in certain cases we may still need to configure stack in such a way that
it pulls in a package or two from hackage.
They both use cabal for dependency resolution.

# System dependencies
Haskell is not an operating system.
There is no Haskell Linux kernel or Haskell std lib.
Therefore we are dependent upon C code to basically do anything on our system.
In other words, a Haskell programmer may use the IO monad to do IO,
however under the hood it has to deal with C libraries.

The way stack interacts with system dependencies on Linux at least is by
requesting 'devel' packages, for example:
Stack will request 'ssl' headers, and recommends the user to install a devel
package. It is up to the user to find the name of this package,
in case of fedora it's: "openssl-devel", and in case of Ubuntu it's "libssl-dev".

## Enter nix
Nix is also a package manager like stack and cabal are,
however, it's not focused on any specific language.
Nix is about reproducible builds, eg guaranteeing some build on a developer
machine is exactly the same as on the user machine.

A developer may for example have had the ssl library installed globally,
but forgot to mark it as a dependency in his software package.
A build tool would happily build that on his machine,
but now when a user downloads this code and tries to build the package it will
fail.
Nix [solves this](https://nixos.org/~eelco/pubs/phd-thesis.pdf).

In principle nix is completely independent from the Haskell ecosystem.
Build around this package manager is even an [operating system](https://nixos.org/).
But because it provides system dependencies *and* haskell dependency management,
and does so in a reproducible manner,
it has become very popular among the correctness loving Haskellers.

## Stack and nix
Stack provides the ability to specify system dependencies trough nix.
Which is very useful as you now can let any developer regardless of Linux
distribution, get on board with your stack project.

However it has an opt in for the isolated builds nix provides,
so stack would first look if a package is specified in the nix configuration,
if not it would fall back on system libraries.
This is rather strange, because you enable the plugin for reproducible builds,
but the default behavior is to look for system libraries
(eg be not reproducible).
There is an opt out for this behavior (`pure:true`), but it seems more sensible
to make this an opt in.

In case of the pure configuration, if stack can't find the library, 
it will ask the user to install the dependency on his system.
Which is of course pointless because one needs to add it to the nix configuration
![Stack error message](/images/2018/stack-error.jpg)
Although the error message was intended to be helpful,
in this particular case it ends up sending the user in the wrong direction.

# Git and stack
If a programmer is working on a piece of software, he may choose to isolate a
piece of code, to share with other programmers.
He will create this software library and opensource it on
github generally.
Stack helps by allowing users to specify a 'github' field,
which will generate an url to both the source and the issue tracker on github.

```yaml
name:                voicebase
version:             0.1.0.0
license:             BSD3
author:              "Jappie Klooster"
maintainer:          "jappie.klooster@daisee.com"
copyright:           "Daisee"
github:              blah/blah
```

Now although github is extremely popular in opensource it's of course not the
only choice,
One may put it on a competitors' website such as
[bitbucket](https://bitbucket.org/daisee/voicebase/src/master/),
or even host their own source code with [gitlab](https://gitlab.com).

There is no doubt that stack supports such a functionality for any alternative,
however it is unclear how to replace the github field properly with alternatives.
It is hard to search for a solution as it's odly specific,
but it [turns out](https://stackoverflow.com/questions/40332040/what-goes-in-a-stack-package-yaml-file?utm_medium=organic&utm_source=google_rich_qa&utm_campaign=google_rich_qa)
that [hpack](https://github.com/sol/hpack)
is used to parse the package.yaml file.
Perhaps thie `git` key is used to replace `github`?
It is unclear.
It's also ironic in that we can immediatly observe the difference strong
types make in documentation ability, this yaml file is not strongly typed.

## Commit dependencies
In certain situations one may need to change a library.
The typical way of doing this is to make the fork, and then specify in the
dependency manager, that a specific (git) url needs to be used on a specific
commit hash.
For example: If one tries to get the `56e833` commit from the
[voicebase library](https://bitbucket.org/daisee/voicebase/src/master/),
you should record the hash and url somewhere.


If we google for the query `stack.yaml git dependency`,
you just need to know that you configure this in `stack.yaml`, which is completely unrelated to `package.yaml`
google points you to
an outdated doc page which fails to work.
A helping [stackoverflow](https://stackoverflow.com/questions/43789271/stack-yaml-not-pulling-in-dependency-from-github?utm_medium=organic&utm_source=google_rich_qa&utm_campaign=google_rich_qa)
gives the reason why it doesn't work, the syntax recently has changed and
Google hasn't updated yet.
One can hardly blame stacks maintainers for this of course. 
But it still impacts a users' experience.

![Outdated docs](/images/2018/stack-old-docs.jpg)

# Conclusion
Dependency management in Haskell is complicated.
Even if one is able to become productive in the langauge,
any of the problems described here could still make it impossible for them to
make the system they want.
Learning about functors and applicative is fun,
learning what specific syntax to use to make stack pull in a git repository is
not.

