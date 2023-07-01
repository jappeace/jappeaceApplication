Title: MySQL Persistent Support for Haskell on Windows
Date: 2023-07-01 15:44
Category: tools
OPTIONS: toc:nil
Tags: programming, windows, mysql, haskell

Using Haskell on Windows can be very useful in a [WAMP](https://www.wampserver.com/en/) like situation,
where the main legacy codebase is stuck at PHP 5.6[^forever] .
However, any new pages can be written with Haskell, 
which is much easier to upgrade since the compiler will inform you about most changes.
New pages can, for example, be written in [Yesod](https://www.yesodweb.com/), 
and then simply linked to, or posted to with normal HTML/HTTP.
The database can be used as an API to interact with PHP state.
Although I've not set up a full Yesod web server on Windows (yet),
these steps include the hardest part, the database connection.
Feel free to [contact me](mailto:hi@jappie.me) if you want help with that. 

[^forever]: Forever, because upgrading is too painful and introduces to many runtime bugs.

This is a small instructional blog post,
primarily for my own reference,
on how to get MySQL support for programs on Windows.
These steps are for *native* Windows support. There's no WSL involvement.
Specifically,
we are obtaining MySQL-persistent support with the help
of the pure Haskell [MySQL bindings](https://hackage.haskell.org/package/persistent-mysql-haskell).
Because the bindings are in pure Haskell, they're easily portable.
Haskell abstracts most operating system oddities away.

There is support for cross-compilation on Windows somewhere
available [here](https://github.com/input-output-hk/nix-hs-hello-windows).
However, at the time of writing,
it has not been implemented in [mainstream nixpkgs](https://github.com/NixOS/nixpkgs/issues/36200).
Therefore, future Jappie,
it would be better to abandon your Nix aspirations and follow these steps.

# Install ghcup

[Cabal](https://www.haskell.org/cabal/) is a package manager for Haskell only.
It assumes that native bindings exist.
We'll provide these using [msys2](https://www.msys2.org/),
which offers a shell and system isolation.
On top of that, we utilize [pacman](https://wiki.archlinux.org/title/pacman).
With this combination, we can create a Linux-like environment to generate *native* Windows executables! 
We use ghcup to manage the versions of these programs.
The name comes from ghc, the main compiler. this is also installed.  [^nix-dfference]

[^nix-dfference]: To be clear, the major difference with nix is that nix in principle leverages cabal and ghc, but nix manages the  versions of these, but also solves the native part in a reproducible manner. This means I can write a script for a build and it mostly will keep on working forever, unless source mirrors disappear for example. 

Follow the instructions in the install link: [https://www.haskell.org/ghcup/#ghcup-instructions-win](https://www.haskell.org/ghcup/#ghcup-instructions-win)

Earlier versions suggested using [Chocolatey](https://chocolatey.org/),
but don't use Chocolatey.
It is akin to [apt](https://manpages.ubuntu.com/manpages/xenial/man8/apt.8.html)
and can cause significant problems with Haskell packages.
If you need system package management and Haskell package management,
use [Nix](https://nixos.org/).
It's the only package manager that seems to work reliably.
However nix has no native Windows support.

Answer the other questions:

```
   default path "C:\" -- 1
   cabal to C:\cabal -- 2
   HLS N -- 3
   stack N -- 4
   msys2 Y -- 5
```
1. The path used for ghcup isn't important.
2. Again, it doesn't matter. Placing everything at the top-level C makes it easy to find.
3. I don't use Windows for editing. I mounted the project folder as a virtual box folder.
   On Linux, I use Emacs to maintain changes,
   and I run the Windows based cabal command in this mounted folder.
4. We don't use stack as it adds another layer of complexity on top of Cabal.
5. This is necessary to build the required system dependencies with relative ease. 

Keep in mind that you need to reopen the terminal for the new programs,
such as `cabal` and `ghcup`, to register on the `$PATH`.

# Build manually with msys2

Next, you need to open an msys2 terminal (which is different from a PowerShell).
To install git, use pacman in a msys64 terminal:

```
pacman -S git
```

There

 are two spells, and I'm not sure which one made it work:
```
cabal user-config -a "extra-prog-path: %HOME%\.ghcup\bin, %HOME%\AppData\Roaming\cabal\bin, C:\\ghcup\msys64, C:\\ghcup\msys64\mingw64\bin, C:\\ghcup\msys64\user\bin" -a "extra-include-dirs: C:\\ghcup\msys64\mingw64\include" -a "extra-lib-dirs: C:\\ghcup\msys64\mingw64\lib" -f init
```
I also did this:

```
    $Env:Path += ";C:\ghcup\msys64\mingw64\bin"
    $Env:Path += ";C:\ghcup\msys64\usr\bin"
```

After these steps, it worked.
Now cabal can find git, and therefore download our patched versions.
Fret not, I'm working on getting these changes into Hackage directly.
It just takes time to do all the politics involved and
give people the opportunity to reply to my requests.
However, even if I manage to get the changes upstream, 
there is a big chance you'd need one native dependency or another anyway,
which this setup will give you.

# Cabal
You need to instruct Cabal to use the correct MySQL packages `project.cabal`:
```cabal
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


You can generate an intial configuration with `cabal init`,
then add `persistent-mysql-haskell` dependency to your build-depends:
`packagename.cabal` file:

```
  build-depends:
    , base                      >=4.9.1.0 && <5
    , persistent-mysql-haskell
```

In combination with the `cabal.project` file, cabal should be using
git instead of hackage to find that package.
Test it out with with `cabal build`.

# Conclusion

This should set you up with Windows development for Haskell
in more complicated setups than just some puzzles.
You're probably better off getting an Ubuntu VM if you just
want to build something *new* in Haskell.
However, for legacy integrations, this will work.
If not, please leave a comment below, or [contact me](mailto:hi@jappie.me).
