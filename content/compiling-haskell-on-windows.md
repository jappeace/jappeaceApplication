Title: MySQL Persistent Support for Haskell on Windows
Date: 2023-07-01 15:44
Category: tools
OPTIONS: toc:nil
Tags: programming, windows, mysql, haskell

This is a small instructional blog post,
primarily for my own reference,
on how to get MySQL support for programs on Windows.
These steps are for *native* Windows support. There's no WSL involvement.

Using Haskell on Windows can be very useful in a [WAMP](https://www.wampserver.com/en/) like situation,
where the main legacy codebase is stuck at PHP 5.6 (forever, because upgrading is too painful).
However, any new pages can be written with Haskell, 
which is much easier to upgrade since the compiler will inform you about most changes.
New pages can, for example, be written in [Yesod](https://www.yesodweb.com/), 
and then simply linked to, or posted to with normal HTML/HTTP.
Although I've not set up a full Yesod web server on Windows (yet),
these steps include the hardest part (the database).
Feel free to [contact me](mailto:hi@jappie.me) for help if you want that. 

Specifically,
we are obtaining MySQL-persistent support with the help
of the pure MySQL bindings written in Haskell.
Because the bindings are in pure Haskell, they're easily portable.
Haskell abstracts most operating system oddities away.

There is technical support for cross-compilation on Windows in nixpkgs,
available [here](https://github.com/input-output-hk/nix-hs-hello-windows).
However, at the time of writing,
it has not been implemented in [mainstream nixpkgs](https://github.com/NixOS/nixpkgs/issues/36200).
Therefore, future Jappie,
it would be better to abandon your Nix aspirations and follow these steps.

# Install ghcup

Link: https://www.haskell.org/ghcup/#ghcup-instructions-win

Earlier versions suggested using [Chocolatey](https://chocolatey.org/),
but don't use Chocolatey.
It is akin to apt and can cause significant problems with Haskell releases.
It's similar to why you shouldn't use apt to manage Haskell dependencies.

If you need package management for Haskell, use [Nix](https://nixos.org/). It's the only package manager that seems to work.
However, it doesn't function on native Windows.

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
You need to instruct Cabal to use the correct MySQL packages:
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


Now, Cabal can figure out what to use in your 
`packagename.cabal` file:

```
  build-depends:
    , base                      >=4.9.1.0 && <5
    , persistent-mysql-haskell
```

# Conclusion

This should set you up with Windows development for Haskell
in more complicated setups than just some puzzles.
You're probably better off getting an Ubuntu VM if you just
want to build something *new* in Haskell.
However, for legacy integrations, this will work.
If not, please leave a comment below, or [contact me](mailto:hi@jappie.me).
