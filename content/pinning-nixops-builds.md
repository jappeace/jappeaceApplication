TITLE: Pinning nixops builds
DATE: 2019-10-17 17:35
CATEGORY: tools
Tags: build-tools, devops, nix
OPTIONS: toc:nil

![Pinned nixops](/images/2019/pinned-nixos.png)

[Nixops](https://nixos.org/nixops/) is an excellent tool for managing cloud deployments.
However by default it will use your system configurations' [channels](https://github.com/NixOS/nixops/issues/736#issuecomment-333399151)
rather then a [pinned nix packages](https://vaibhavsagar.com/blog/2018/05/27/quick-easy-nixpkgs-pinning/).
This will make your deployment different from whatever machine you're running it from.
Which is not [why you're using nix](https://medium.com/@ejpcmac/about-using-nix-in-my-development-workflow-12422a1f2f4c).

To pin a nixops deployment we create a shell[^alternative] from which we run nixops:
[^alternative]: For my reflex project I simply made an alternative shell,
             because reflex-platform doesn't make the shell easily modifiable.
             you can run a different shell by providing the filename to the nix-shell command:
              `nix-shell nixops-shell.nix`

```yaml
let
   pkgs = import ./pin.nix { };
in

  pkgs.stdenv.mkDerivation{
  name = "nixops-env";
  NIX_PATH="nixpkgs=${pkgs.path}";
  buildInputs = [ pkgs.nixops ];
  }
```

Where `pin.nix` [looks like](https://nixos.wiki/wiki/FAQ/Pinning_Nixpkgs) this:
```nix
let 
hostPkgs = import <nixpkgs> {};
pinnedPkgs = hostPkgs.fetchFromGitHub {
    owner = "NixOS";
    repo = "nixpkgs-channels";
    # nixos-unstable as of 15.06.2019
    rev = "1601f559e89ba71091faa26888711d4dd24c2d4d";
    sha256 = "0iayyz9617mz6424spwbi9qvmcl8hiql42czxg8mi4ycq4p1k0dx";
};
in
import pinnedPkgs {
    # since I also use this for clients I don't want to have to care
    config.allowUnfree = true; # took me to long to figure out
}
```

This is also possible for [reflex-platform](https://github.com/reflex-frp/reflex-platform)
projects where we simply depend on the provided pin:
```nix
   pkgs = ((import ./reflex) { }).nixpkgs;
```

There are other ways to do pinning, which is
described [here](https://discourse.nixos.org/t/nixops-pinning-nixpkgs/734).
I found it hard to get a working solution from that thread, 
therefore I recorded my own solution here.
