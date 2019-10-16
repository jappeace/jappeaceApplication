TITLE: Pinning nixops builds
DATE: 2019-09-06
CATEGORY: tools
Tags: haskell build-tools
OPTIONS: toc:nil
status: draft

Nixops is an excelent tool for managing cloud deployments.
However by default it will use your system configurations' channels
rather then your project nix packages.
To pin this down we can simly create an (alternative) shell to run nixops in like this:

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

you can run a different shell by providng the filename:
```bash
nix-shell nixops-shell.nix
```

Where `pin.nix` looks something like:
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

This is also possible for reflex-platform projects where
we simply depend on the provided pin:

```nix
   pkgs = ((import ./reflex) { }).nixpkgs;
```
