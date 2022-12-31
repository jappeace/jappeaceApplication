import ((import (builtins.fetchGit {
    # Descriptive name to make the store path easier to identify
    name = "nixos-pin-2018-06-20";
    url = https://github.com/nixos/nixpkgs/;
    rev = "9fa29c0df25cef249645a11c3b9de4dc996bb5e9";
}) {}).fetchFromGitHub { # TODO delete after upgrade
      owner = "NixOS";
      repo = "nixpkgs-channels";
      # nixos-unstable as of 2017-11-13T08:53:10-00:00
      rev = "08d245eb31a3de0ad73719372190ce84c1bf3aee";
      sha256 = "1g22f8r3l03753s67faja1r0dq0w88723kkfagskzg9xy3qs8yw8";
})
