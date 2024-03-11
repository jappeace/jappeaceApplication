{ pkgs ? import (builtins.fetchTarball {
  # nixpkgs-unstable 2020.11.16
  url =
    "https://github.com/NixOS/nixpkgs/archive/bc714f86a515a558c2f595e1a090e882d08bd7ca.tar.gz";
}) { }

, revealjs ? pkgs.fetchFromGitHub {
  owner = "hakimel";
  repo = "reveal.js";
  rev = "4.1.0";
  sha256 = "10xhblbyw8mvak58d294hbxxnf5sq0akj6qldv7brgm6944zppm0";
}, org-files ? [ "category-adts" "keter-nix" "mtl" "excel-in-haskell" "ui-haskell" "typeclasses" "why-haskell"], theme ?
  "simple" # https://github.com/jgm/pandoc/wiki/Using-pandoc-to-produce-reveal.js-slides
}:
let
  file-paths = map (x: {
    path = ./. + "/${x}.org";
    name = x;
  }) org-files;
  # https://github.com/NixOS/nixpkgs/blob/5d8dd5c2598a74761411bc9bef7c9111d43d2429/pkgs/build-support/trivial-builders.nix#L42
in pkgs.runCommand "talks" { } (''
  set -xe
  mkdir -p $out
  cp -R ${./img} $out/img
  cp ${./makefile.host} $out/makefile
  ln -s ${revealjs} $out/reveal.js
'' + pkgs.lib.concatStrings (pkgs.lib.forEach file-paths (file: ''
  ${pkgs.pandoc}/bin/pandoc -s -V theme=${theme} -t revealjs -o $out/${file.name}.html ${file.path}
'')))
# sed -i "s/plugins:\ \[/plugins: [ RevealHighlight, /" $out/${talk-name}.html
