{
  sources ? import ../npins,
  pkgs ? import sources.nixpkgs { },
  revealjs ? sources."reveal.js",
  org-files ? [
    "category-adts"
    "ai-talk"
    "keter-nix"
    "mtl"
    "excel-in-haskell"
    "ui-haskell"
    "typeclasses"
    "why-haskell"
    "stability-innovation"
    "garbage"
  ],
  theme ? "simple", # https://github.com/jgm/pandoc/wiki/Using-pandoc-to-produce-reveal.js-slides
}:
let
  file-paths = map (x: {
    path = ./. + "/${x}.org";
    name = x;
  }) org-files;
  # https://github.com/NixOS/nixpkgs/blob/5d8dd5c2598a74761411bc9bef7c9111d43d2429/pkgs/build-support/trivial-builders.nix#L42
in
pkgs.runCommand "talks" { } (
  ''
    set -xe
    mkdir -p $out
    cp -R ${./img} $out/img
    cp ${./makefile.host} $out/makefile
    ln -s ${revealjs} $out/reveal.js
  ''
  + pkgs.lib.concatStrings (
    pkgs.lib.forEach file-paths (file: ''
      ${pkgs.pandoc}/bin/pandoc -s -V theme=${theme} -V revealjs-url=reveal.js -t revealjs -o $out/${file.name}.html ${file.path}
    '')
  )
)
# sed -i "s/plugins:\ \[/plugins: [ RevealHighlight, /" $out/${talk-name}.html
