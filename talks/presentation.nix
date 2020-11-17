{ pkgs ? import (builtins.fetchTarball {
    # nixpkgs-unstable 2020.11.16
    url = https://github.com/NixOS/nixpkgs/archive/bc714f86a515a558c2f595e1a090e882d08bd7ca.tar.gz;
  }) {}

, revealjs ? pkgs.fetchFromGitHub {
    owner = "hakimel";
    repo = "reveal.js";
    rev = "4.1.0";
    sha256 = "10xhblbyw8mvak58d294hbxxnf5sq0akj6qldv7brgm6944zppm0";
  }
 , org-file ? ./category-adts.org
 , talk-name ? "adt-talk"
 , theme ? "blood" # https://github.com/jgm/pandoc/wiki/Using-pandoc-to-produce-reveal.js-slides
}:

pkgs.runCommand "${talk-name}" {} ''
  mkdir -p $out
  ln -s ${revealjs} $out/reveal.js
  ${pkgs.pandoc}/bin/pandoc -s -V theme=${theme} -t revealjs -o $out/${talk-name}.html ${org-file}
''
  # sed -i "s/plugins:\ \[/plugins: [ RevealHighlight, /" $out/${talk-name}.html
