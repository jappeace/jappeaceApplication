{ sources ? import ./npins
, pkgs ? import sources.nixpkgs {}
}:
let
  shake-blog = import ./shake { inherit sources pkgs; };
  ignore = import ./nix/gitignoreSource.nix { inherit (pkgs) lib; };
  talks = import ./talks {};
in
pkgs.stdenv.mkDerivation {
  name = "shake-blog-site";
  src = ignore.gitignoreSource ./.;
  nativeBuildInputs = [
    shake-blog
    pkgs.glibcLocales
    pkgs.optipng
    pkgs.libjpeg
  ];
  LANG = "en_US.UTF-8";
  LOCALE_ARCHIVE = "${pkgs.glibcLocales}/lib/locale/locale-archive";
  buildPhase = ''
    shake-blog build

    # Copy talks (make writable since nix store files are read-only)
    mkdir -p _site/talks
    cp -R ${talks}/* _site/talks/
    chmod -R u+w _site/talks/

    # Optimize images
    find _site -name '*.png' -exec optipng -quiet {} \;
    find _site \( -name '*.jpg' -o -name '*.jpeg' \) -exec jpegtran -copy none -optimize -progressive -outfile {} {} \;
  '';
  installPhase = ''
    mkdir -p $out/jappieklooster.nl $out/penguin.engineer
    cp -r _site/* $out/jappieklooster.nl/
    cp -r penguin/* $out/penguin.engineer/
  '';
}
