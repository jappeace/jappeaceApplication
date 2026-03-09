{ sources ? import ./npins
, pkgs ? import sources.nixpkgs {}
}:
let
  shake-blog = import ./shake { inherit sources pkgs; };
in
pkgs.mkShell {
  buildInputs = [
    shake-blog
    pkgs.glibcLocales
  ];
  LANG = "en_US.UTF-8";
  LOCALE_ARCHIVE = "${pkgs.glibcLocales}/lib/locale/locale-archive";
}
