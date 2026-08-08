{ sources ? import ../npins
, pkgs ? import sources.nixpkgs {}
}:
let
  hpkgs = pkgs.haskellPackages.override {
    overrides = hnew: hold: {
      shake-blog = hnew.callCabal2nix "shake-blog" ./. { };
    };
  };
  unwrapped = hpkgs.shake-blog;
in
# Decision: het shake-blog-binary wordt gewrapt met elm op PATH en een
# UTF-8-locale, zodat `./server.sh` (nix-build shake && result/bin/shake-blog
# serve) buiten de dev-shell werkt. Alternatief was elm alleen in shell.nix
# laten (de oude situatie), maar dan faalt de serve-build buiten de shell op
# `elm make`, en zonder LANG leest Data.Text.IO de content-bestanden met de
# verkeerde encoding. De wrap maakt het binary zelfvoorzienend; ci.nix (via
# de root default.nix) levert elm daarnaast zelf al aan en merkt hier niets
# van.
pkgs.symlinkJoin {
  name = "shake-blog-wrapped";
  paths = [ unwrapped ];
  nativeBuildInputs = [ pkgs.makeWrapper ];
  postBuild = ''
    wrapProgram $out/bin/shake-blog \
      --prefix PATH : ${pkgs.elmPackages.elm}/bin \
      --set-default LANG en_US.UTF-8 \
      --set-default LOCALE_ARCHIVE ${pkgs.glibcLocales}/lib/locale/locale-archive
  '';
}
