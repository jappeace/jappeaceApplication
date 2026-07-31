{ sources ? import ./npins
, pkgs ? import sources.nixpkgs {}
}:
let
  shake-blog = import ./shake { inherit sources pkgs; };
  hostdir = pkgs.writeShellScriptBin "hostdir" ''
    ${pkgs.lib.getExe pkgs.python3} -m http.server "$@"
  '';
in
pkgs.mkShell {
  buildInputs = [
    shake-blog
    hostdir
    pkgs.glibcLocales
    pkgs.elmPackages.elm # shake compiles the /prijzen calculator via `elm make`
    pkgs.elmPackages.elm-test # run the calculator's pricing tests: (cd elm && elm-test)
    pkgs.nodejs # elm-test runs the compiled tests on node
  ];
  LANG = "en_US.UTF-8";
  LOCALE_ARCHIVE = "${pkgs.glibcLocales}/lib/locale/locale-archive";
}
