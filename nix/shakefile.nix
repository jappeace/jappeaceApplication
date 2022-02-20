{ pkgs ? (
  import ./pin.nix {}
 )
}:
let
 hpkgs = pkgs.haskellPackages;
 our-ghc = pkgs.ghc.withPackages (pkgs: [
    hpkgs.shake hpkgs.directory hpkgs.tagsoup
    hpkgs.text hpkgs.containers hpkgs.uri-encode
    hpkgs.process hpkgs.aeson hpkgs.pandoc hpkgs.SHA
  ]);

in

pkgs.stdenv.mkDerivation {
    name = "shake-file";
    src = builtins.filterSource (path: type: builtins.match ".*shakefile.hs" path != null) ../.;
    nativeBuildInputs = [ our-ghc pkgs.removeReferencesTo ];
    buildInputs = [];

    buildPhase = ''
    ghc shakefile.hs
    '';

    installPhase = ''
    mkdir -p $out/bin
    cp shakefile $out/bin/shake
    '';

    # disallowedReferences = [
    #   shake directory tagsoup
    #   text containers uri-encode
    #   process aeson Agda pandoc SHA pandoc-types HTTP
    #   js-flot js-jquery js-dgtable
    # ];
  }
