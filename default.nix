{ pkgs ? (
  import ./nix/pin.nix {}
 )
}:

let
  stdenv = pkgs.stdenv;
  ignore = import ./nix/gitignoreSource.nix {  inherit (pkgs) lib; };

  talks = import ./talks {};
in
stdenv.mkDerivation {
  name = "jappie-blogs";
  src = ignore.gitignoreSource ./.;
  buildInputs = [
    pkgs.sass
    pkgs.optipng
    #    nix-env -qaPA 'nixos.nodePackages' | grep -i svgo
    pkgs.nodePackages.svgo
    pkgs.libjpeg
    # TODO svgo
    # TODO jpegtran
  ];

  # TODO buildphase
  buildPhase = ''
    mkdir -p output/talks/
    cp -R ${talks}/* output/talks/
  '';
  installPhase = ''
    # Copy the generated result
    mkdir -p $out/jappieklooster.nl $out/penguin.engineer
    cp -r "output/"* $out/jappieklooster.nl/
    cp -r "penguin/"*  $out/penguin.engineer/
  '';
}
