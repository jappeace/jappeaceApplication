{ pkgs ? (
  import ./nix/pin.nix {}
 )
}:

let
  stdenv = pkgs.stdenv;
  ignore = pkgs.callPackage ./nix/gitignoreSource.nix {};
  shakefile = pkgs.callPackage ./nix/shakefile.nix {};

in
pkgs.mkShell {
  name = "blog-johbo";
  buildInputs = [
    shakefile
    pkgs.sass
    pkgs.optipng
    #    nix-env -qaPA 'nixos.nodePackages' | grep -i svgo
    pkgs.nodePackages.svgo
    pkgs.libjpeg
    # TODO svgo
    # TODO jpegtran
  ];
  shellHook = ''
    export PATH=${pkgs.s3cmd}/bin:$PATH
  '';
  buildPhase = ''
    make run
  '';
  installPhase = ''
    # Copy the generated result
    mkdir -p $out/blog
    cp -r "output/"* $out/blog
  '';
}
