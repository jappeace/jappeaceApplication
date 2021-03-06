{ pkgs ? (
  import ./nix/pin.nix {}
 )
}:

let
  stdenv = pkgs.stdenv;
  pythonPackages = pkgs.lib.getAttr "python36Packages" pkgs;
  ignore = import ./nix/gitignoreSource.nix {  inherit (pkgs) lib; };

  minify = pythonPackages.buildPythonPackage {
      name = "pelican-minify-0.9";
      doCheck = false;
      propagatedBuildInputs = [
        pythonPackages."htmlmin"
        pythonPackages."pelican"
        pythonPackages."joblib"
      ];
      src = pkgs.fetchurl {
        url = "https://files.pythonhosted.org/packages/91/46/9956d4ef70ddf440e4f89a0fcb455eb8311eeddf7d7bbeb6fa6126ca5161/pelican-minify-0.9.tar.gz";
        sha256 = "0ww7fp3w8ai2nvcgyikrg916sdc7pbzcb4j6xi5br2ahy6qskzx0";
      };
    };

  smartypants = pythonPackages.buildPythonPackage {
    name = "smartypants-2.0.0";
    doCheck = false;
    src = pkgs.fetchurl {
      url = "https://files.pythonhosted.org/packages/09/dd/08848ea21422a585ecd2dd32c032fd8f75f0f8345225b455f3bf9e5a0feb/smartypants-2.0.0.tar.gz";
      sha256 = "0dmz9i6awarq1cnyxnpfr9nr8jn905p65mccmahrj9h268x3a4kq";
    };
  };

  cssmin = pythonPackages.buildPythonPackage {
    name = "cssmin-0.2.0";
    doCheck = false;
    src = pkgs.fetchurl {
      url = "https://files.pythonhosted.org/packages/8e/d8/dc9da69bb186303f7ab41adef0a5b6d34da2fdba006827620877760241c3/cssmin-0.2.0.tar.gz";
      sha256 = "1dk723nfm2yf8cp4pj785giqlwv42l0kj8rk40kczvq1hk6g04p0";
    };
  };
  typogrify = pythonPackages.buildPythonPackage {
    name = "typogrify-2.0.7";
    doCheck = false;
    propagatedBuildInputs = [
      smartypants
    ];
    src = pkgs.fetchurl {
      url = "https://files.pythonhosted.org/packages/8a/bf/64959d6187d42472acb846bcf462347c9124952c05bd57e5769d5f28f9a6/typogrify-2.0.7.tar.gz";
      sha256 = "0f6b2gnnxjbx1fbmkcscc6qjr4hi78kwm1wx4b766ha3va66dr4b";
    };
  };
in
stdenv.mkDerivation {
  name = "jappie-blogs";
  src = ignore.gitignoreSource ./.;
  buildInputs = [
    minify
    typogrify
    (import ./emacs.nix { inherit pkgs; })
    pythonPackages.pelican
    cssmin
    pythonPackages.markdown
    pythonPackages.webassets
    pythonPackages.praw
    pythonPackages.pygments
    pkgs.sass
    pkgs.optipng
    #    nix-env -qaPA 'nixos.nodePackages' | grep -i svgo
    pkgs.nodePackages.svgo
    pkgs.libjpeg
    # TODO svgo
    # TODO jpegtran
  ];
  buildPhase = ''
    pelican content -D -s publishconf.py
  '';
  installPhase = ''
    # Copy the generated result
    mkdir -p $out/jappieklooster.nl $out/penguin.engineer
    cp -r "output/"* $out/jappieklooster.nl/
    cp -r "penguin/"*  $out/penguin.engineer/
  '';
}
