{ pkgs ? (import <nixpkgs> {})
, pythonPackages ? "python36Packages"
}:

with pkgs.lib;

let _pythonPackages = pythonPackages; in
let
  stdenv = pkgs.stdenv;
  pythonPackages = getAttr _pythonPackages pkgs;


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
  name = "blog-johbo";
  src = builtins.filterSource
    (path: type:
      baseNameOf path != ".git" &&
      baseNameOf path != "output" &&
      baseNameOf path != "result")
    ./.;
  buildInputs = [
    minify 
    typogrify 
    pythonPackages.pelican
    pythonPackages.markdown
    pythonPackages.webassets
    pythonPackages.praw
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
