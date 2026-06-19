{ sources ? import ./npins
, pkgs ? import sources.nixpkgs {}
}:
let
  shake-blog = import ./shake { inherit sources pkgs; };
  ignore = import ./nix/gitignoreSource.nix { inherit (pkgs) lib; };
  talks = import ./talks {};
  seo-analyzer = pkgs.callPackage ./nix/seo-analyzer.nix {};
in
pkgs.stdenv.mkDerivation {
  name = "shake-blog-site";
  src = ignore.gitignoreSource ./.;
  nativeBuildInputs = [
    shake-blog
    pkgs.glibcLocales
    pkgs.optipng
    pkgs.libjpeg
    seo-analyzer
    pkgs.libxml2 # xmllint for sitemap validation
  ];
  LANG = "en_US.UTF-8";
  LOCALE_ARCHIVE = "${pkgs.glibcLocales}/lib/locale/locale-archive";
  buildPhase = ''
    shake-blog build

    # Copy talks (make writable since nix store files are read-only)
    mkdir -p _site/talks
    cp -R ${talks}/* _site/talks/
    chmod -R u+w _site/talks/

    # SEO regression checks — fail the build if OG/meta tags are missing.
    # Use find + -f (files) instead of -fl (folders) because seo-analyzer's
    # built-in ignore logic is broken (compares full paths against short names).
    seo-analyzer \
      -f $(find _site -name '*.html' \
           -not -path '_site/talks/*' \
           -not -path '_site/fire/*' \
           -not -path '_site/raw-html/*' \
           -not -path '_site/theme/*' \
           -not -path '_site/images/*' \
           -not -path '_site/files/*' \
           -not -name 'google4043c908cce5ef76.html' \
           -not -name 'reflex-talk.html' \
         ) \
      -r titleLengthRule='{"min":10,"max":120}' \
         metaBaseRule \
         metaSocialRule='{"properties":["og:title","og:description","og:type","og:locale","og:url"]}'

    seo-analyzer \
      -f $(find _penguin-site -name '*.html' \
           -not -name 'google4043c908cce5ef76.html' \
         ) \
      -r titleLengthRule='{"min":10,"max":120}' \
         metaBaseRule \
         metaSocialRule='{"properties":["og:title","og:description","og:type","og:locale","og:url"]}'

    seo-analyzer \
      -f $(find _webwinkelverhuis-site -name '*.html' \
           -not -name 'google4043c908cce5ef76.html' \
         ) \
      -r titleLengthRule='{"min":10,"max":120}' \
         metaBaseRule \
         metaSocialRule='{"properties":["og:title","og:description","og:type","og:locale","og:url"]}'

    xmllint --noout _site/sitemap.xml
    xmllint --noout _penguin-site/sitemap.xml
    xmllint --noout _webwinkelverhuis-site/sitemap.xml

    # Optimize images
    find _site -name '*.png' -exec optipng -quiet {} \;
    find _site \( -name '*.jpg' -o -name '*.jpeg' \) -exec jpegtran -copy none -optimize -progressive -outfile {} {} \;
  '';
  installPhase = ''
    mkdir -p $out/jappieklooster.nl $out/jappiesoftware.com $out/webwinkelverhuis.nl
    cp -r _site/* $out/jappieklooster.nl/
    cp -r _penguin-site/* $out/jappiesoftware.com/
    cp -r _webwinkelverhuis-site/* $out/webwinkelverhuis.nl/
  '';
}
