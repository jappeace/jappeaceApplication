Title: Private repository flake input
Date: 2025-04-20 18:21
Category: story
OPTIONS: toc:nil
Tags: nix haskell

Setting up private dependencies in nix as flake input
allows it to cache it properly.
Especially if they're flakes themselves.
However it took me a significant amount
of time to get a working configuration for this.
It's possible to setup flakes with private inputs,
Note the url format uses git+ssh instead of github:

```nix
{
  description = "some commercial project";

  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixpkgs-unstable";
    company-lib-base = {
      url = "git+ssh://git@github.com/company-io/lib-base.git?rev=8e9b3a8d43db3fbfbe2355502890e76a65c5ef1c";
      inputs.nixpkgs.follows = "nixpkgs"; # needs to work on the same package set as our main project, so follows does that
    };
  };

  outputs = { self, nixpkgs, company-lib-base }:
    let
      pkgs = import nixpkgs { system = "x86_64-linux"; config.allowUnfree = true; };
      lib = pkgs.haskell.lib;
      hpkgs = pkgs.haskellPackages.override {
        overrides = hnew: hold: {

          # shove in the privae dependency, the name before = needs to match cabal library
          converge-lib-base = converge-lib-base.defaultPackage.x86_64-linux;

          # ... other overrides here

          # main project
          the-project = lib.failOnAllWarnings ((lib.overrideCabal (hnew.callCabal2nix "the-project" ./. {}) (drv: {
            # reduce build times
            enableLibraryProfiling = false;
            enableExecutableProfiling = false;
            doHaddock = false;
          })));
        };
      };
      defaultPackage = hpkgs.the-project;
    in
    {
      defaultPackage.x86_64-linux =  defaultPackage;

      devShell.x86_64-linux = hpkgs.shellFor {
        packages = ps : [ ps."the-project" ];
        withHoogle = false;

      };
    };
}
```

# Full commercial example
The full example is like this,
which allows running the integration tests in docker compose
on github actions and run the main service
on whatever docker support.
This nix example should help
out people who want to try Haskell on nix a lot.
It only uses tools provided by nixpkgs, which is fairly
stable.

```nix
{
  description = "some commercial project";

  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixpkgs-unstable";
    company-lib-base = {
      url = "git+ssh://git@github.com/company-io/lib-base.git?rev=8e9b3a8d43db3fbfbe2355502890e76a65c5ef1c";
      inputs.nixpkgs.follows = "nixpkgs";
    };
  };

  outputs = { self, nixpkgs, company-lib-base }:
    let
      pkgs = import nixpkgs { system = "x86_64-linux"; config.allowUnfree = true; };
      lib = pkgs.haskell.lib;
      hpkgs = pkgs.haskellPackages.override {
        overrides = hnew: hold: {
          # other overrides here

          converge-lib-base = converge-lib-base.defaultPackage.x86_64-linux;

          the-project = lib.failOnAllWarnings ((lib.overrideCabal (hnew.callCabal2nix "the-project" ./. {}) (drv: {
            # reduce build times
            enableLibraryProfiling = false;
            enableExecutableProfiling = false;
            doHaddock = false;
            buildTools = [pkgs.git];
            # these two options drastically reduce docker image size
            enableSharedExecutables = false;
            postFixup = "rm -rf $out/lib $out/nix-support $out/share/doc";

            # NB I developed this to deal with integration tests in nix
            # environments: https://github.com/jappeace/esqueleto-postgis/blob/master/flake.nix#L18
            # I think I should upstream it.
            # we copy over the tests suites to be ran outside of the container
            # need to enable checks for it to build those :s
            postBuild = ''
                echo "entering the phase"
                mkdir -p $out/bin/test
                cp ./dist/build/unit-tests/unit-tests $out/bin/unit-tests
                cp ./dist/build/integration-tests/integration-tests $out/bin/integration-tests
            '';

            checkPhase = ''
            echo "ran by the actions"
            '';

          })));
        };
      };
      defaultPackage = hpkgs.the-project;
    in
    {
      defaultPackage.x86_64-linux =  defaultPackage;

      # docker image for running the thing
      packages.x86_64-linux.the-project-service = pkgs.dockerTools.buildImage
        {
          name = "project-api";
          contents = [
                     defaultPackage
                     pkgs.cacert
                    # uncoment to allow debugging with
                    # docker run -it the-project:whatevertag bash
                      pkgs.bash
                      pkgs.coreutils-full
                      pkgs.iputils
                    ];
          extraCommands = ''
              cp -R ${./migrations} ./migrations
          '';
          config = {
            Cmd = [ "project-server" ];
          };
        };

      # docker image for integration tests
      packages.x86_64-linux.the-project-tests = pkgs.dockerTools.buildImage
        {
          name = "the-project-tests";
          contents = [
                     defaultPackage
                     pkgs.cacert
                      # uncoment to allow debugging with
                      # docker run -it the-project:whatevertag bash
                      pkgs.bash
                      pkgs.coreutils-full
                      pkgs.iputils
                    ];
          extraCommands = ''
              cp -R ${./migrations} ./migrations
          '';
          config = {
            Cmd = [ "integration-tests" ];
          };
        };

      devShell.x86_64-linux = hpkgs.shellFor {
        packages = ps : [ ps."the-project" ];
        withHoogle = false;

        # you can set environment variables in the shell like this
        # (this is just a nix feature)
        ENVIRONMENT="development";
        PORT=8080;

        buildInputs = [
          hpkgs.haskell-language-server
          pkgs.ghcid
          pkgs.cabal-install
          pkgs.eventlog
          pkgs.docker-compose
          hpkgs.eventlog2html
          pkgs.openapi-generator-cli
          pkgs.hpack
          # hpkgs.ghc-events-analyze
        ];
      };
    };
}
```

Hope this saves someone else hours.
