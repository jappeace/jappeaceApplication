import
(builtins.fetchTarball {
  # Descriptive name to make the store path easier to identify
  name = "nixos-pin-2018-06-20";
  url = "https://github.com/nixos/nixpkgs/archive/c36f81b42665d8f9849b5a39b5c05f115c39d50a.tar.gz";
})
