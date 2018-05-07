{ lib, ... }:

let
  pkgs = [
    ./afldcr
    ./external
    ./pretty-custom
  ];

  doImport = self: pkg:
    import pkg {
      inherit lib self;
    };
in
  lib.mkPackages (self:
    builtins.foldl'
      (acc: x: acc // doImport self x)
      {}
      pkgs)
