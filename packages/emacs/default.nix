{ lib, nixpkgs, settings }:

let
  oldlib = lib;
in let
  base =
    if nixpkgs.stdenv.isDarwin then
      import ./base-macos { inherit nixpkgs; }
    else
      abort "Emacs is not supported on non-macos systems.";

  layers =
    import ./layers { inherit lib; };

  lib =
    oldlib // import ./_lib { inherit base nixpkgs; } lib oldlib;

in import ./bundle { inherit settings nixpkgs base layers; }
