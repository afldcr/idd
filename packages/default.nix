{ nixpkgs  ? <nixpkgs>
, pkgs     ? import nixpkgs {}
, settings ? null
}:

with builtins;

let
  givenSettings = settings;
  nixpkgs       = pkgs;
in let

  doImport = name:
    import (toPath "${toString ./.}/${name}")
      { inherit lib nixpkgs settings; };

  allPackages =
    let files     = readDir ./.;
        fileNames = attrNames files;
        filterFn  = name: let type = files.${name};
                              pfx  = substring 0 1 name;
                           in type == "directory" &&
                              pfx  != "_"         &&
                              pfx  != ".";
     in filter filterFn fileNames;

  settings =
    if givenSettings != null then
      givenSettings
    else if settingsPath != null then
      import settingsPath { inherit lib nixpkgs; }
    else
      {};

  settingsPath =
    let xdgConfigDir = getEnv "XDG_CONFIG_HOME";
        homeDir      = getEnv "HOME";
        choices      = [ { root = xdgConfigDir; path = "idd.nix"; }
                         { root = homeDir;      path = ".config/idd.nix"; }
                         { root = homeDir;      path = ".idd.nix"; }
                       ];
    in foldl' (acc: {root, path}:
      if acc != null then
        acc
      else if root != null then
        (let p = toPath "${root}/${path}";
          in if pathExists p then p else null)
      else
        null) null choices;

  lib =
    import ./_lib;

in
  nixpkgs.lib.genAttrs allPackages doImport
