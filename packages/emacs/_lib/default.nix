{ base, nixpkgs }: self: super:
with builtins;

{
  mkCollection = value:
    value // { __collection = true; };

  mkPackage =
    { name
    , src
    , deps                   ? []
    , depsHostHost           ? []
    , depsHostHostPropagated ? []
    , isTheme                ? false
    , featureName            ? name
    , cleanSource            ? true
    , loadOnInit             ? false
    , themeDirectory         ? null
    , ...
    } @ attrs:

    nixpkgs.stdenv.mkDerivation (removeAttrs attrs ["deps"] // {
      inherit name src featureName loadOnInit;
      depsHostHostPropagated = deps ++ depsHostHostPropagated;
      depsHostHost = [ base ] ++ depsHostHost;
      phases = 
        [ "unpackPhase" ] ++
        nixpkgs.lib.optional cleanSource "cleanSourcePhase" ++
        [ "patchPhase"
          "buildPhase"
          "installPhase"
        ] ++
        nixpkgs.lib.optional isTheme "setupThemesPhase" ++
        [ "fixupPhase" ];
      builder = ./package-builder.sh;
    });

  mkPackages = f:
    let defs = if builtins.isFunction f then f pkgs else f;
        pkgs = builtins.listToAttrs
          (map (name: let value = defs.${name};
                       in { inherit name;
                            value = if value ? __collection then
                                      self.mkPackages value
                                    else
                                      self.mkPackage ({ inherit name; } // value);
                          })
            (builtins.attrNames defs));
     in pkgs;
}
