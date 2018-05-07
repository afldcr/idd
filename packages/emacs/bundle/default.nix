{ settings, nixpkgs
, base, layers
}:

let
  activeLayers =
    (settings.emacs.layers or (_: [])) layers;

  themes =
    (settings.emacs.themes or (_: [])) layers.themes;
in

nixpkgs.stdenv.mkDerivation {
  name              = "emacs";
  emacs             = base;
  emacsVersion      = base.version;
  builder           = ./builder.sh;
  generateAutoloads = ./generate-autoloads.el;
  layers            = activeLayers;
  themes            = themes;

  loadOnInit =
    builtins.map (x: x.featureName)
      (builtins.filter (x: x.loadOnInit or false) activeLayers);

  depsBuildBuild =
    [ base
      nixpkgs.makeWrapper
      nixpkgs.xorg.lndir
    ];

  depsHostHostPropagated =
    activeLayers ++ themes;
}
