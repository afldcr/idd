{ lib, self, ... }:
with builtins;

let
  lookupDeps = key:
    foldl' (acc: x: acc.${x}) self
      (lib.string.split "\\." key);
  toNix = name: props:
    let fetchFn   = lib.fetch.${props.via};
        fetchArgs = removeAttrs props ["via" "attrs"];
     in props.attrs or {} // {
          src = fetchFn fetchArgs;
          unpackCmd = lib.unpack.${props.attrs.unpack or "copyDir"};
          deps = map lookupDeps props.attrs.deps or [];
        };
  jsonToNix = path:
    lib.set.map toNix (lib.json.read path);
in
  jsonToNix ./registry.json //
  { themes = lib.mkCollection (jsonToNix ./themes-registry.json); }
