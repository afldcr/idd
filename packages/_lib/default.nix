let
  lib = {
    fetch = {
      github = {owner, ref, repo, sha256}:
        builtins.fetchTarball {
          inherit sha256;
          url = lib.string.uri {
            base = "https://github.com";
            path = [owner repo "archive" (ref + ".tar.gz")];
          };
        };
      url = builtins.fetchurl;
    };
  
    set = {
      map = f: xs:
        builtins.listToAttrs
          (builtins.map (name: { inherit name;
                                value = f name xs.${name};
                              })
                        (builtins.attrNames xs));
    };
  
    json = {
      from = builtins.fromJSON;
      read = x: builtins.fromJSON (builtins.readFile x);
      to = builtins.toJSON;
    };
  
    string = {
      split = sep: string:
        builtins.filter builtins.isString
          (builtins.split sep string);
      join = builtins.concatStringsSep;
      uri = {base ? ".", path} @ args:
        lib.string.join "/" ([base] ++ path);
    };
  
    unpack = {
      copyFile = ''
        srcDest="$(stripHash "$(basename "$curSrc")")"
        mkdir "$name"
        cp "$curSrc" "$name/$srcDest"
      '';

      copyDir = ''
        cp -R "$curSrc" "$name"
      '';
    };
  };
in lib
