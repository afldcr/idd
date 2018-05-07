{ lib, ... }:
{
  pretty-custom = {
    name = "pretty-custom";
    src = ./pretty-custom.el;
    unpackCmd = lib.unpack.copyFile;
  };
}
