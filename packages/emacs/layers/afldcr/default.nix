{ lib, self, ... }:
{
  afldcr = {
    name = "afldcr";
    deps = [ self.pretty-custom ];
    loadOnInit = true;
    src = ./afldcr.el;
    unpackCmd = lib.unpack.copyFile;  
  };
}
