{ nixpkgs
# Default version
, emacsVersion    ? "25.3"
, emacsMacVersion ? "6.8"
, sha256          ? "0an1pqr6icjgvjzs4y6cn3jyzqapjpjqq8s4i07pi59bmmw4jkg4"
}:

nixpkgs.stdenv.mkDerivation {
  name = "emacs-mac-${emacsVersion}-${emacsMacVersion}";
  enableParallelBuilding = true;
  builder = ./builder.sh;
  siteStart = ./site-start.el;

  # TODO: Convert to a registry.json entry
  src  = builtins.fetchTarball {
    url = "https://bitbucket.org/mituharu/emacs-mac/get/emacs-${emacsVersion}-mac-${emacsMacVersion}.tar.bz2";
    sha256 = "0an1pqr6icjgvjzs4y6cn3jyzqapjpjqq8s4i07pi59bmmw4jkg4";
  };

  preConfigurePhases = [ "macPatchPhase" "macPreConfigurePhase" ];
  preFixupPhases = [ "siteStartPhase" ];

  nativeBuildInputs = with nixpkgs;
    [ pkgconfig autoconf automake ];

  buildInputs = with nixpkgs; with nixpkgs.darwin.apple_sdk.frameworks;
    [ ncurses libxml2 gnutls texinfo gettext
      AppKit Carbon Cocoa IOKit OSAKit Quartz QuartzCore WebKit
      ImageCaptureCore GSS ImageIO
    ];

  configureFlags = [
      "LDFLAGS=-L${nixpkgs.ncurses.out}/lib"
      "--with-xml2=yes"
      "--with-gnutls=yes"
      "--with-mac"
      "--with-modules"
      "--enable-mac-app=$$out/Applications"
  ];

  CFLAGS = "-O3 -DMAC_OS_X_VERSION_MAX_ALLOWED=MAC_OS_X_VERSION_10_10 -DMAC_OS_X_VERSION_MIN_REQUIRED=MAC_OS_X_VERSION_10_10";
  LDFLAGS = "-O3 -L${nixpkgs.ncurses.out}/lib";

  passthru = {
    version = emacsVersion;
  };

  inherit (nixpkgs) gettext;
}
