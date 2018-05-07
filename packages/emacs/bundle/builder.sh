#!/usr/bin/env bash
# shellcheck disable=SC1090,SC2154
set -eu
set -o pipefail

declare -ax EMACS_PACKAGES
declare -x EMACSLOADPATH="$out/share/emacs/site-lisp"
. "$stdenv/setup"

mkdir -p "$out"
lndir -silent "$emacs" "$out"

for pkg in "${EMACS_PACKAGES[@]}"; do
	pkgName="$(stripHash "$pkg")"
        EMACSLOADPATH+=":$out/share/emacs/site-lisp/$pkgName"
	ln -s "$pkg/share/emacs/site-lisp/$pkgName" "$out/share/emacs/site-lisp/$pkgName"
done

# build bundle-start
bundle="$out/share/emacs/site-lisp/bundle-start.el"

# always require package-autoloads

echo "(require 'package-autoloads nil t)" > "$bundle"

# load some more paths on init!
# not using default.el because for some reason, that doesn't work!
for feature in $loadOnInit; do
	printf "(require '%s)\\n" "$feature" >> "$bundle"
done

# check layers for safe-themes files
printf '(setq custom-safe-themes\n (list\n' >> "$bundle"
for theme in $themes; do
    if [ -e "$theme/nix-support/safe-themes" ]; then
	cat "$theme/nix-support/safe-themes" | while read -r sha file ; do
	    echo "adding theme '$file'"
	    printf '\n   "%s"' "$sha" >> "$bundle"
	done
    fi
done
printf "\\n   'default))" >> "$bundle"

echo "(provide 'bundle-start)" >> "$bundle"

# add the default load path
EMACSLOADPATH+=':'

# shellcheck disable=SC2046
emacs -batch --load="$generateAutoloads" $(find "$out/share/emacs/site-lisp" -maxdepth 1 -type d)

# byte compile el files in the top level directory
emacs -batch -f batch-byte-compile "$out"/share/emacs/site-lisp/*.el

# make sure that load path is correctly set
for b in Applications/Emacs.app/Contents/MacOS/Emacs "bin/emacs-${emacsVersion}" bin/emacsclient; do
	if [ -L "$out/$b" ]; then
		unlink "$out/$b"
		makeWrapper "$emacs/$b" "$out/$b" --set EMACSLOADPATH "$EMACSLOADPATH"
	fi
done
