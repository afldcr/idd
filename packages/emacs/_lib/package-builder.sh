#!/usr/bin/env bash
# shellcheck disable=SC1090,SC2154
set -eu
set -o pipefail

declare -ax EMACS_PACKAGES
declare -a emacsFlags
. "$stdenv/setup"

for pkg in "${EMACS_PACKAGES[@]}"; do
	pkgName="$(stripHash "$pkg")"
	emacsFlags+=( -L "$pkg/share/emacs/site-lisp/$pkgName" )
done

cleanSourcePhase () {
    for f in ./*; do
	if [ "$f" = "${themeDirectory:-.}" ]; then
	    continue
	elif ! [ -f "$f" ]; then
	    echo "Removing non file file $f"
	    rm -r "$f"
	elif [ "$f" = "${f%.el}" ]; then
	    echo "Removing non emacs-lisp file $f"
	    rm -r "$f"
	elif grep -q 'ert-deftest'; then
	    echo "Removing elisp test file $f"
	    rm "$f"
	fi
    done
}

buildPhase () {
	runHook preBuild
	emacsFlags+=( -L "$(pwd)" )
	header "Building $name"
	echo "emacsFlags: ${emacsFlags[*]}"
	emacs "${emacsFlags[@]}" -batch --eval '(byte-recompile-directory "." 0 t)'
	runHook postBuild
}

installPhase () {
	runHook preInstall
	header "Installing $name"
	mkdir -p "$out/share/emacs/site-lisp/$name" "$out/nix-support"
	find . -regex '.*\.elc?$' -type f | while read -r path; do
		cp --parents "$path" "$out/share/emacs/site-lisp/$name"
	done
	cat <<- EOF > "$out/nix-support/setup-hook"
	EMACS_PACKAGES+=( '$out' )
	EOF
	runHook postInstall
}

setupThemesPhase () {
    mkdir -p "$out/nix-support"
    (
	cd "${themeDirectory:-.}"
	sha256sum *.el > "$out/nix-support/safe-themes"
    )
}

genericBuild
