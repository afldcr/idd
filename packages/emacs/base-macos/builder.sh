#!/usr/bin/env bash
# shellcheck disable=SC1090,SC2154
set -eu
set -o pipefail

. "$stdenv/setup"

macPatchPhase () {
	substituteInPlace lisp/international/mule-cmds.el \
      --replace /usr/share/locale "$gettext/share/locale"
}

macPreConfigurePhase () {
	./autogen.sh
    for i in Makefile.in ./src/Makefile.in ./lib-src/Makefile.in ./leim/Makefile.in; do
        substituteInPlace "$i" --replace /bin/pwd pwd
    done
}

siteStartPhase () {
	mkdir -p "$out/share/emacs/site-lisp"
	cp "$siteStart" "$out/share/emacs/site-lisp/site-start.el"
}

genericBuild
