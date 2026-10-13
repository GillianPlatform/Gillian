#!/bin/sh
# Builds fully static Gillian binaries (musl). Run inside alpine:3.22 from the
# repo root, e.g.: docker run --rm -v "$PWD:/src" -w /src alpine:3.22 scripts/build-static.sh
set -eu

apk add --no-cache build-base bash git m4 pkgconf opam sqlite-dev sqlite-static \
  gmp-dev gmp-static zlib-dev zlib-static linux-headers z3 file

# Kept apart from a developer's glibc _opam and _build; dune ignores _-dirs.
# Deps come from apk above; opam cannot see them without an apk index.
export OPAMROOT=/src/_static/opam OPAMROOTISOK=1 OPAMYES=1 OPAMSWITCH=5.3.0 OPAMNODEPEXTS=1
# The repo is bind-mounted and owned by another user.
git config --global --add safe.directory '*'

[ -f "$OPAMROOT/config" ] || opam init --bare --disable-sandboxing
opam switch list --short | grep -qx "$OPAMSWITCH" \
  || opam switch create "$OPAMSWITCH" ocaml-base-compiler.5.3.0
make init-ci

# Set after init-ci: these would leak into opam's builds of dune packages.
export DUNE_BUILD_DIR=/src/_static/build DUNE_PROFILE=static

opam exec -- dune build @install
opam exec -- dune test
opam exec -- dune exec -- bash ./wisl/scripts/quicktests.sh
if [ "$(uname -m)" = x86_64 ]; then
  opam exec -- dune fmt
  make docs
fi

bins="wisl wislf gillian-c2 gillian-js"
# CompCert is x86_64-only, so gillian-c and transformers are not shipped elsewhere.
if [ "$(uname -m)" = x86_64 ]; then
  bins="$bins gillian-c"
  for t in transformers t_c t_c_a t_c_s t_js t_js_a t_js_s t_js_as t_wisl t_wisl_a t_wisl_s \
    t_wislf t_wislf_a t_wislf_s c_bi_abd; do
    bins="$bins transformers/$t"
  done
fi
rm -rf _static/dist
mkdir -p _static/dist/transformers
for b in $bins; do
  cp "$DUNE_BUILD_DIR/install/default/bin/${b#transformers/}" "_static/dist/$b"
  file "_static/dist/$b" | grep -q "statically linked" \
    || { file "_static/dist/$b"; echo "error: $b is not statically linked"; exit 1; }
done
find _static/dist -type f -exec file {} +
