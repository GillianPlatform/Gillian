#!/bin/sh
# Packages downloaded CI binary artifacts as release tarballs plus SHA256SUMS.
# Usage (from the repo root): scripts/package-release.sh <version> <artifacts-dir> <out-dir>
# <artifacts-dir> holds one directory per artifact, e.g. wisl-linux-x86_64/wisl.
set -eu

version=$1 art=$2 out=$3
mkdir -p "$out"
stage=$(mktemp -d)
trap 'rm -rf "$stage"' EXIT

# gillian-c contains CompCert; ship its license from the pinned commit.
compcert=$(grep -o 'CompCert.git#[0-9a-f]*' gillian-c.opam.template | cut -d'#' -f2)
curl -fsSL -o "$stage/LICENSE-CompCert" \
  "https://raw.githubusercontent.com/GillianPlatform/CompCert/$compcert/LICENSE"

for dir in "$art"/*/; do
  name=$(basename "$dir")   # <binary>-<os>-<arch>
  bin=${name%-*-*}
  case $bin in
    wisl | wislf | gillian-c2 | gillian-js | gillian-c) ;;
    *) continue ;;           # transformers, api-docs, docker images
  esac
  platform=${name#"$bin"-}
  d="$stage/$name"
  mkdir "$d"
  cp "$dir/$bin" "$d/"
  chmod 755 "$d/$bin"
  files=$bin
  if [ "$bin" = gillian-c ]; then
    cp "$stage/LICENSE-CompCert" "$d/"
    files="$bin LICENSE-CompCert"
  fi
  # shellcheck disable=SC2086 # $files is a space-separated list of plain names
  tar czf "$out/$bin-$version-$platform.tar.gz" -C "$d" $files
done

cd "$out"
sha256sum -- *.tar.gz > SHA256SUMS
