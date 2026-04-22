#!/usr/bin/env bash

set -euo pipefail

PATCH_FILE="${1:?missing patch file}"
SOURCE_DIR="${2:?missing source dir}"
MODE="${3:-generic}"

cd "$SOURCE_DIR"

if [[ -s "$PATCH_FILE" ]]; then
  patch --binary -p1 -N < "$PATCH_FILE"
fi

if [[ "$MODE" == "hamlib-autotools" ]]; then
  # Keep generated autotools files newer than the .am inputs so tarball
  # builds do not try to rerun a specific automake version.
  find . \
    \( -name Makefile.in -o -name aclocal.m4 -o -name configure -o -name config.h.in \) \
    -exec touch {} +
fi
