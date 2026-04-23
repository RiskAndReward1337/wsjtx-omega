#!/usr/bin/env bash

set -euo pipefail

install_prefix="${1:?missing install prefix}"
shift

if command -v cygpath >/dev/null 2>&1; then
  install_prefix="$(cygpath -u "$install_prefix")"
fi

exec ./configure "--prefix=${install_prefix}" "$@"
