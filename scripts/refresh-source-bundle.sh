#!/usr/bin/env bash

set -euo pipefail

ROOT_DIR="$(CDPATH= cd -- "$(dirname -- "$0")/.." && pwd)"
SOURCE_DIR="${ROOT_DIR}/wsjtx"
OUTPUT_TARBALL="${1:-${ROOT_DIR}/src/wsjtx.tgz}"
OUTPUT_ORIGIN_FILE="${OUTPUT_TARBALL}.origin"

if [[ ! -f "${SOURCE_DIR}/CMakeLists.txt" ]]; then
  echo "error: expected checked-in source tree at ${SOURCE_DIR}" >&2
  exit 1
fi

mkdir -p "$(dirname -- "${OUTPUT_TARBALL}")"

tar \
  --exclude-vcs \
  --exclude='*.user' \
  --exclude='*.tmp' \
  -czf "${OUTPUT_TARBALL}" \
  -C "${ROOT_DIR}" \
  wsjtx

md5sum "${OUTPUT_TARBALL}" > "${OUTPUT_TARBALL}.md5sum"
printf 'local-tree\n' > "${OUTPUT_ORIGIN_FILE}"
