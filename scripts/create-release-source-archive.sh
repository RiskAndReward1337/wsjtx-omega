#!/usr/bin/env bash

set -euo pipefail

ROOT_DIR="$(CDPATH= cd -- "$(dirname -- "$0")/.." && pwd)"
OUTPUT_ARCHIVE="${1:?missing output archive path}"
ROOT_NAME="$(basename -- "${ROOT_DIR}")"

bash "${ROOT_DIR}/scripts/refresh-source-bundle.sh"

mkdir -p "$(dirname -- "${OUTPUT_ARCHIVE}")"

tar \
  --exclude-vcs \
  --exclude="${ROOT_NAME}/build-*" \
  --exclude="${ROOT_NAME}/tmp" \
  --exclude="${ROOT_NAME}/source-tree" \
  --exclude="${ROOT_NAME}/dist" \
  --exclude="${ROOT_NAME}/dist-test" \
  -czf "${OUTPUT_ARCHIVE}" \
  -C "$(dirname -- "${ROOT_DIR}")" \
  "${ROOT_NAME}"
