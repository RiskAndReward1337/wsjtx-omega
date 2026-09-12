#!/usr/bin/env bash
set -euo pipefail

bundle="${1:?missing installed app bundle}"
[[ -d "${bundle}/Contents/MacOS" ]] || { echo "Invalid app bundle: ${bundle}" >&2; exit 1; }

# Bundle fixup rewrites Mach-O load commands, invalidating their old signatures.
# Sign from the inside out, after all dependency copying/rewriting has finished.
while IFS= read -r -d '' binary; do
  description="$(file -b "${binary}")"
  [[ "${description}" == *Mach-O* ]] || continue
  if [[ "$(lipo -archs "${binary}")" != arm64 ]]; then
    echo "Non-ARM64 binary in Apple Silicon bundle: ${binary}" >&2
    exit 1
  fi
  dependencies="$(otool -L "${binary}")"
  if printf '%s\n' "${dependencies}" | tail -n +2 | grep -E '^[[:space:]]+/' \
      | grep -Ev '^[[:space:]]+(/usr/lib/|/System/Library/)'; then
    echo "Unbundled dependency in ${binary}" >&2
    exit 1
  fi
  codesign --force --sign - --timestamp=none "${binary}"
done < <(find "${bundle}" -type f -print0)

while IFS= read -r -d '' framework; do
  codesign --force --sign - --timestamp=none "${framework}"
done < <(find "${bundle}" -depth -type d -name '*.framework' -print0)

codesign --force --sign - --timestamp=none "${bundle}"
codesign --verify --deep --strict --verbose=2 "${bundle}"
echo "Verified ARM64 dependencies and ad-hoc signatures: ${bundle}"
