#!/usr/bin/env bash
set -euo pipefail

if [[ "$(uname -s)" != Darwin || "$(uname -m)" != arm64 ]]; then
  echo "This build requires a native Apple Silicon Mac, not Rosetta or an Intel runner." >&2
  exit 1
fi

root="$(CDPATH= cd -- "$(dirname -- "$0")/.." && pwd)"
build="${root}/build-macos-arm64"
inner="${build}/wsjtx-prefix/src/wsjtx-build"
brew_prefix="$(brew --prefix)"
qt_prefix="$(brew --prefix qt@5)"
boost_prefix="$(brew --prefix boost@1.85)"
fortran="$(brew --prefix gcc@15)/bin/gfortran-15"
version="${GITHUB_REF_NAME:-$(git -C "${root}" describe --tags --always)}"
version="${version//\//-}"

# Environment prefixes supplement the superbuild's private Hamlib prefix.
export CMAKE_PREFIX_PATH="${qt_prefix}:${boost_prefix}:${brew_prefix}${CMAKE_PREFIX_PATH:+:${CMAKE_PREFIX_PATH}}"
export PKG_CONFIG_PATH="${brew_prefix}/lib/pkgconfig${PKG_CONFIG_PATH:+:${PKG_CONFIG_PATH}}"
export PATH="${qt_prefix}/bin:${brew_prefix}/bin:${PATH}"
export MACOSX_DEPLOYMENT_TARGET="${MACOSX_DEPLOYMENT_TARGET:-15.0}"

cmake -S "${root}" -B "${build}" -G Ninja \
  -DCMAKE_BUILD_TYPE=Release \
  -DCMAKE_C_COMPILER=/usr/bin/clang \
  -DCMAKE_CXX_COMPILER=/usr/bin/clang++ \
  -DCMAKE_Fortran_COMPILER="${fortran}" \
  -DCMAKE_OSX_ARCHITECTURES=arm64 \
  -DCMAKE_OSX_DEPLOYMENT_TARGET="${MACOSX_DEPLOYMENT_TARGET}" \
  -DCMAKE_OSX_SYSROOT="$(xcrun --sdk macosx --show-sdk-path)" \
  -DQt5_DIR="${qt_prefix}/lib/cmake/Qt5" \
  -DBoost_ROOT="${boost_prefix}" \
  -DBoost_NO_SYSTEM_PATHS=ON \
  -DWSJT_GENERATE_DOCS=OFF \
  -DWSJT_ENABLE_OMNIRIG=OFF \
  -DCPACK_PACKAGE_FILE_NAME="wsjtx-omega-${version#v}-macos-arm64" \
  -DCPACK_PACKAGE_CHECKSUM=SHA256

cmake --build "${build}" --target package --parallel "${BUILD_JOBS:-2}"
QT_QPA_PLATFORM=offscreen ctest --test-dir "${inner}" --output-on-failure

mkdir -p "${root}/dist/macos-arm64"
found=0
for package in "${inner}"/*.dmg "${inner}"/*.dmg.sha256; do
  [[ -f "${package}" ]] || continue
  cp "${package}" "${root}/dist/macos-arm64/"
  found=1
done
if [[ "${found}" != 1 ]]; then
  echo "No macOS release package was generated." >&2
  exit 1
fi
