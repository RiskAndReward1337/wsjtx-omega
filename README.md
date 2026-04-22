# WSJTX Omega

WSJTX Omega is a GPLv3 amateur radio application for weak-signal digital modes.
It is a derivative work built from WSJT-X, WSJT-X Improved+, and WSJT-Z, with
Omega-specific packaging, branding, and operating workflow changes.

This repository keeps the current source tree in `wsjtx/` and uses the
top-level superbuild to produce Windows and Linux release packages.

## Repository Layout

- `wsjtx/`: the checked-in Omega source tree.
- `CMakeLists.txt`: the superbuild used to produce release packages.
- `src/`: bundled source archives used for offline or transfer builds.
- `scripts/refresh-source-bundle.sh`: refreshes `src/wsjtx.tgz` from `wsjtx/`.
- `src/wsjtx.tgz.origin`: marks the bundled WSJTX tarball as a snapshot of the checked-in Omega tree.
- `wsjtx.patch`: the older upstream-to-Omega patch flow kept for reference and rebasing work.
- `prepare-external-source.sh`: helper used when patching bundled external sources.

## License And Attribution

Omega remains distributed under GPLv3. Upstream attribution and licensing
details for WSJT-X, WSJT-X Improved+, WSJT-Z, and bundled Hamlib are documented
in [NOTICE.md](NOTICE.md). The root [LICENSE](LICENSE) file contains the GPLv3
text used by the application.

## Local Builds

Linux:

```bash
cmake -S . -B build-linux -DCMAKE_BUILD_TYPE=Release -DWSJT_GENERATE_DOCS=OFF
cmake --build build-linux --target package --parallel
```

Windows from an MSYS2 MinGW64 shell:

```bash
cmake -S . -B build-windows -G "MinGW Makefiles" -DCMAKE_BUILD_TYPE=Release
cmake --build build-windows --target package --parallel
```

If you want to refresh the bundled `src/wsjtx.tgz` archive from the checked-in
source tree before building or publishing:

```bash
bash scripts/refresh-source-bundle.sh
```

## GitHub Releases

The release workflow in `.github/workflows/release.yml` is designed to:

- build a Linux `wsjtx-omega_*.deb`,
- build a Windows NSIS installer,
- upload the resulting artifacts to the workflow run, and
- publish them to a GitHub release when a `v*` tag is pushed.

The release job also uploads a source archive of the repository contents so the
corresponding source remains attached to tagged releases.
