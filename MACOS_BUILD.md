# Apple Silicon Build

The release workflow builds **ARM64 only** on a native `macos-15` runner.
The initial Homebrew-based packages require **macOS 15 or newer**. Intel Macs
and Rosetta builds are not supported by this release job.

Install Apple's command line developer tools and Homebrew, then install:

```sh
brew install cmake ninja pkg-config gcc@15 qt@5 boost@1.85 fftw libusb portaudio
bash scripts/build-macos-arm64.sh
```

The script builds the repository's patched Hamlib and current `wsjtx/` source,
runs the tests, and writes a DMG and SHA-256 checksum to `dist/macos-arm64/`.
The GUI DMG omits generated Unix manpages and uses the online user guide.
The app includes its Qt plugins and non-system libraries; users do not need
Homebrew or the compiler toolchain installed. Packaging rejects non-ARM64
binaries and unresolved Homebrew/build-machine dependency paths.

The first macOS packages use **ad-hoc signing**, not Apple Developer ID signing
or notarization. macOS may require explicit approval in System Settings >
Privacy & Security to open the downloaded app. This does not require disabling
Gatekeeper system-wide. Grant microphone access when prompted so the program
can receive radio audio.

Read `ReadMe.txt` inside the DMG before first use. It describes WSJT-X's
shared-memory requirements and the included `com.wsjtx.sysctl.plist` for a
fresh Mac. Existing WSJT-X/JTDX installations may already configure these
limits; the package does not replace system settings automatically.

Pushing a `v*` tag runs the Windows, Linux, source, and macOS jobs and publishes
all successful artifacts together in a GitHub release. A manual workflow run
can select `macos-arm64` to test just the Mac build without publishing a release.
