WSJTX Omega for Apple Silicon
============================

This package requires an Apple Silicon (ARM64) Mac running macOS 15 or newer.
It does not support Intel Macs. Qt, Hamlib, and the other runtime libraries
are bundled; Homebrew is not required to run it.

Installation
------------

Open the DMG and drag wsjtx.app to Applications. Quit any existing Omega
instance before replacing it. The app is ad-hoc signed, not Developer ID
signed or notarized. If macOS blocks its first launch, review the warning
and, if you trust this download, use System Settings > Privacy & Security >
Open Anyway. Do not disable Gatekeeper system-wide.

Apple's explanation of first-launch approval:
https://support.apple.com/guide/mac-help/mh40616/mac

Shared memory
-------------

Omega uses the WSJT-X decoder's shared-memory configuration. If an existing
WSJT-X installation already works on this Mac, check its configuration before
making changes. Do not overwrite larger custom limits or competing WSJT-X /
JTDX startup settings without reviewing them.

For a fresh installation, the included com.wsjtx.sysctl.plist sets the maximum
segment size to 52428800 bytes and the total allocation to 25600 pages. The
app does NOT install this system-wide setting automatically.

To install it, leave the DMG mounted and run in Terminal:

  sudo cp -n "/Volumes/WSJTX Omega/com.wsjtx.sysctl.plist" /Library/LaunchDaemons/
  sudo chown root:wheel /Library/LaunchDaemons/com.wsjtx.sysctl.plist
  sudo chmod 644 /Library/LaunchDaemons/com.wsjtx.sysctl.plist

The -n option preserves any existing file. Review that file if it already
exists. In System Settings > General > Login Items & Extensions, allow the
sysctl background item if macOS requests it, then restart the Mac.

After restarting, check:

  sysctl kern.sysv.shmmax kern.sysv.shmall

The maximum segment must be at least 52428800 bytes. If Omega reports
"Unable to create shared memory segment", check these settings and whether
another radio application's startup task is replacing them.

Audio and radio setup
--------------------

Grant microphone access when prompted: this permission also covers audio
from a USB radio interface. Choose the radio's input/output devices in
Settings > Audio. Use Audio MIDI Setup to check their sample rates (normally
48000 Hz). Configure the callsign, grid, radio and CAT/PTT settings before
enabling transmit. Keep the system clock synchronized automatically.

Support and documentation
-------------------------

Omega releases and issue reports:
https://github.com/RiskAndReward1337/wsjtx-omega

WSJT-X user guide:
https://wsjt.sourceforge.io/wsjtx-main_en.html
