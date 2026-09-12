# WSJTX Omega

WSJTX Omega is a GPLv3 amateur radio application for weak-signal digital modes.
It is a derivative work built from WSJT-X, WSJT-X Improved+, and WSJT-Z.
# Disclaimer!
Unattended operation is not supported, and is likely illegal in your area.

Please be responsible when using automation features

This project is not intended for unattended operation and i am not responsible for damages if you end up doing so. 

# What's Different?
Features:

Auto CQ

Auto Call

Auto Hunt (POTA)

Auto Pota (AUTO CQ POTA)

Decoder is automatically configured to be the fastest/strongest

Filtering/Ignore workflows inspired by those found in WSJTZ

Expanded Filter functionality.

Dark Mode is automatically enabled along with my personally preferred color scheme for highlights.

Automation Debugging output

All automation features have heavily modified behavior vs Stock WSJTZ, to improve and minimize QRM/Fast QSOs.



# Why?
Due to (at the time) stagnant development of WJSTZ, I wanted to use the new Multithreaded decoder with the filtering of WSJTZ.

This project was cobbled together using Codex and Claude Code, as I have no formal experience with development of projects like WSJTX.
# Why add automation?
Personally, I believe there are those already using fully automated versions of wsjtx privately, as well as the existence of WSJTZ being public.

Most DXpeditions already run Fully Automated FT8, why can't we enjoy that also?

All arguments against automation are just simply out of touch with reality. If you don't like it, don't use it. The automation features are completely optional.

# X Feature doesn't work
This project was developed and tested in my spare time, there are some modes and features untested against my changes.

If you happen to find a bug, please make an issue on github, and i will try my best to address it.

Only FT8 has been tested with the current implementation of automation. 

# I want X Feature
Make a github issue with your feature suggestion, i may be able to add it myself. No guarantees though. 
# Tested enviroments.
WSJTX Omega has been tested on Windows 11, Windows 10/LTSC, and Linux Mint.

Mac Support is not default but might be achieved by compiling from source.

Debian based distros are currently supported, but others may not be. 


## License And Attribution

Omega remains distributed under GPLv3. Upstream attribution and licensing
details for WSJT-X, WSJT-X Improved+, WSJT-Z, and bundled Hamlib are documented
in [NOTICE.md](NOTICE.md). The root [LICENSE](LICENSE) file contains the GPLv3
text used by the application.

## Multi-Response

Multi-Response is controlled by one checkbox in the A tab and operates in FT8
Auto POTA. It sends RR73 to the finishing station and a signal report to the
next caller in one transmission. The caller-selection dropdown offers:

- **Recent callers** (default): select callers heard in the latest receive
  period, favoring stations that keep calling across consecutive periods.
  Duplicate decoder passes count as one transmission. Silent callers leave
  the queue and can become candidates again if they call again.
- **Queue order (original)**: retain the original arrival-order selection.

Recency only applies to waiting callers. Once we start working a station, a
quiet receive period does not replace that partner with a queued caller.
Normal QSO completion, a configured timeout, or an operator selection ends
that attempt. The Multi-Response selector does not run in single-response
Auto POTA, Auto CQ, Auto Call, or Auto Hunt.

Double-clicking a decoded station clears its matching auto-ignore entry so
the operator can retry it immediately, including while Auto POTA is enabled.

## QSO Limits And Counts

Settings > General > Behavior now has **Maximum QSO time** (default: 3 minutes;
zero disables it). This independent limit starts with our first transmission
to a station and includes every transmit and receive period. Reports, message
changes, and user activity do not restart it. Each station promoted by a
Multi-Response handoff gets a fresh limit when we send its first report.
Expiry lets an on-air packet finish, uses the configured auto-ignore duration
for automated attempts, and returns to the active calling mode without logging
an unfinished QSO. The remaining QSO limit appears in the status bar.

Both this limit and the existing **Tx watchdog** offer minutes or cycles.
A Maximum QSO cycle is one TX/RX pair (30 seconds in FT8, 15 seconds in FT4),
counted from the first TX slot. The secondary Tx watchdog retains its original
idle-minute behavior or can count transmitted periods of the repeated message;
in cycle mode it allows the receive period before stopping the next repeat.
Its existing activity/message-change resets do not reset Maximum QSO time.

The status-bar total includes every contact in the log, with a **Duplicates**
count alongside it. Each additional record matching the same full callsign,
band, and mode across the entire log counts as one duplicate. This is display
only: no records are removed and logging rules are unchanged.

## Local Builds

Linux:

```bash
cmake -S . -B build-linux -DCMAKE_BUILD_TYPE=Release -DWSJT_GENERATE_DOCS=OFF
cmake --build build-linux --target package --parallel
```

Windows from an MSYS2 MinGW64 shell:

```bash
cmake -S . -B build-windows -G Ninja \
  -DCMAKE_BUILD_TYPE=Release \
  -DCMAKE_PREFIX_PATH=/mingw64 \
  -DCMAKE_C_COMPILER=/mingw64/bin/gcc.exe \
  -DCMAKE_CXX_COMPILER=/mingw64/bin/g++.exe \
  -DCMAKE_Fortran_COMPILER=/mingw64/bin/gfortran.exe \
  -DCMAKE_MAKE_PROGRAM=/mingw64/bin/ninja.exe
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
