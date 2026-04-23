# WSJTX Omega

WSJTX Omega is a GPLv3 amateur radio application for weak-signal digital modes.
It is a derivative work built from WSJT-X, WSJT-X Improved+, and WSJT-Z.
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


