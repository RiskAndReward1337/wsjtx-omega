# Notice And Attribution

WSJTX Omega is a derivative work. This repository contains code and release
tooling derived from multiple GPL-covered upstream projects.

## Upstream Lineage

### WSJT-X

- Upstream project: https://wsjt.sourceforge.io/
- Licensing statement: the official WSJT development page states that WSJT-X is
  open source and that its code is copyrighted and licensed under GNU GPLv3.
- Source reference: https://wsjt.sourceforge.io/devel.html

### WSJT-X Improved+

- Upstream project: https://wsjt-x-improved.sourceforge.io/
- Project statement: Uwe Risse, DG2YCB describes wsjt-x_improved as an enhanced
  version of WSJT-X and states that it is licensed under GNU GPLv3.
- Source reference: https://wsjt-x-improved.sourceforge.io/

### WSJT-Z

- Upstream project: https://sourceforge.net/projects/wsjt-z/
- Project statement: the WSJT-Z SourceForge page describes it as a modified
  version of WSJT-X by SQ9FVE, states that it is licensed under GNU GPL v3, and
  notes that most of the code is created and copyrighted by the WSJT-X team.
- Source reference: https://sourceforge.net/projects/wsjt-z/

### Hamlib

- Bundled dependency source: `src/hamlib-4.7.tar.gz`
- Project reference: https://github.com/Hamlib/Hamlib
- License note: Hamlib's `LICENSE` file states that most source files are under
  the GNU Lesser General Public License and that some parts are under the GNU
  General Public License. Keep the original Hamlib license files with any
  redistributed source bundle.

## Distribution Commitments For Omega

When publishing or redistributing WSJTX Omega, keep the following in place:

- Preserve the GPLv3 license text in `LICENSE`.
- Preserve this notice file and the original notices kept in `wsjtx/`.
- Keep the derivative attribution to WSJT-X, WSJT-X Improved+, and WSJT-Z.
- Mark Omega as modified software rather than presenting it as an official
  upstream release.
- Publish the corresponding source for released binaries in the same repository
  or release tag.

## Current Omega Attribution Summary

WSJTX Omega contains code and derivative work from:

- Joe Taylor, K1JT and the WSJT Development Team for WSJT-X.
- Uwe Risse, DG2YCB for WSJT-X Improved+ work included in this codebase.
- Tom Rudzinski, SQ9FVE and WSJT-Z contributors where WSJT-Z-derived work is
  carried forward.
- WSJTX Omega contributors for Omega-specific branding, packaging, automation,
  release, and build integration changes.

This notice is a project-maintenance aid and not legal advice.
