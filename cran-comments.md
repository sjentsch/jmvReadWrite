# CRAN Notes - jmvReadWrite

## Current version
0.3.3
updated / bug-fix release (0.3.2 -> 0.3.3)

## Test environments
* one NOTE about the package being archived (check issues were not addressed in time) for all commands below
* ``devtools::check()``
  - local: Ubuntu 22.04, R 4.2 (x86_64-pc-linux-gnu)
* ``devtools::check_rhub()``
  - Ubuntu 20.04 R-release / Fedora R-devel: e-mail returns as PREPERROR, but when checking the log files installation / test ends with success (only the archive note)
  - Windows 2022 Server R-devel: a further NOTE because of a leftover LaTeX-file
* ``devtools::check_rhub(pkg = ".", platforms = c("solaris-x86-patched", "solaris-x86-patched-ods"), env_vars = c("_R_CHECK_FORCE_SUGGESTS_" = "false"))``
  - compiles with NOTE on Solaris (R-release, R-devel): pandoc not installed, RProtoBuf and jmv (imports RProtoBuf) not available
* ``devtools::check_win_devel()``
  - no further notes, warnings or errors

## R CMD check results
* no further notes, no warnings or errors
