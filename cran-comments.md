# CRAN Notes - jmvReadWrite

## Current version
0.3.4
updated / bug-fix release (0.3.3 -> 0.3.4)

## Test environments
* ``devtools::check()``
  - local: Ubuntu 22.04, R 4.3 (x86_64-pc-linux-gnu)
* ``devtools::check_rhub()``
  - Ubuntu 20.04 R-release / Fedora R-devel: e-mail returns as PREPERROR, but when checking the log files installation / test ends with success (only the archive note)
  - Windows 2022 Server R-devel: two NOTEs because of leftover files (''NULL'', 'lastMiKTeXException') 
* ``devtools::check_rhub(pkg = ".", platforms = c("solaris-x86-patched", "solaris-x86-patched-ods"), env_vars = c("_R_CHECK_FORCE_SUGGESTS_" = "false"))``
  - compiles with NOTE on Solaris (R-release, R-devel): pandoc not installed, RProtoBuf, jmv (imports RProtoBuf) and rmarkdown not available
* ``devtools::check_win_devel()``
  - no notes, warnings or errors

## R CMD check results
* no notes, no warnings and no errors
