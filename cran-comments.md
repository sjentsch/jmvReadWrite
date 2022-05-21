# CRAN Notes - jmvReadWrite

## Current version
0.3.2
updated / bug-fix release (0.3.1 -> 0.3.2).

## Test environments
* ``devtools::check()`` - local: Ubuntu 22.04, R 4.2 (x86_64-pc-linux-gnu)
* ``devtools::check_rhub()``
  Ubuntu 20.04 R-release / Fedora R-devel - e-mail returns as PREPERROR, but when checking the log files installation / test ends with SUCCESS
  Windows 2022 Server R-devel returns with NOTE because of leftover LaTeX-file
* ``devtools::check_rhub(pkg = ".", platforms = c("solaris-x86-patched", "solaris-x86-patched-ods"), env_vars = c("_R_CHECK_FORCE_SUGGESTS_" = "false"))``
  compiles with NOTE on Solaris (R-release, R-devel): pandoc not installed, RProtoBuf and jmv (imports RProtoBuf) not available
* ``devtools::check_win_devel()``
  no notes, warnings or errors

## R CMD check results
Status: OK
