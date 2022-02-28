# CRAN Notes - jmvReadWrite

## Current version
0.3.1
updated / bug-fix release (0.3.0 -> 0.3.1).

## Test environments
* ``devtools::check()`` - local: Ubuntu 20.04, R 4.1 (x86_64-pc-linux-gnu)
* ``devtools::check_rhub(pkg = ".", platforms = rhub::platforms()$name, env_vars = c("_R_CHECK_FORCE_SUGGESTS_" = "false"))``
  compiles OK on Mac (High Sierra, R-release: CRAN / brew), Windows Server 2008 (R-patched, R-release, R-oldrel)
  compiles with NOTE on Windows Server 2022 (R-devel): file left in temp directory
  compiles with NOTE on Solaris (R-release, R-devel): pandoc not installed, RProtoBuf and jmv (imports RProtoBuf) not available
  compiles with WARNING on Mac M1 (High Sierra, R-release): missing qpdf / pandoc (problem on rhub)
  incorrectly returns with PREPERROR on (more or less) all Linux-platforms whereas the build returns "Status: OK" and "Finished: SUCCESS" in the log file (cf. https://github.com/r-hub/rhub/issues/448)
* ``devtools::check_win_devel()``
  no notes, warnings or errors

## R CMD check results
Status: OK
