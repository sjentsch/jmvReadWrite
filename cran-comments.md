# CRAN Notes - jmvReadWrite

## Current version
0.2.4

## Test environments
* ``devtools::check()`` - local: Ubuntu 20.04, R 4.1.0 (x86_64-pc-linux-gnu)
* ``devtools::check_rhub(pkg = ".", platforms = rhub::platforms()$name)``
  compiles OK on Mac (High Sierra, R-release: CRAN / brew), Windows Server 2008 (R-patched, R-release, R-oldrel)
  mail returns PREPERROR but in the log file "Status: OK": Debian (R-release, R-devel), Fedora (R-devel), Ubuntu 20.04 (R-release, R-devel)
  compiles with NOTE on Windows Server 2022 (R-devel): file left in temp directory and Solaris (R-release, R-devel): pandoc not installed
  compiles with WARNING on Mac M1 (High Sierra, R-release): missing qpdf / pandoc
  compiles with PREPERROR on CentOS (LaTeX error when creating manual PDF)
* ``devtools::check_win_devel()``
  no notes, warnings or errors

## R CMD check results

0 errors | 0 warnings | 0 note

* This is a new release.
