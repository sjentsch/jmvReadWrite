# CRAN Notes - jmvReadWrite

## Current version
0.3.6
arrange_cols_omv added, bug-fixes (0.3.5 -> 0.3.6)

## Test environments
* ``devtools::check()``
  - local: Ubuntu 22.04, R 4.3 (x86_64-pc-linux-gnu)
* ``devtools::check_rhub()``
  - Ubuntu 20.04 R-release / Fedora R-devel: e-mail returns as PREPERROR, but when checking the log files installation / test ends with success (only the archive note)
  - Windows 2022 Server R-devel: two NOTEs because of leftover files (''NULL'', 'lastMiKTeXException')
* ``devtools::check_win_devel()``
  - Status: OK

## R CMD check results
* no notes, no warnings and no errors
