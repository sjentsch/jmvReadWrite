# CRAN Notes - jmvReadWrite

## Current version
0.4.0
added `describe_omv`, better integration when running in jamovi, other small improvements and bug-fixes (0.3.7 -> 0.4.0)

## Test environments
* ``devtools::check()``
  - local: Ubuntu 22.04, R 4.3 (x86_64-pc-linux-gnu)
* ``devtools::check_rhub()``
  - Ubuntu 20.04 R-release / Fedora R-devel: e-mail returns as PREPERROR (likely due to that the connection gets interrupted),
    but when checking the log files installation / test ends with success and one NOTE (checking HTML version of manual... → no command 'tidy' found)
  - Windows 2022 Server R-devel: two NOTEs because of leftover files (''NULL'', 'lastMiKTeXException')
* ``devtools::check_win_devel()``
  - Status: OK

## R CMD check results
* no notes, no warnings and no errors
