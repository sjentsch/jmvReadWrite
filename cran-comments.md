# CRAN Notes - jmvReadWrite

## Current version
0.4.2
bug fixes and smaller updates (changed parameter in `wide2long_omv` and added checks of input data frames in `merge_cols_omv`)

## Test environments
* `devtools::check()`
  - local: Ubuntu 22.04, R 4.3 (x86_64-pc-linux-gnu)
  - 0 errors, 0 warnings, 0 notes
* `devtools::check_rhub()`
  - Ubuntu 20.04 R-release / Fedora R-devel / Windows 2022 Server R-devel: e-mail returns as NOTEs
  
  : two NOTEs because of jmv not being in
    mainstream repositories, and the other notes (on Linux) because of missing “tidy” (checking HTML version of manual... → no command 'tidy' found)
    or (on Windows) because of leftover files (''NULL'', 'lastMiKTeXException')
* `devtools::check_win_devel()`
  - Status: one NOTE (lost braces when checking Rd files - several occurrences)

## R CMD check results
* status: OK – no notes, no warnings and no errors
