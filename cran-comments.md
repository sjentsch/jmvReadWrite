# CRAN Notes - jmvReadWrite

## Current version
0.4.2
bug fixes and smaller updates (changed parameter in `wide2long_omv` and added checks of input data frames in `merge_cols_omv`)

## Test environments
* `devtools::check()`
  - local (Ubuntu 22.04, R 4.3 x86_64-pc-linux-gnu): 0 errors, 0 warnings, 0 notes
* `devtools::check_rhub()`
  - Ubuntu 20.04 R-release: 0 errors, 0 warnings, 1 note – missing “tidy” (checking HTML version of manual... → no command 'tidy' found)
  - Fedora R-devel: 0 errors, 0 warnings, 1 note – missing “tidy” (checking HTML version of manual... → no command 'tidy' found)
  - Windows 2022 Server R-devel: 0 errors, 0 warnings, 1 note – because of leftover files (''NULL'', 'lastMiKTeXException')
* `devtools::check_win_devel()`
  - status: 0 errors, 0 warnings, 0 notes (status: OK)

## R CMD check (on .tar.gz)
* status: OK – no notes, no warnings and no errors
