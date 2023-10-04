# CRAN Notes - jmvReadWrite

## Current version
0.4.1
bug-fix release: `jmv` wasn't / isn't currently available and hence the tests / examples / vignettes weren't running through

## Test environments
all environments currently return a NOTE, “Suggests or Enhances not in mainstream repositories: jmv” (and there is nothing I can do about that)
* `devtools::check()`
  - local: Ubuntu 22.04, R 4.3 (x86_64-pc-linux-gnu)
* `devtools::check_rhub()`
  - Ubuntu 20.04 R-release / Fedora R-devel: e-mail returns as PREPERROR (likely due to that the connection gets interrupted),
    but when checking the log files installation / test ends with success and one NOTE (checking HTML version of manual... → no command 'tidy' found)
  - Windows 2022 Server R-devel: four NOTEs because of jmv not in mainstream repositories (two notes), and of leftover files (''NULL'', 'lastMiKTeXException'; two notes)
* `devtools::check_win_devel()`
  - Status: NOTE (jmv not in mainstream repositories)

## R CMD check results
* no notes, no warnings and no errors
