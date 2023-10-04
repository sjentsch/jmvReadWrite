# CRAN Notes - jmvReadWrite

## Current version
0.4.1
bug-fix release: `jmv` wasn't / isn't currently available and hence the tests / examples / vignettes weren't running through

## Test environments
all environments currently return a NOTE, “Suggests or Enhances not in mainstream repositories: jmv” (and there is nothing I can do about that)
* `devtools::check()`
  - local: Ubuntu 22.04, R 4.3 (x86_64-pc-linux-gnu)
* `devtools::check_rhub()`
  - Ubuntu 20.04 R-release / Fedora R-devel / Windows 2022 Server R-devel: e-mail returns as NOTEs: two NOTEs because of jmv not being in
    mainstream repositories, and the other notes (on Linux) because of missing “tidy” (checking HTML version of manual... → no command 'tidy' found)
    or (on Windows) because of leftover files (''NULL'', 'lastMiKTeXException')
* `devtools::check_win_devel()`
  - Status: one NOTE (jmv not in mainstream repositories)

## R CMD check results
* status: OK – no notes, no warnings and no errors
