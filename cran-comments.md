# CRAN Notes - jmvReadWrite

## Current version
0.4.9

* added parameter `rtnOut` to `jmvOpn` (preventing the output from system2 to be returned, used for the jamovi module `Rj`)
* corrected a bug in `jmvPtB` handling if `requireNamespace("RProtoBuf")` returns `FALSE`

## Test environments
* `devtools::check()`
  - local (Ubuntu 24.04, R 4.4 x86_64-pc-linux-gnu): 0 errors, 0 warnings, 0 notes
* `rhub::rc_submit(platforms=c("linux", "windows", "macos", "macos-arm64"))`
  - linux (r-devel), windows (r-devel), macos (r-devel), macos-arm64 (r-devel):
    Status: OK for all four OSes
* `devtools::check_win_devel()`
  - Status: OK

## R CMD check (on .tar.gz)
* status: OK – no notes, no warnings and no errors
