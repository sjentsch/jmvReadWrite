# CRAN Notes - jmvReadWrite

## Current version
0.4.11

* added `combine_cols_omv`
* reorganizing and cleaning the code (particularly in `read_all`)

## Test environments
* `devtools::check()`
  - local (Ubuntu 24.04, R 4.5 x86_64-pc-linux-gnu): 0 errors, 0 warnings, 0 notes
* `rhub::rc_submit(platforms=c("linux", "windows", "macos", "macos-arm64"))`
  - linux (r-devel), windows (r-devel), macos (r-devel), macos-arm64 (r-devel):
    Status: OK for all four OSes
* `devtools::check_win_devel()`
  - Status: OK

## R CMD check (on .tar.gz)
* status: OK
