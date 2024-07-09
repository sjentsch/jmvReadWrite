# CRAN Notes - jmvReadWrite

## Current version
0.4.7
* added `transform_vars_omv`: apply transformations - calculating the square root, the logarithm to the base 10 or
  an inversion - to make variables (better) conform to a normal contribution (incl. unit tests)
* fixed small bugs in `read_omv` and `write_omv` to better handle variable labels

## Test environments
* `devtools::check()`
  - local (Ubuntu 22.04, R 4.4 x86_64-pc-linux-gnu): 0 errors, 0 warnings, 0 notes
* `rhub::rc_submit(platforms=c("linux", "windows", "macos", "macos-arm64"))`
  - linux (r-devel), windows (r-devel), macos (r-devel), macos-arm64 (r-devel):
    Status: OK for all four OSes
* `devtools::check_win_devel()`
  - Status: OK

## R CMD check (on .tar.gz)
* status: OK – no notes, no warnings and no errors
