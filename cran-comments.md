# CRAN Notes - jmvReadWrite

## Current version
0.4.13

* corrected unit tests (for `read_omv`) for dealing with a change in how data frame attributes are arranged (changed with R 4.6)
* `aggregate_omv`: permit to aggregate over the whole data set (without a grouping variable)
* added converting Nominal / Ordinal into Numeric using the value labels (if numeric)
* `describe_omv`: added option for creating descriptions in other languages than English, and added helper function
  defining standard licenses for dtaDsc
* fixing typos and small bugs, and improving unit tests (e.g., limit line length)

## Test environments
* `devtools::check()`
  - local (Ubuntu 24.04, R 4.5 x86_64-pc-linux-gnu): 0 errors, 0 warnings, 0 notes
* `R CMD check` (on .tar.gz)
  - local (Ubuntu 24.04, R 4.5 x86_64-pc-linux-gnu): Status: OK
* `rhub::rc_submit(platforms=c("linux", "windows", "macos"))`
  - linux (r-devel), windows (r-devel), macos (r-devel): Status: OK for all OSes
* `devtools::check_win_devel()`
  - Status: OK
