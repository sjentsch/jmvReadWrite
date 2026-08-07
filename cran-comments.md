# CRAN Notes - jmvReadWrite

## Current version
0.4.15

* `transform_vars_omv`: ensure that only positive OR negative transformations are applied to one variable (both
  transformations have the same suffix and would be overwritten; variables are either positively OR negatively skewed,
  hence there is no requirement for applying both types of transformations)
* fixing typos and small bugs, and improving unit tests (e.g., limit line length)

## Test environments
* `devtools::check()`
  - local (Ubuntu 24.04, R 4.6 x86_64-pc-linux-gnu): 0 errors, 0 warnings, 0 notes
* `R CMD check` (on .tar.gz)
  - local (Ubuntu 24.04, R 4.6 x86_64-pc-linux-gnu): Status: OK
* `rhub::rc_submit(platforms=c("linux", "windows", "macos"))`
  - linux (r-devel), windows (r-devel), macos (r-devel): Status: OK for all OSes
* `devtools::check_win_devel()`
  - Status: OK
