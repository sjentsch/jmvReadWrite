# CRAN Notes - jmvReadWrite

## Current version
0.4.15

* `transform_vars_omv`: ensure that only positive OR negative transformations are applied to one variable (both
  transformations have the same suffix and would be overwritten; variables are either positively OR negatively skewed,
  hence there is no requirement for applying both types of transformations)
* account for a change in `haven` (R-package; additional attribute: `haven_meta`)
* fixing typos and small bugs, and improving unit tests (e.g., limit line length)

## Test environments
* `devtools::check()`
  - local (Ubuntu 24.04, R 4.6 x86_64-pc-linux-gnu): 0 errors, 0 warnings, 0 notes
* `R CMD check` (on .tar.gz)
  - local (Ubuntu 24.04, R 4.6 x86_64-pc-linux-gnu): Status: OK
* `rhub::rc_submit(platforms=c("linux", "windows", "macos-arm64", "mkl", "nold"))`
  - linux (r-devel), windows (r-devel), mkl, nold: Status: OK for these OSes / containers
  - macos-arm64 (r-devel) currently doesn't run through because of an issue on rhub (problem when setting up dependencies)
* `devtools::check_win_devel()`
  - Status: OK
