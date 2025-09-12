# CRAN Notes - jmvReadWrite

## Current version
0.4.12

* added `aggregate_omv`
* parameter `varTme` for `wide2long_omv` can now be a vector with variable names
  (previously it could only be a prefix)
* changes suggested by pkgcheck (remove dontrun from examples, etc.)
* reducing cyclomatic complexity for `wide2long_omv`

## Test environments
* `devtools::check()`
  - local (Ubuntu 24.04, R 4.5 x86_64-pc-linux-gnu): 0 errors, 0 warnings, 0 notes
* `rhub::rc_submit(platforms=c("linux", "windows", "macos", "macos-arm64"))`
  - linux (r-devel), windows (r-devel), macos (r-devel), macos-arm64 (r-devel):
    Status: OK for all four OSes
* `devtools::check_win_devel()`
  - Status: 1 NOTE
---
Found the following (possibly) invalid URLs:
  URL: https://www.gnu.org/licenses/agpl-3.0.html
    From: README.md
    Status: Error
    Message: Timeout was reached [www.gnu.org]:
      Failed to connect to www.gnu.org port 443 after 21018 ms: Could not connect to server
---
The link, however, works without problems on my machine. It takes a moment to
load though, and hence the timeout on winbuilder might be to short.

## R CMD check (on .tar.gz)
* status: OK
