# CRAN Notes - jmvReadWrite

## Current version
0.2.2

## Test environments
* ``devtools::check()`` - local: Ubuntu 20.04, R 4.1.0 (x86_64-pc-linux-gnu)
* ``devtools::check_rhub(pkg = ".", platforms = rhub::platforms()$name)``
  currently doesn't compile on: Debian Linux, R-devel, GCC ASAN/UBSAN, Ubuntu Linux 20.04.1 LTS, R-devel with rchk (RProtoBuf 0.4.17 did not compile) and Fedora Linux, R-devel (zip not found)
  for both errors, issues have been raised on rhub; the remaining platforms compile with NOTES (most of them about archiving an earlier version of the package on CRAN)
* ``devtools::check_win_devel()``

## R CMD check results

0 errors | 0 warnings | 1 note

* This is a new release.
