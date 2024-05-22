# CRAN Notes - jmvReadWrite

## Current version
0.4.6
* ensure that the `dataType` attribute is preserved / honoured by `write_omv` (earlier on, the data type
  of a variable / column was determined by a logic and my have changed the class of this column)
* fixed a bug leading to an error in `write_omv` when columns where completely empty (i.e., if they contained
  only NAs; incl. the respective unit tests)
* reduced cyclomatic complexity for `replace_omv`, `wide2long_omv`, and  `jmvAtt`

## Test environments
* `devtools::check()`
  - local (Ubuntu 22.04, R 4.4 x86_64-pc-linux-gnu): 0 errors, 0 warnings, 0 notes
* `rhub::rc_submit(platforms=c("linux", "windows", "macos", "macos-arm64"))`
  - linux (r-devel), windows (r-devel), macos (r-devel), macos-arm64 (r-devel):
    0 errors, 0 warnings, 2 notes (about files found in the main directory; but these files are correctly excluded in .Rbuildignore which rhub doesn't seem to honour)
* `devtools::check_win_devel()`
  - status: 0 errors, 0 warnings, 0 notes (status: OK)

## R CMD check (on .tar.gz)
* status: OK – no notes, no warnings and no errors
