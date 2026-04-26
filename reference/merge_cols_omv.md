# Merges two or more data files by adding the content of other input files as columns to the first input file and outputs them as files for the statistical spreadsheet 'jamovi' (<https://www.jamovi.org>)

Merges two or more data files by adding the content of other input files
as columns to the first input file and outputs them as files for the
statistical spreadsheet 'jamovi' (<https://www.jamovi.org>)

## Usage

``` r
merge_cols_omv(
  dtaInp = NULL,
  fleOut = "",
  typMrg = c("outer", "inner", "left", "right"),
  varBy = list(),
  varSrt = c(),
  psvAnl = FALSE,
  usePkg = c("foreign", "haven"),
  selSet = "",
  ...
)
```

## Arguments

- dtaInp:

  Either a data frame (with the attribute "fleInp" containing the files
  to merge) or vector with the names of the input files (including the
  path, if required; "FILENAME.ext"; default: NULL); files can be of any
  supported file type, see Details below

- fleOut:

  Name of the data file to be written (including the path, if required;
  "FILE_OUT.omv"; default: ""); if empty, the resulting data frame is
  returned instead

- typMrg:

  Type of merging operation: "outer" (default), "inner", "left" or
  "right"; see Details below

- varBy:

  Name of the variable by which the data sets are matched, can either be
  a string, a character or a list (see Details below; default: list())

- varSrt:

  Variable(s) that are used to sort the data frame (see Details; if
  empty, the order after merging is kept; default: c())

- psvAnl:

  Whether analyses that are contained in the input file shall be
  transferred to the output file (TRUE / FALSE; default: FALSE)

- usePkg:

  Name of the package: "foreign" or "haven" that shall be used to read
  SPSS, Stata and SAS files; "foreign" is the default (it comes with
  base R), but "haven" is newer and more comprehensive

- selSet:

  Name of the data set that is to be selected from the workspace (only
  applies when reading .RData-files)

- ...:

  Additional arguments passed on to methods; see Details below

## Value

a data frame (only returned if `fleOut` is empty) where the columns of
all input data sets (given in the `dtaInp`-argument) are concatenated

## Details

- Using data frames with the input parameter `dtaInp` is primarily
  thought to be used when calling `merge_cols_omv` from the
  jamovi-modules `jTransform` and `Rj`. For the use in R, it is strongly
  recommended to use a character vector with the file names instead.

- There are four different types of merging operations (defined via
  `typMrg`): "outer" keeps all cases (but columns in the resulting data
  set may contain empty cells / missing values if same input data sets
  did not have a row containing the matching variable (defined in
  `varBy`). "inner" keeps only those cases where all datasets contain
  the same value in the matching variable, for "left" all cases from the
  first data set in `dtaInp` are kept (whereas cases that are only
  contained in the second or any later input data set are dropped), for
  "right" all cases from the second (or any higher) data set in `dtaInp`
  are kept. The behaviour of "left" and "right" may be somewhat
  difficult to predict in case of merging several data sets, therefore
  "outer" might be a safer choice if several data sets are merged.

- The variable that is used for matching (`varBy`) can either be a
  string (if all datasets contain a matching variable with the same
  name), a character vector (containing more than one matching variables
  that are contained in / the same for all data sets) or a list with the
  same length as dtaInp. In such list, each cell can again contain
  either a string (one matching variable for each data set in dtaInp) or
  a character vector (several matching variables for each data set in
  dtaInp; NB: all character vectors in the cells of the list must have
  the same length as it is necessary to always use the same number of
  matching variables when merging).

- `varSrt` can be either a character or a character vector (with one or
  more variables respectively). The sorting order for a particular
  variable can be inverted with preceding the variable name with "-".
  Please note that this doesn't make sense and hence throws a warning
  for certain variable types (e.g., factors).

- The ellipsis-parameter (`...`) can be used to submit arguments /
  parameters to the functions that are used for transforming or reading
  the data. By clicking on the respective function under “See also”, you
  can get a more detailed overview over which parameters each of those
  functions take.

- Adding columns uses `merge`. `typMrg` is implemented by setting `TRUE`
  or `FALSE` to `all.x` and `all.y` in `merge`, `varBy` matches `by.x`
  and `by.y`. The help for `merge` can be accessed by clicking on the
  link under “See also”.

- The functions for reading and writing the data are: `read_omv` and
  `write_omv` (for jamovi-files), `read.table` (for CSV / TSV files;
  using similar defaults as `read.csv` for CSV and `read.delim` for TSV
  which both are based upon `read.table`), `load` (for .RData-files),
  `readRDS` (for .rds-files), `read_sav` (needs R-package `haven`) or
  `read.spss` (needs R-package `foreign`) for SPSS-files, `read_dta`
  (`haven`) / `read.dta` (`foreign`) for Stata-files, `read_sas`
  (`haven`) for SAS-data-files, and `read_xpt` (`haven`) / `read.xport`
  (`foreign`) for SAS-transport-files. If you would like to use `haven`,
  you may need to install it using
  `install.packages("haven", dep = TRUE)`.

## See also

`merge_cols_omv` internally uses the following functions: Adding columns
uses [`merge()`](https://rdrr.io/r/base/merge.html). For reading and
writing data files in different formats:
[`read_omv()`](https://sjentsch.github.io/jmvReadWrite/reference/read_omv.md)
and
[`write_omv()`](https://sjentsch.github.io/jmvReadWrite/reference/write_omv.md)
for jamovi-files,
[`utils::read.table()`](https://rdrr.io/r/utils/read.table.html) for CSV
/ TSV files, [`load()`](https://rdrr.io/r/base/load.html) for reading
.RData-files, [`readRDS()`](https://rdrr.io/r/base/readRDS.html) for
.rds-files,
[`haven::read_sav()`](https://haven.tidyverse.org/reference/read_spss.html)
or
[`foreign::read.spss()`](https://rdrr.io/pkg/foreign/man/read.spss.html)
for SPSS-files,
[`haven::read_dta()`](https://haven.tidyverse.org/reference/read_dta.html)
or
[`foreign::read.dta()`](https://rdrr.io/pkg/foreign/man/read.dta.html)
for Stata-files,
[`haven::read_sas()`](https://haven.tidyverse.org/reference/read_sas.html)
for SAS-data-files, and
[`haven::read_xpt()`](https://haven.tidyverse.org/reference/read_xpt.html)
or
[`foreign::read.xport()`](https://rdrr.io/pkg/foreign/man/read.xport.html)
for SAS-transport-files.

## Examples

``` r
dtaInp <- jmvReadWrite::bfi_sample2
nmeInp <- paste0(tempfile(), "_", 1:3, ".rds")
nmeOut <- tempfile(fileext = ".omv")
for (i in seq_along(nmeInp)) {
    saveRDS(stats::setNames(dtaInp, c("ID", paste0(names(dtaInp)[-1], "_", i))), nmeInp[i])
}
# save dtaInp three times (i.e., the length of nmeInp), adding "_" + 1 ... 3 as index
# to the data variables (A1 ... O5, gender, age → A1_1, ...)
jmvReadWrite::merge_cols_omv(dtaInp = nmeInp, fleOut = nmeOut, varBy = "ID")
cat(file.info(nmeOut)$size)
#> 16376
# -> 17731 (size may differ on different OSes)
dtaOut <- jmvReadWrite::read_omv(nmeOut, sveAtt = FALSE)
# read the data set where the three original datasets were added as columns and show
# the variable names
cat(names(dtaOut))
#> ID A1_1 A2_1 A3_1 A4_1 A5_1 C1_1 C2_1 C3_1 C4_1 C5_1 E1_1 E2_1 E3_1 E4_1 E5_1 N1_1 N2_1 N3_1 N4_1 N5_1 O1_1 O2_1 O3_1 O4_1 O5_1 gender_1 age_1 ID2_1 A1_2 A2_2 A3_2 A4_2 A5_2 C1_2 C2_2 C3_2 C4_2 C5_2 E1_2 E2_2 E3_2 E4_2 E5_2 N1_2 N2_2 N3_2 N4_2 N5_2 O1_2 O2_2 O3_2 O4_2 O5_2 gender_2 age_2 ID2_2 A1_3 A2_3 A3_3 A4_3 A5_3 C1_3 C2_3 C3_3 C4_3 C5_3 E1_3 E2_3 E3_3 E4_3 E5_3 N1_3 N2_3 N3_3 N4_3 N5_3 O1_3 O2_3 O3_3 O4_3 O5_3 gender_3 age_3 ID2_3
cat(names(dtaInp))
#> ID A1 A2 A3 A4 A5 C1 C2 C3 C4 C5 E1 E2 E3 E4 E5 N1 N2 N3 N4 N5 O1 O2 O3 O4 O5 gender age ID2
# compared to the input data set, we have the same names (expect for "ID" which was
# used for matching and that each variable had added an indicator from which data
# set they came)
cat(dim(dtaInp), dim(dtaOut))
#> 250 29 250 85
# the first dimension of the data sets (rows) stayed the same (250), whereas the
# second dimension is now approx. three times as large (28 -> 82):
# 28 - 1 (for "ID") = 27 * 3 + 1 (for "ID") = 82
cat(colMeans(dtaInp[2:11]))
#> 2.488 4.776 4.548 4.74 4.484 4.42 4.372 4.304 2.536 3.332
cat(colMeans(dtaOut[2:11]))
#> 2.488 4.776 4.548 4.74 4.484 4.42 4.372 4.304 2.536 3.332
# it's therefore not much surprise that the values of the column means for the first
# 10 variables of dtaInp and dtaOut are the same too

unlink(nmeInp)
unlink(nmeOut)

```
