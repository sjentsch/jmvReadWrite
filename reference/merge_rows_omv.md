# Merges two .omv-files for the statistical spreadsheet 'jamovi' (<https://www.jamovi.org>) by adding the content of the second, etc. file(s) as rows to the first file

Merges two .omv-files for the statistical spreadsheet 'jamovi'
(<https://www.jamovi.org>) by adding the content of the second, etc.
file(s) as rows to the first file

## Usage

``` r
merge_rows_omv(
  dtaInp = NULL,
  fleOut = "",
  typMrg = c("all", "common"),
  colInd = FALSE,
  rstRwN = TRUE,
  rmvDpl = FALSE,
  varSrt = c(),
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

  Type of merging operation: "all" (default) or "common"; see also
  Details

- colInd:

  Add a column with an indicator (the basename of the file minus the
  extension) marking from which input data set the respective rows are
  coming (default: FALSE)

- rstRwN:

  Reset row names (i.e., do not keep the row names of the original input
  data sets but number them consecutively - one to the row number of all
  input data sets added up; default: TRUE)

- rmvDpl:

  Remove duplicated rows (i.e., rows with the same content as a previous
  row in all columns; default: FALSE)

- varSrt:

  Variable(s) that are used to sort the data frame (see Details; if
  empty, the order after merging is kept; default: c())

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

a data frame (only returned if `fleOut` is empty) where the rows of all
input data sets (given in the `dtaInp`-argument) are concatenated

## Details

- Using data frames with the input parameter `dtaInp` is primarily
  thought to be used when calling `merge_cols_omv` from the
  jamovi-modules `jTransform` and `Rj`. For the use in R, it is strongly
  recommended to use a character vector with the file names instead.

- There are four different types of merging operations (defined via
  `typMrg`): "all" keeps all existing variables / columns that are
  contained in any of the input data sets and fills them up with NA
  where the variable / column doesn't exist in an input data set.
  "common" only keeps the variables / columns that are common to all
  input data sets (i.e., that are contained in all data sets).

- `varSrt` can be either a character or a character vector (with one or
  more variables respectively). The sorting order for a particular
  variable can be inverted with preceding the variable name with "-".
  Please note that this doesn't make sense and hence throws a warning
  for certain variable types (e.g., factors).

- The ellipsis-parameter (`...`) can be used to submit arguments /
  parameters to the functions that are used for merging or reading the
  data. By clicking on the respective function under “See also”, you can
  get a more detailed overview over which parameters each of those
  functions take.

- Adding columns uses `rbind` (with some further operation, adding
  missing columns (filled with NAs), if `typMrg` is "all").

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

`merge_rows_omv` internally uses the following functions: Adding columns
uses [`rbind()`](https://rdrr.io/r/base/cbind.html). For reading and
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
for (i in seq_along(nmeInp)) saveRDS(dtaInp[-i - 1], nmeInp[i])
# save dtaInp three times (i.e., the length of nmeInp), removing one data columns in
# each data set (for demonstration purposes, A1 in the first, A2 in the second, ...)
jmvReadWrite::merge_rows_omv(dtaInp = nmeInp, fleOut = nmeOut, colInd = TRUE)
cat(file.info(nmeOut)$size)
#> 10936
# -> 10767 (size may differ on different OSes)
dtaOut <- jmvReadWrite::read_omv(nmeOut, sveAtt = FALSE)
unlink(nmeOut)
# read the data set where the three original datasets were added as rows and show
# the variable names
cat(names(dtaInp))
#> ID A1 A2 A3 A4 A5 C1 C2 C3 C4 C5 E1 E2 E3 E4 E5 N1 N2 N3 N4 N5 O1 O2 O3 O4 O5 gender age ID2
cat(names(dtaOut))
#> fleInd ID A2 A3 A4 A5 C1 C2 C3 C4 C5 E1 E2 E3 E4 E5 N1 N2 N3 N4 N5 O1 O2 O3 O4 O5 gender age ID2 A1
# compared to the input data set, we have the same variable names; fleInd (switched
# on by colInd = TRUE and showing from which data set the rows are coming from) is
# new and A1 is moved to the end of the list (the "original" order of variables may
# not always be preserved and columns missing from at least one of the input data
# sets may be added at the end)
cat(dim(dtaInp), dim(dtaOut))
#> 250 29 750 30
# the first dimension of the data sets (rows) is now three times of that of the input
# data set (250 -> 750), the second dimension (columns / variables) is increased by 1
# (for "fleInd")

jmvReadWrite::merge_rows_omv(dtaInp = nmeInp, fleOut = nmeOut, typMrg = "common")
# the argument typMrg = "common" removes the columns that are not present in all of
# the input data sets (i.e., A1, A2, A3)
dtaOut <- jmvReadWrite::read_omv(nmeOut, sveAtt = FALSE)
unlink(nmeOut)
# read the data set where the three original datasets were added as rows and show
# the variable names
cat(names(dtaInp))
#> ID A1 A2 A3 A4 A5 C1 C2 C3 C4 C5 E1 E2 E3 E4 E5 N1 N2 N3 N4 N5 O1 O2 O3 O4 O5 gender age ID2
cat(names(dtaOut))
#> ID A4 A5 C1 C2 C3 C4 C5 E1 E2 E3 E4 E5 N1 N2 N3 N4 N5 O1 O2 O3 O4 O5 gender age ID2
# compared to the input data set, the variables that were missing in at least one
# data set (i.e., "A1", "A2" and "A3") are removed
cat(dim(dtaInp), dim(dtaOut))
#> 250 29 750 26
# the first dimension of the data sets (rows) is now three times of that of the
# input data set (250 -> 750), the second dimension (columns / variables) is
# reduced by 3 (i.e., "A1", "A2", "A3")

unlink(nmeInp)

```
