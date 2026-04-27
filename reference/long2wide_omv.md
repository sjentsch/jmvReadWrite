# Converts .omv-files for the statistical spreadsheet 'jamovi' (<https://www.jamovi.org>) from long to wide format

Converts .omv-files for the statistical spreadsheet 'jamovi'
(<https://www.jamovi.org>) from long to wide format

## Usage

``` r
long2wide_omv(
  dtaInp = NULL,
  fleOut = "",
  varTgt = c(),
  varExc = c(),
  varID = "ID",
  varTme = c(),
  varSep = "_",
  varOrd = c("times", "vars"),
  varAgg = c("mean", "first"),
  varSrt = c(),
  usePkg = c("foreign", "haven"),
  selSet = "",
  ...
)
```

## Arguments

- dtaInp:

  Either a data frame or the name of a data file to be read (including
  the path, if required; "FILENAME.ext"; default: NULL); files can be of
  any supported file type, see Details below

- fleOut:

  Name of the data file to be written (including the path, if required;
  "FILE_OUT.omv"; default: ""); if empty, the resulting data frame is
  returned instead

- varTgt:

  Names of one or more variables to be transformed / reshaped (other
  variables are excluded, if empty(c()) all variables except `varTme`,
  `varID` and `varExc` are included; default: c())

- varExc:

  Name of the variable(s) should be excluded from the transformation,
  typically this will be between-subject-variable(s) (default: c())

- varID:

  Names of one or more variables that identify the same group /
  individual (default: c())

- varTme:

  Name of the variable(s) that differentiates multiple records from the
  same group / individual (default: c())

- varSep:

  Separator character when concatenating the fixed and time-varying part
  of the variable name ("VAR1_1", "VAR1_2"; default: "\_")

- varOrd:

  How variables / columns are organized: for "times" (default) the steps
  of the time varying variable are adjacent, for "vars" the steps of the
  original columns in the long dataset

- varAgg:

  How multiple occurrences of particular combinations of time varying
  variables are aggregated: either "mean" (calculate the mean over
  occurrences), or "first" (take the first occurrence)

- varSrt:

  Variable(s) that are used to sort the data frame (see Details; if
  empty, the order returned from reshape is kept; default: c())

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

a data frame (only returned if `fleOut` is empty) where the input data
set is converted from long to wide format

## Details

- If `varTgt` is empty, it is tried to generate it using all variables
  in the data frame except those defined by `varID`, `varTme` and
  `varExc`. The variable(s) in `varID` need to be unique identifiers (in
  the original dataset), those in `varExc` don't have this requirement.
  It is generally recommended that the variable names in `varExc` and
  `varID` should not contain the variable separator (defined in
  `varSep`; default: "\_").

- `varSrt` can be either a character or a character vector (with one or
  more variables respectively). The sorting order for a particular
  variable can be inverted with preceding the variable name with "-".
  Please note that this doesn't make sense and hence throws a warning
  for certain variable types (e.g., factors).

- The ellipsis-parameter (`...`) can be used to submit arguments /
  parameters to the functions that are used for transforming, reading or
  writing the data. By clicking on the respective function under “See
  also”, you can get a more detailed overview over which parameters each
  of those functions take.

- The transformation from long to wide uses `reshape`. `varTgt` matches
  (~) `v.names` in `reshape`, `varID` ~ `idvar`, `varTme` ~ `timevar`,
  and `varSep` ~ `sep`. The help for `reshape` is very explanatory,
  click on the link under “See also” to access it, particularly what is
  explained under “Details”.

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

`long2wide_omv` internally uses the following functions: The
transformation from long to wide uses
[`stats::reshape()`](https://rdrr.io/r/stats/reshape.html). For reading
and writing data files in different formats:
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
# generate a test dataframe with 100 (imaginary) participants / units of
#  observation (ID), 8 measurement (measure) of one variable (X)
dtaInp <- data.frame(ID = rep(as.character(seq(1, 100)), each = 8),
                     measure = rep(seq(1, 8), times = 100),
                     X = runif(800, -10, 10))
cat(str(dtaInp))
#> 'data.frame':    800 obs. of  3 variables:
#>  $ ID     : chr  "1" "1" "1" "1" ...
#>  $ measure: int  1 2 3 4 5 6 7 8 1 2 ...
#>  $ X      : num  -6.758 -0.492 -9.961 -1.171 -4.781 ...
# the output should look like this
# 'data.frame': 800 obs. of  3 variables:
#  $ ID     : chr  "1" "1" "1" "1" ...
#  $ measure: int  1 2 3 4 5 6 7 8 1 2 ...
#  $ X      : num  ...
# this data set is stored as (temporary) RDS-file and later processed by long2wide
nmeInp <- tempfile(fileext = ".rds")
nmeOut <- tempfile(fileext = ".omv")
saveRDS(dtaInp, nmeInp)
jmvReadWrite::long2wide_omv(dtaInp = nmeInp, fleOut = nmeOut, varTgt = "X", varID = "ID",
  varTme = "measure")
# it is required to give at least the arguments dtaInp, varID and varTme
# check whether the file was created and its size
cat(list.files(dirname(nmeOut), basename(nmeOut)))
#> file29033b9f0999.omv
# -> "file[...].omv" ([...] contains a random combination of numbers / characters
cat(file.info(nmeOut)$size)
#> 6621
# -> 6851 (approximate size; size may differ in every run [in dependence of
#          how well the generated random data can be compressed])
cat(str(jmvReadWrite::read_omv(nmeOut, sveAtt = FALSE)))
#> 'data.frame':    100 obs. of  9 variables:
#>  $ ID : chr  "1" "2" "3" "4" ...
#>   ..- attr(*, "jmv-id")= logi TRUE
#>   ..- attr(*, "missingValues")= list()
#>  $ X_1: num  -6.758 -0.476 7.273 1.768 -4.141 ...
#>   ..- attr(*, "missingValues")= list()
#>  $ X_2: num  -0.492 3.805 -1.28 0.045 -6.713 ...
#>   ..- attr(*, "missingValues")= list()
#>  $ X_3: num  -9.9613 -0.7821 -0.0426 -6.2044 -2.0179 ...
#>   ..- attr(*, "missingValues")= list()
#>  $ X_4: num  -1.171 9.103 3.839 -9.963 -0.808 ...
#>   ..- attr(*, "missingValues")= list()
#>  $ X_5: num  -4.78 4.25 5.21 7.55 -1.32 ...
#>   ..- attr(*, "missingValues")= list()
#>  $ X_6: num  8.77 -2.06 -6.89 -7.32 0.34 ...
#>   ..- attr(*, "missingValues")= list()
#>  $ X_7: num  4.32 -7.65 6.99 -9.55 6.92 ...
#>   ..- attr(*, "missingValues")= list()
#>  $ X_8: num  -6.74 -5.2 8.94 8.78 -8.9 ...
#>   ..- attr(*, "missingValues")= list()
# the data set is now transformed into wide (and each the measurements is now
# indicated as a suffix to X; X_1, X_2, ...)
# 'data.frame':  100 obs. of  9 variables:
#  $ ID : chr  "1" "2" "3" "4" "5" "6" "7" "8" "9" "10" ...
#   ..- attr(*, "jmv-id")= logi TRUE
#   ..- attr(*, "missingValues")= list()
#  $ X_1: num  ...
#   ..- attr(*, "missingValues")= list()
#  $ X_2: num  ...
#   ..- attr(*, "missingValues")= list()
#  $ X_3: num  ...
#   ..- attr(*, "missingValues")= list()
#  $ X_4: num  ...
#   ..- attr(*, "missingValues")= list()
#  $ X_5: num  ...
#   ..- attr(*, "missingValues")= list()
#  $ X_6: num  ...
#   ..- attr(*, "missingValues")= list()
#  $ X_7: num  ...
#   ..- attr(*, "missingValues")= list()
#  $ X_8: num  ...
#   ..- attr(*, "missingValues")= list()

unlink(nmeInp)
unlink(nmeOut)

```
