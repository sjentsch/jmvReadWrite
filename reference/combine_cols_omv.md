# Combines pairs of columns from a raw data matrix in .omv-files for the statistical spreadsheet 'jamovi' (<https://www.jamovi.org>)

Combines pairs of columns from a raw data matrix in .omv-files for the
statistical spreadsheet 'jamovi' (<https://www.jamovi.org>)

## Usage

``` r
combine_cols_omv(
  dtaInp = NULL,
  fleOut = "",
  varPrs = list(),
  mdeCmb = c("none", "first", "second"),
  psvAnl = FALSE,
  usePkg = c("foreign", "haven"),
  selSet = "",
  ...
)
```

## Arguments

- dtaInp:

  Either a data frame or the name of a data file to be read (including
  the path, if required; "FILENAME.ext"; default: NULL); files can be of
  any supported file type, see Details below.

- fleOut:

  Name of the data file to be written (including the path, if required;
  "FILE_OUT.omv"; default: ""); if empty, the resulting data frame is
  returned instead.

- varPrs:

  Definition of variable pairs; a list containing either list(s) or
  character vector(s) with the names of pairs of variables to be
  combined (default: list()).

- mdeCmb:

  Mode of combining the variables when conflicting values occur, either
  "none", "first", or "second" (default: "none"), see Details below.

- psvAnl:

  Whether analyses that are contained in the input file shall be
  transferred to the output file (TRUE / FALSE; default: FALSE)

- usePkg:

  Name of the package: "foreign" or "haven" that shall be used to read
  SPSS, Stata, and SAS files; "foreign" is the default (it comes with
  base R), but "haven" is newer and more comprehensive.

- selSet:

  Name of the data set that is to be selected from the workspace (only
  applies when reading .RData-files)

- ...:

  Additional arguments passed on to methods; see Details below.

## Value

a data frame containing the column pairs given in `varPrs` combined and
the original columns removed

## Details

- The need to combine two columns into one is quite common after merging
  columns or rows (using, e.g., merge_cols_omv or merge_rows_omv).
  `varPrs` defines the variable pairs to be combined. It is a list
  containing the pairs of variables to be combined, either as list or as
  character vector; e.g., list(c("A", "B"), c("C", "D")) or
  list(list("A", "B"), list("C", "D")). `mdeCmb` defines what to to if
  values in the first and the second variable of a variable pair contain
  conflicting / different values: "none" does not merge the variables
  (and instead throws an error), "first" makes that the values from the
  first variable of each pair are taken if the values are conflicting,
  and "second" use the values from the second variable of each pair in
  case of conflicts.

- The ellipsis-parameter (`...`) can be used to submit arguments /
  parameters to the functions that are used for reading and writing the
  data. By clicking on the respective function under “See also”, you can
  get a more detailed overview over which parameters each of those
  functions take. The functions are: `read_omv` and `write_omv` (for
  jamovi-files), `read.table` (for CSV / TSV files; using similar
  defaults as `read.csv` for CSV and `read.delim` for TSV which both are
  based upon `read.table`), `load` (for .RData-files), `readRDS` (for
  .rds-files), `read_sav` (needs the R-package `haven`) or `read.spss`
  (needs the R-package `foreign`) for SPSS-files, `read_dta` (`haven`) /
  `read.dta` (`foreign`) for Stata-files, `read_sas` (`haven`) for
  SAS-data-files, and `read_xpt` (`haven`) / `read.xport` (`foreign`)
  for SAS-transport-files. If you would like to use `haven`, you may
  need to install it using `install.packages("haven", dep = TRUE)`.

## See also

`combine_cols_omv` uses the following functions for reading and writing
data files in different formats:
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
# create a new column (A1_1) containing a subset of the values in the original variable
# whereas those lines are replaced with NAs
set.seed(1)
selRow <- rnorm(nrow(dtaInp)) < 0
dtaInp[selRow,  "A1_1"] <- dtaInp[selRow, "A1"]
dtaInp[selRow,  "A1"]   <- NA
head(dtaInp[, c("A1", "A1_1")])
#>   A1 A1_1
#> 1 NA    2
#> 2  1   NA
#> 3 NA    1
#> 4  2   NA
#> 5  1   NA
#> 6 NA    4
dtaOut <- combine_cols_omv(dtaInp, varPrs = list(c("A1", "A1_1")))
# show the differences before and after combining the values in the columns and ensure
# that all values are the same as in the original data set
dtaInp[, "A1"]
#>   [1] NA  1 NA  2  1 NA  2  3  2 NA  1  1 NA NA  5 NA NA  2  1  1  3  2  4 NA  3
#>  [26] NA NA NA NA  2  2 NA  3 NA NA NA NA NA  5  4 NA NA  6  4 NA NA  4  2 NA  3
#>  [51]  1 NA  3 NA  5  1 NA NA  5 NA  1 NA  2  1 NA  3 NA  1  3  3  2 NA  2 NA NA
#>  [76]  3 NA  3  2 NA NA NA  1 NA  3  1  6 NA  1  1 NA  3  1  1  1  3 NA NA NA NA
#> [101] NA  6 NA  2 NA  3  4  2  2  3 NA NA  1 NA NA NA NA NA  2 NA NA  2 NA NA NA
#> [126]  1 NA NA NA NA  1 NA  1 NA  2 NA NA NA NA NA NA  4 NA NA NA NA  4  5 NA NA
#> [151]  1 NA NA NA NA NA  4 NA NA  1  4 NA  4  1 NA  1 NA NA NA  4  1  1  4 NA NA
#> [176] NA  6  1  4  3 NA  2  3 NA  2 NA  4 NA NA NA NA  1 NA  2 NA NA  1 NA  4 NA
#> [201]  1  4  1 NA NA  4  2  2 NA  1 NA  2 NA NA  4  5 NA NA  2 NA NA  1 NA NA NA
#> [226]  1 NA NA  5  2 NA NA NA  1 NA NA  3 NA  5 NA  1  3  2 NA  2 NA NA NA NA  1
#> attr(,"jmv-desc")
#> [1] "Am indifferent to the feelings of others. (reversed)"
dtaOut[, "A1"]
#>   [1] 2 1 1 2 1 4 2 3 2 1 1 1 1 1 5 2 2 2 1 1 3 2 4 1 3 1 6 1 1 2 2 2 3 3 4 3 4
#>  [38] 1 5 4 1 2 6 4 4 4 4 2 4 3 1 3 3 1 5 1 2 1 5 5 1 2 2 1 3 3 1 1 3 3 2 2 2 3
#>  [75] 2 3 4 3 2 3 1 1 1 2 3 1 6 1 1 1 5 3 1 1 1 3 4 4 6 1 1 6 5 2 3 3 4 2 2 3 2
#> [112] 2 1 2 1 3 1 6 2 2 3 2 1 5 5 1 2 2 1 2 1 2 1 2 2 2 3 1 2 1 3 4 1 1 1 1 4 5
#> [149] 3 2 1 2 2 2 2 2 4 2 1 1 4 5 4 1 5 1 4 2 2 4 1 1 4 5 5 3 6 1 4 3 4 2 3 2 2
#> [186] 6 4 2 2 5 3 1 3 2 2 1 1 1 4 1 1 4 1 2 3 4 2 2 1 1 2 2 6 3 4 5 4 2 2 3 4 1
#> [223] 1 2 3 1 5 2 5 2 2 1 1 1 2 1 3 4 5 2 1 3 2 4 2 4 1 1 2 1
#> attr(,"jmv-desc")
#> [1] "Am indifferent to the feelings of others. (reversed)"
all(dtaOut[, "A1"] == jmvReadWrite::bfi_sample2[, "A1"])
#> [1] TRUE

# create a new column, containing values that are different from the original variable
dtaInp <- jmvReadWrite::bfi_sample2
dtaInp[selRow,  "A1_1"] <- dtaInp[selRow,  "A1"] + 1
# [1] if mdeCmb is "none" (or if mdeCmb is not given - "none" is the default) an error would be
# thrown (therefore the next line is commented out)
# dtaOut <- combine_cols_omv(dtaInp, varPrs = list(c("A1", "A1_1")), mdeCmb = "none")
# [2] if mdeCmb is "first", missing values are replaced and values from the first column ("A1")
# take precedence if the values are unequal
dtaOut <- combine_cols_omv(dtaInp, varPrs = list(c("A1", "A1_1")), mdeCmb = "first")
head(cbind(dtaOut[, "A1"], dtaInp[, c("A1", "A1_1")]))
#>   dtaOut[, "A1"] A1 A1_1
#> 1              2  2    3
#> 2              1  1   NA
#> 3              1  1    2
#> 4              2  2   NA
#> 5              1  1   NA
#> 6              4  4    5
# [3] if mdeCmb is "second", missing values are replaced and values from the second column
# ("A1_1") take precedence if the values are unequal
dtaOut <- combine_cols_omv(dtaInp, varPrs = list(c("A1", "A1_1")), mdeCmb = "second")
head(cbind(dtaOut[, "A1"], dtaInp[, c("A1", "A1_1")]))
#>   dtaOut[, "A1"] A1 A1_1
#> 1              3  2    3
#> 2              1  1   NA
#> 3              2  1    2
#> 4              2  2   NA
#> 5              1  1   NA
#> 6              5  4    5

```
