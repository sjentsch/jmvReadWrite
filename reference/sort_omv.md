# Sort data (using one or more variables) in .omv-files for the statistical spreadsheet 'jamovi' (<https://www.jamovi.org>)

Sort data (using one or more variables) in .omv-files for the
statistical spreadsheet 'jamovi' (<https://www.jamovi.org>)

## Usage

``` r
sort_omv(
  dtaInp = NULL,
  fleOut = "",
  varSrt = c(),
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
  any supported file type, see Details below

- fleOut:

  Name of the data file to be written (including the path, if required;
  "FILE_OUT.omv"; default: ""); if empty, the resulting data frame is
  returned instead

- varSrt:

  Variable(s) that are used to sort the data frame (see Details;
  default: c())

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

a data frame (only returned if `fleOut` is empty) where the input data
set is sorted (according to the variables in `varSrt`)

## Details

- `varSrt` can be either a character or a character vector (with one or
  more variables respectively). The sorting order for a particular
  variable can be inverted with preceding the variable name with "-".
  Please note that this doesn't make sense and hence throws a warning
  for certain variable types (e.g., factors).

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

`sort_omv` internally uses the following functions for reading and
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
nmeInp <- system.file("extdata", "AlbumSales.omv", package = "jmvReadWrite")
nmeOut <- tempfile(fileext = ".omv")
jmvReadWrite::sort_omv(dtaInp = nmeInp, fleOut = nmeOut, varSrt = "Image")
dtaFrm <- jmvReadWrite::read_omv(nmeOut)
unlink(nmeOut)
cat(dtaFrm$Image)
#> 1 1 1 2 3 4 4 4 4 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 9 9 9 9 9 9 9 9 9 9 9 9 10
# shows that the variable "Image" is sorted in ascending order
cat(is.unsorted(dtaFrm$Image))
#> FALSE
# is.unsorted (which checks for whether the variable is NOT sorted) returns FALSE
jmvReadWrite::sort_omv(dtaInp = nmeInp, fleOut = nmeOut, varSrt = "-Image")
# variables can also be sorted in descending order by preceding them with "-"
dtaFrm <- jmvReadWrite::read_omv(nmeOut)
unlink(nmeOut)
cat(dtaFrm$Image)
#> 10 9 9 9 9 9 9 9 9 9 9 9 9 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 4 4 4 4 3 2 1 1 1
# shows that the variable "Image" is now sorted in descending order
cat(is.unsorted(dtaFrm$Image))
#> TRUE
# this first returns TRUE (the variable is not in ascending order, i.e., unsorted)
cat(is.unsorted(-dtaFrm$Image))
#> FALSE
# if the sign of the variable is changed, it returns FALSE (i.e., the variable is
# NOT unsorted)

```
