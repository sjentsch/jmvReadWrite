# Re-arrange columns / variables in .omv-files for the statistical spreadsheet 'jamovi' (<https://www.jamovi.org>)

Re-arrange columns / variables in .omv-files for the statistical
spreadsheet 'jamovi' (<https://www.jamovi.org>)

## Usage

``` r
arrange_cols_omv(
  dtaInp = NULL,
  fleOut = "",
  varOrd = c(),
  varMve = list(),
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

- varOrd:

  Character vector with the desired order of variable(s) in the data
  frame (see Details; default: c())

- varMve:

  Named list defining to how much a particular variable (name of a list
  entry) should be moved up (neg. value of a list entry) or down (pos.
  value) in the data frame (see Details; default: c())

- psvAnl:

  Whether analyses that are contained in the input file shall be
  transferred to the output file (default: FALSE)

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

a data frame (only returned if `fleOut` is empty) where the order of
variables / columns of the input data set is re-arranged

## Details

- `varOrd` is a character vector. If not all variables of the original
  data set are contained in `varOrd`, a warning is issued but otherwise
  the list of variables defined in `varOrd` is used (removing variables
  not contained in `varOrd`).

- `varMve` is a named list. For example would `list(VARNAME = -3)` move
  the variable `VARNAME` three positions up in the list of variables
  (towards the first column), and `list(VARNAME = 3)` would move it
  three positions down (towards the last column). If the number of steps
  the variable is to be moved leads to the position being either lower
  than the first or higher than the total number of variables in the
  data set, an error message is issued. Please note that the list
  entries are processed one after another, that is, for a second list
  entry, you have to consider how the first list entry may have changed
  to order of variables.

- Using `varOrd` makes more sense for changing the position of several
  variables, whereas using `varMve` makes more sense for one variable.
  If both parameters are given, a warning is issued and `varOrd` takes
  precedence.

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

`arrange_cols_omv` internally uses the following functions for reading
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
nmeInp <- system.file("extdata", "AlbumSales.omv", package = "jmvReadWrite")
nmeOut <- tempfile(fileext = ".omv")
# the original file has the variables in the order: "Adverts", "Airplay", "Image", "Sales"
names(read_omv(nmeInp))
#> [1] "Adverts" "Airplay" "Image"   "Sales"  
# first, we move the variable "Sales" to the first place using the varOrd-parameter
jmvReadWrite::arrange_cols_omv(dtaInp = nmeInp, fleOut = nmeOut,
  varOrd = c("Sales", "Adverts", "Airplay", "Image"))
names(jmvReadWrite::read_omv(nmeOut))
#> [1] "Sales"   "Adverts" "Airplay" "Image"  
unlink(nmeOut)
# now, we move the variable "Sales" to the first place using the varMve-parameter
jmvReadWrite::arrange_cols_omv(dtaInp = nmeInp, fleOut = nmeOut, varMve = list(Sales = -3))
names(jmvReadWrite::read_omv(nmeOut))
#> [1] "Sales"   "Adverts" "Airplay" "Image"  
unlink(nmeOut)

```
