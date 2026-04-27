# Convert data files (CSV, R, other statistics packages) into .omv-files for the statistical spreadsheet 'jamovi' (<https://www.jamovi.org>)

Convert data files (CSV, R, other statistics packages) into .omv-files
for the statistical spreadsheet 'jamovi' (<https://www.jamovi.org>)

## Usage

``` r
convert_to_omv(
  fleInp = "",
  fleOut = "",
  varSrt = c(),
  usePkg = c("foreign", "haven"),
  selSet = "",
  ...
)
```

## Arguments

- fleInp:

  Name (including the path, if required) of the data file to be read
  ("FILENAME.ext"; default: ""); supports CSV and R-files natively, or
  other file types if "foreign" or "haven" are installed, see Details
  below

- fleOut:

  Name (including the path, if required) of the data file to be written
  ("FILENAME.omv"; default: ""); if empty, the extension of fleInp is
  replaced with ".omv"

- varSrt:

  Variable(s) that are used to sort the data frame (see Details; if
  empty, the row order of the input file is kept; default: c())

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

the function doesn't have a return value (it returns NULL)

## Details

- In difference to the remaining helper functions, `convert_to_omv`
  doesn't accept a data frame as input and it neither does return a data
  frame if `fleOut` is left empty: If you want to write a data frame,
  use `write_omv`. If you want to have a data frame returned use
  `read_omv` for jamovi-files or any of the functions listed in the
  bullet point below for any other file type.

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

`convert_to_omv` internally uses the following functions for reading and
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
# Example 1: Convert from RDS
# (use ToothGrowth as example, save it as RDS)
nmeInp <- tempfile(fileext = ".rds")
nmeOut <- tempfile(fileext = ".omv")
saveRDS(jmvReadWrite::ToothGrowth, nmeInp)
jmvReadWrite::convert_to_omv(fleInp = nmeInp, fleOut = nmeOut)
cat(list.files(dirname(nmeOut), basename(nmeOut)))
#> file293870009e3f.omv
# -> "file[...].omv" ([...] contains a random combination of numbers / characters
cat(file.info(nmeOut)$size)
#> 2618
# -> 2448 (size may differ on different OSes)
cat(str(jmvReadWrite::read_omv(nmeOut, sveAtt = FALSE)))
#> 'data.frame':    60 obs. of  7 variables:
#>  $ ID    : chr  "1" "2" "3" "4" ...
#>   ..- attr(*, "jmv-id")= logi TRUE
#>   ..- attr(*, "missingValues")= list()
#>  $ supp  : Factor w/ 2 levels "OJ","VC": 2 2 2 2 2 2 2 2 2 2 ...
#>   ..- attr(*, "missingValues")= list()
#>  $ supp2 : Factor w/ 2 levels "1","2": 1 1 1 1 1 1 1 1 1 1 ...
#>   ..- attr(*, "values")= int [1:2] 1 2
#>   ..- attr(*, "jmv-desc")= chr "Transformation of the supplement type (factor to numerical: VC = 1; OJ = 2)"
#>   ..- attr(*, "missingValues")= list()
#>  $ dose  : num  0.5 0.5 0.5 0.5 0.5 0.5 0.5 0.5 0.5 0.5 ...
#>   ..- attr(*, "missingValues")= list()
#>  $ dose2 : Ord.factor w/ 3 levels "0.5"<"1.0"<"2.0": 1 1 1 1 1 1 1 1 1 1 ...
#>   ..- attr(*, "missingValues")= list()
#>  $ len   : num  4.2 11.5 7.3 5.8 6.4 10 11.2 11.2 5.2 7 ...
#>   ..- attr(*, "missingValues")= list()
#>  $ logLen: num  0.623 1.061 0.863 0.763 0.806 ...
#>   ..- attr(*, "jmv-desc")= chr "Natural logarithm of the tooth length (len)"
#>   ..- attr(*, "missingValues")= list()
# gives a overview of the dataframe (all columns and some attributes,
# sveAtt is intentionally set to FALSE to make the output not too overwhelming)
unlink(nmeInp)
unlink(nmeOut)

# Example 2: Convert from CSV
# (use ToothGrowth again as example, this time save it as CSV)
nmeInp <- tempfile(fileext = ".csv")
nmeOut <- tempfile(fileext = ".omv")
write.csv(jmvReadWrite::ToothGrowth, nmeInp)
jmvReadWrite::convert_to_omv(fleInp = nmeInp, fleOut = nmeOut)
cat(list.files(dirname(nmeOut), basename(nmeOut)))
#> file293810985f7a.omv
cat(file.info(nmeOut)$size)
#> 2274
# -> 2104 (size may differ acc. to OS; the size is smaller than for the RDS-file
# because CSV can store fewer attributes, e.g., labels)
cat(str(jmvReadWrite::read_omv(nmeOut, sveAtt = FALSE)))
#> 'data.frame':    60 obs. of  8 variables:
#>  $ X     : int  1 2 3 4 5 6 7 8 9 10 ...
#>   ..- attr(*, "missingValues")= list()
#>  $ ID    : int  1 2 3 4 5 6 7 8 9 10 ...
#>   ..- attr(*, "missingValues")= list()
#>  $ supp  : Factor w/ 2 levels "OJ","VC": 2 2 2 2 2 2 2 2 2 2 ...
#>   ..- attr(*, "missingValues")= list()
#>  $ supp2 : int  1 1 1 1 1 1 1 1 1 1 ...
#>   ..- attr(*, "missingValues")= list()
#>  $ dose  : num  0.5 0.5 0.5 0.5 0.5 0.5 0.5 0.5 0.5 0.5 ...
#>   ..- attr(*, "missingValues")= list()
#>  $ dose2 : num  0.5 0.5 0.5 0.5 0.5 0.5 0.5 0.5 0.5 0.5 ...
#>   ..- attr(*, "missingValues")= list()
#>  $ len   : num  4.2 11.5 7.3 5.8 6.4 10 11.2 11.2 5.2 7 ...
#>   ..- attr(*, "missingValues")= list()
#>  $ logLen: num  0.623 1.061 0.863 0.763 0.806 ...
#>   ..- attr(*, "missingValues")= list()
# gives a overview of the dataframe (all columns and some attributes)
unlink(nmeInp)
unlink(nmeOut)

```
