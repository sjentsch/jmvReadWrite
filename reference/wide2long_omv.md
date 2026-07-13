# Converts .omv-files for the statistical spreadsheet 'jamovi' (<https://www.jamovi.org>) from wide to long format

Converts .omv-files for the statistical spreadsheet 'jamovi'
(<https://www.jamovi.org>) from wide to long format

## Usage

``` r
wide2long_omv(
  dtaInp = NULL,
  fleOut = "",
  varLst = c(),
  varExc = c(),
  varID = NULL,
  varTme = "cond",
  varSep = "_",
  varOrd = TRUE,
  varSrt = c(),
  excLvl = NULL,
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

- varLst:

  List / set of variables that are to be transformed into single
  (time-varying) variables in long format (default: c())

- varExc:

  Name of the variable(s) should be excluded from the transformation,
  typically this will be between-subject-variable(s) (default: c())

- varID:

  Name(s) of one or more variables that (is created to) identify the
  same group / individual (if empty, "ID" is added with row numbers
  identifying cases; default: NULL)

- varTme:

  Name of the variable (or vector with variable names) that (is created
  to) differentiate multiple records from the same group / individual
  \#' (default: "cond"; if required, a counter is added for each
  time-varying part)

- varSep:

  Character that separates the variables in varLst into a time-varying
  part and a part that forms the variable name in long format ("*" in
  "VAR_1", "VAR_2", default: "*")

- varOrd:

  Whether to arrange the variables before the transformation, so that
  they are in accordance with the different split levels (default: TRUE)

- varSrt:

  Variable(s) that are used to sort the data frame (see Details; if
  empty, the order returned from reshape is kept; default: c())

- excLvl:

  Integer (or vector of integers) determining which parts of the
  variable names in varLst shall not be transformed (default: NULL), see
  Details below

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
set is converted from wide to long format

## Details

- If `varLst` is empty, it is tried to generate it using all variables
  in the data frame except those defined by `varExc` and `varID`. The
  variable(s) in `varID` need to be unique identifiers (in the original
  dataset), those in `varExc` don't have this requirement. It is
  recommended that the variable names in `varExc` and `varID` should not
  contain the variable separator (defined in `varSep`; default: "\_").

- `varOrd` determines whether the variables are rearranged to match the
  order of split levels. Consider the `varLst` X_1, Y_1, X_2, Y_2. If
  `varOrd` were set to FALSE, the original order would be preserved and
  the second part of the variable name (1, 2, ...) would become
  condition 1, and the first part condition 2. In most cases, leaving
  `varOrd` set to TRUE is recommended.

- `varSrt` can be either a character or a character vector (with one or
  more variables respectively). The sorting order for a particular
  variable can be inverted with preceding the variable name with "-".
  Please note that this doesn't make sense and hence throws a warning
  for certain variable types (e.g., factors).

- `exclLvl` points to a part of the variable names in `varLst` to be
  excluded. For example, if the variable name is `PART1_PART2_PART3`
  (split at \_), then `excLvl` = 1 would exclude PART1 from the
  transformation. Quite often, one has more that one variable of a
  particular type (e.g., responses, reaction times, etc.). Those would
  typically be the first part of each variable name in `varLst` (the
  conditions then being PART2, PART3, and so on). `excLvl` = 1 would
  exclude those variable types / categories from being transformed into
  long (i.e., they would be kept as separate columns).

- The ellipsis-parameter (`...`) can be used to submit arguments /
  parameters to the functions that are used for transforming or reading
  the data. By clicking on the respective function under “See also”, you
  can get a more detailed overview over which parameters each of those
  functions take.

- The transformation from long to wide uses `reshape`: `varID` matches
  (~) `idvar` in `reshape`, `varTme` ~ `timevar`, `varLst` ~ `varying`,
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
if (FALSE) { # \dontrun{
# generate a test dataframe with 100 (imaginary) participants / units of
# observation (ID), and 8 repeated measurements of variable (X_1, X_2, ...)
dtaInp <- cbind(data.frame(ID = as.character(seq(1:100))),
                stats::setNames(
                    as.data.frame(matrix(runif(800, -10, 10), nrow = 100)),
                    paste0("X_", 1:8)))
cat(str(dtaInp))
# 'data.frame':  100 obs. of  9 variables:
#  $ ID : chr  "1" "2" "3" "4" ...
#  $ X_1: num  ...
#  $ X_2: num  ...
#  $ X_3: num  ...
#  $ X_4: num  ...
#  $ X_5: num  ...
#  $ X_6: num  ...
#  $ X_7: num  ...
#  $ X_8: num  ...
# this data set is stored as (temporary) RDS-file and later processed by wide2long
nmeInp <- tempfile(fileext = ".rds")
nmeOut <- tempfile(fileext = ".omv")
saveRDS(dtaInp, nmeInp)
jmvReadWrite::wide2long_omv(dtaInp = nmeInp, fleOut = nmeOut, varID = "ID",
    varTme = "measure", varLst = setdiff(names(dtaInp), "ID"),
    varSrt = c("ID", "measure"))
# it is required to give at least the arguments dtaInp (if dtaInp is a data frame,
# fleOut needs to be provided too) and varID
# "reshape" then assigns all variables expect the variable defined by varID to
# varLst (but throws a warning)
# varSrt enforces sorting the data set after the transformation (sorted, the
# measurements within one person come after another; unsorted all measurements
# for one repetition would come after another)

# check whether the file was created and its size
cat(list.files(dirname(nmeOut), basename(nmeOut)))
# -> "file[...].omv" ([...] contains a random combination of numbers / characters
cat(file.info(nmeOut)$size)
# -> 6939 (approximate size; size may differ in every run [in dependence of how
#          well the generated random data can be compressed])
cat(str(jmvReadWrite::read_omv(nmeOut, sveAtt = FALSE)))
# the data set is now transformed into long (and each the measurements is now
# indicated by the "measure")
# 'data.frame':  800 obs. of  3 variables:
#  $ ID     : Factor w/ 100 levels "1","2","3","4",..: 1 1 1 1 1 1 1 1 2 2 ...
#   ..- attr(*, "missingValues")= list()
#  $ measure: Factor w/ 8 levels "1","2","3","4",..: 1 2 3 4 5 6 7 8 1 2 ...
#   ..- attr(*, "missingValues")= list()
#  $ X      : num  ...
#   ..- attr(*, "missingValues")= list()

unlink(nmeInp)
unlink(nmeOut)
} # }
```
