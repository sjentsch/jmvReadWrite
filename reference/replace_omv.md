# Search values in .omv-files for the statistical spreadsheet 'jamovi' (<https://www.jamovi.org>)

Search values in .omv-files for the statistical spreadsheet 'jamovi'
(<https://www.jamovi.org>)

## Usage

``` r
replace_omv(
  dtaInp = NULL,
  fleOut = "",
  rplLst = list(),
  whlTrm = TRUE,
  varInc = NULL,
  varExc = NULL,
  incNum = TRUE,
  incOrd = TRUE,
  incNom = TRUE,
  incID = TRUE,
  incCmp = TRUE,
  incRcd = TRUE,
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

  Name of the data set / file to be written (including the path, if
  required; "FILE_OUT.omv"; default: ""); if empty, the resulting data
  frame is returned instead.

- rplLst:

  A list where each entry is a vector (with length 2) containing the
  original value and the to-replace-value (default: list())

- whlTrm:

  Whether the search term (first entry in the vectors) must be found
  exactly (TRUE) or whether a partial match is sufficient (FALSE;
  default: TRUE)

- varInc:

  Names of variables (character vector) to be included in the
  replacement (default: NULL)

- varExc:

  Names of variables (character vector) to be excluded from the
  replacement (default: NULL)

- incNum:

  Whether to include continuous variables in the replacement (default:
  TRUE)

- incOrd:

  Whether to include ordinal variables in the replacement (default:
  TRUE)

- incNom:

  Whether to include nominal variables in the replacement (default:
  TRUE)

- incID:

  Whether to include ID variables in the replacement (default: TRUE)

- incCmp:

  Whether to include Computed variables in the replacement (default:
  TRUE)

- incRcd:

  Whether to include Recoded variables in the replacement (default:
  TRUE)

- psvAnl:

  Whether analyses that are contained in the input file shall be
  transferred to the output file (TRUE / FALSE; default: FALSE)

- usePkg:

  Name of the package: "foreign" or "haven" that shall be used to read
  SPSS, Stata, and SAS files; "foreign" is the default (it is included
  in base R), but "haven" is newer and more comprehensive; you may have
  to install using `install.packages("haven", dep = TRUE)`.

- selSet:

  Name of the object / data set that is to be selected from the
  workspace (only relevant when reading .RData-files which can contain
  several objects / data sets)

- ...:

  Additional arguments passed on to methods; see Details below

## Value

a data frame (only returned if `fleOut` is empty) with the replaced
values

## Details

- `rplLst` is a list. Each list entry contains a vector (with length 2),
  where the first entry is the original value, and the second entry is
  the value the original value is to be replaced with.

- `whlTrm` indicates whether partial matches of the original value(s)
  shall replaced (e.g., for original: 24 and replacement: 34, 241 will
  be changed into 341).

- `varInc` and `varExc` determine which variables are included or
  excluded from the replacement. If both are given, a warning is issued
  and `varInc` takes precedence. `varInc` makes that only in these
  variables, the replacement requested by `rplLst` is carried out, if
  `varExc` is given, for all variables of the input data set, except
  those defined in `varExc`, the replacement is carried out.

- The ellipsis-parameter (`...`) can be used to submit arguments /
  parameters to the functions that are used for reading the data. By
  clicking on the respective function under “See also”, you can get a
  more detailed overview over which parameters each of those functions
  take.

## See also

`replace_omv` internally uses the following functions for reading and
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
if (FALSE) { # \dontrun{
bfi_sample <- jmvReadWrite::bfi_sample
# the gender in the original data file is plural...
table(bfi_sample$gender)
# and shall be converted to singular
rplDF <- jmvReadWrite::replace_omv(dtaInp = bfi_sample,
           rplLst = list(c("Females", "Female"), c("Males", "Male")))
table(rplDF$gender)
# with giving an output file name, the data set is written
nmeOut <- tempfile(fileext = ".omv")
jmvReadWrite::replace_omv(bfi_sample, fleOut = nmeOut,
  rplLst = list(c("Females", "Female"), c("Males", "Male")))
file.exists(nmeOut)
rplDF <- jmvReadWrite::read_omv(nmeOut)
table(rplDF$gender)
unlink(nmeOut)
# it is sensible to check / search for the original values before running replace_omv
jmvReadWrite::search_omv(bfi_sample, 24, whlTrm = TRUE)
rplDF <- jmvReadWrite::replace_omv(bfi_sample, rplLst = list(c(24, NA)))
table(rplDF$age)
} # }
```
