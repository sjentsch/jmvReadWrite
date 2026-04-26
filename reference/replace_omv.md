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
  varInc = c(),
  varExc = c(),
  incNum = TRUE,
  incOrd = TRUE,
  incNom = TRUE,
  incID = TRUE,
  incCmp = TRUE,
  incRcd = TRUE,
  psvAnl = FALSE,
  ...
)
```

## Arguments

- dtaInp:

  Either a data frame or the name of a jamovi data file to be read
  (including the path, if required; "FILENAME.omv"; default: NULL)

- fleOut:

  Name of the data file to be written (including the path, if required;
  "FILE_OUT.omv"; default: ""); if empty, the resulting data frame is
  returned instead

- rplLst:

  A list where each entry is a vector (with length 2) containing the
  original value and the to-replace-value (default: list())

- whlTrm:

  Whether the search term (first entry in the vectors) must be found
  exactly (TRUE) or whether a partial match is sufficient (FALSE;
  default: TRUE)

- varInc:

  Names of variables (character vector) to be included in the
  replacement (default: c())

- varExc:

  Names of variables (character vector) to be excluded from the
  replacement (default: c())

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
  transferred to the output file (default: FALSE)

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
  parameters to the function that is used for reading and writing the
  data. Clicking on the respective function under “See also”, you can
  get a more detailed overview over which parameters each of those
  functions take. The functions are: `read_omv` and `write_omv` (for
  jamovi-files).

## See also

`replace_omv` uses
[`read_omv()`](https://sjentsch.github.io/jmvReadWrite/reference/read_omv.md)
and
[`write_omv()`](https://sjentsch.github.io/jmvReadWrite/reference/write_omv.md)
for reading and writing jamovi-files.

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
