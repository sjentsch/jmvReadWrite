# Transpose .omv-files for the statistical spreadsheet 'jamovi' (<https://www.jamovi.org>)

Transpose .omv-files for the statistical spreadsheet 'jamovi'
(<https://www.jamovi.org>)

## Usage

``` r
transpose_omv(
  dtaInp = NULL,
  fleOut = "",
  varNme = "",
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

- varNme:

  Name of the variables in the output data frame; see Details below

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
set is transposed

## Details

- If `varNme` empty, the row names of the input data set are used
  (preceded by "V\_" if all row names are numbers); if `varNme` has
  length 1, then it is supposed to point to a variable in the input data
  frame; if `varNme` has the same length as the number of rows in the
  input data frame, then the values in `varNme` are assigned as column
  names to the output data frame.

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

`transpose_omv` internally uses the following functions for reading and
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
set.seed(1)
tmpDF <- stats::setNames(as.data.frame(matrix(sample(6, 1200, replace = TRUE), nrow = 16)),
                         sprintf("sbj_%03d", seq(75)))
str(tmpDF)
#> 'data.frame':    16 obs. of  75 variables:
#>  $ sbj_001: int  1 4 1 2 5 3 6 2 3 3 ...
#>  $ sbj_002: int  2 1 5 5 1 1 6 5 5 2 ...
#>  $ sbj_003: int  3 6 2 2 6 4 4 4 2 4 ...
#>  $ sbj_004: int  2 3 2 6 6 2 5 2 6 6 ...
#>  $ sbj_005: int  6 3 1 4 5 1 1 6 4 5 ...
#>  $ sbj_006: int  1 5 5 6 1 1 3 6 2 2 ...
#>  $ sbj_007: int  2 2 1 3 3 2 2 5 2 5 ...
#>  $ sbj_008: int  2 3 3 1 5 6 6 6 4 4 ...
#>  $ sbj_009: int  6 3 6 3 3 4 1 1 4 2 ...
#>  $ sbj_010: int  3 5 3 4 2 1 4 1 4 2 ...
#>  $ sbj_011: int  2 3 3 3 6 3 5 3 1 2 ...
#>  $ sbj_012: int  5 5 5 4 6 6 4 6 4 2 ...
#>  $ sbj_013: int  5 1 3 4 5 4 1 1 5 5 ...
#>  $ sbj_014: int  1 5 5 6 6 2 6 6 5 2 ...
#>  $ sbj_015: int  4 4 1 2 4 1 1 3 1 1 ...
#>  $ sbj_016: int  3 5 1 1 1 6 5 4 4 4 ...
#>  $ sbj_017: int  4 6 4 5 4 2 2 2 1 1 ...
#>  $ sbj_018: int  5 4 6 1 2 1 5 2 3 3 ...
#>  $ sbj_019: int  3 2 4 3 3 6 6 1 4 5 ...
#>  $ sbj_020: int  1 2 5 1 3 2 4 1 3 1 ...
#>  $ sbj_021: int  3 2 6 4 1 1 6 1 3 3 ...
#>  $ sbj_022: int  2 4 1 6 1 1 5 6 6 1 ...
#>  $ sbj_023: int  4 4 5 4 1 5 6 5 2 4 ...
#>  $ sbj_024: int  1 3 3 4 6 1 1 6 3 4 ...
#>  $ sbj_025: int  1 5 3 5 4 1 1 5 5 3 ...
#>  $ sbj_026: int  6 5 2 2 6 2 5 1 1 5 ...
#>  $ sbj_027: int  1 1 4 6 2 4 5 2 4 4 ...
#>  $ sbj_028: int  1 5 5 1 4 2 6 3 2 5 ...
#>  $ sbj_029: int  3 4 1 3 6 6 3 5 4 3 ...
#>  $ sbj_030: int  2 2 6 5 2 6 2 5 3 1 ...
#>  $ sbj_031: int  3 4 6 2 6 1 6 1 4 5 ...
#>  $ sbj_032: int  6 3 4 4 4 4 3 4 6 2 ...
#>  $ sbj_033: int  3 5 4 4 2 5 5 6 2 5 ...
#>  $ sbj_034: int  3 4 4 2 4 5 1 5 6 5 ...
#>  $ sbj_035: int  3 6 4 1 6 4 6 1 3 3 ...
#>  $ sbj_036: int  2 3 4 6 1 3 3 2 5 1 ...
#>  $ sbj_037: int  3 5 3 2 6 6 1 5 4 4 ...
#>  $ sbj_038: int  3 2 5 2 5 5 6 1 5 6 ...
#>  $ sbj_039: int  1 2 3 1 4 5 4 1 5 3 ...
#>  $ sbj_040: int  4 5 6 4 2 4 2 3 4 2 ...
#>  $ sbj_041: int  3 4 6 5 4 5 3 2 5 1 ...
#>  $ sbj_042: int  4 2 2 1 3 1 3 5 2 4 ...
#>  $ sbj_043: int  4 6 5 3 2 2 3 1 1 4 ...
#>  $ sbj_044: int  4 4 6 6 6 3 1 4 4 4 ...
#>  $ sbj_045: int  6 1 2 1 1 1 3 6 1 3 ...
#>  $ sbj_046: int  4 1 2 3 6 5 5 2 1 2 ...
#>  $ sbj_047: int  4 6 2 5 1 6 2 6 5 1 ...
#>  $ sbj_048: int  3 5 3 3 4 5 5 5 5 3 ...
#>  $ sbj_049: int  3 5 3 5 2 5 4 6 2 6 ...
#>  $ sbj_050: int  6 2 5 2 5 5 6 1 1 6 ...
#>  $ sbj_051: int  1 6 1 5 2 5 3 4 3 3 ...
#>  $ sbj_052: int  2 1 6 5 5 4 3 1 2 4 ...
#>  $ sbj_053: int  6 2 1 3 5 1 1 6 4 6 ...
#>  $ sbj_054: int  2 6 1 2 6 5 4 4 2 2 ...
#>  $ sbj_055: int  4 1 3 6 5 3 5 6 1 2 ...
#>  $ sbj_056: int  6 3 2 2 3 3 3 6 6 6 ...
#>  $ sbj_057: int  2 3 2 1 3 4 2 4 4 1 ...
#>  $ sbj_058: int  2 4 3 6 4 3 4 5 5 4 ...
#>  $ sbj_059: int  6 3 5 4 4 6 6 3 2 5 ...
#>  $ sbj_060: int  4 3 6 2 5 1 1 5 3 3 ...
#>  $ sbj_061: int  5 4 4 1 3 4 2 2 5 1 ...
#>  $ sbj_062: int  1 1 1 6 4 2 4 2 1 4 ...
#>  $ sbj_063: int  2 3 4 3 5 3 1 4 4 6 ...
#>  $ sbj_064: int  3 3 3 2 2 2 6 4 6 2 ...
#>  $ sbj_065: int  1 6 6 5 2 5 1 1 5 3 ...
#>  $ sbj_066: int  3 2 4 6 1 1 6 3 2 2 ...
#>  $ sbj_067: int  3 5 1 3 4 5 3 4 3 2 ...
#>  $ sbj_068: int  3 3 2 2 1 5 4 2 2 3 ...
#>  $ sbj_069: int  6 5 5 1 6 1 6 2 3 2 ...
#>  $ sbj_070: int  4 1 6 4 4 1 1 1 2 1 ...
#>  $ sbj_071: int  2 6 5 1 4 4 2 5 6 2 ...
#>  $ sbj_072: int  6 1 2 1 3 2 3 6 2 1 ...
#>  $ sbj_073: int  4 2 6 5 6 5 4 3 6 5 ...
#>  $ sbj_074: int  3 5 2 5 1 3 3 3 2 3 ...
#>  $ sbj_075: int  5 5 5 5 1 5 3 1 6 1 ...
# Data sets that were extracted, e.g., from PsychoPy, may look like this (trials as rows
# and participants as columns, one for each participant, manually assembled / copy-and-pasted).
# However, for analyses, one wants the data set transposed (units / participants as columns)...
nmeOut <- tempfile(fileext = ".omv")
jmvReadWrite::transpose_omv(dtaInp = tmpDF, fleOut = nmeOut)
dtaFrm <- jmvReadWrite::read_omv(nmeOut)
unlink(nmeOut)
str(dtaFrm)
#> 'data.frame':    75 obs. of  17 variables:
#>  $ ID  : chr  "sbj_001" "sbj_002" "sbj_003" "sbj_004" ...
#>   ..- attr(*, "jmv-id")= logi TRUE
#>   ..- attr(*, "name")= chr "ID"
#>   ..- attr(*, "id")= int 1
#>   ..- attr(*, "columnType")= chr "Data"
#>   ..- attr(*, "dataType")= chr "Text"
#>   ..- attr(*, "measureType")= chr "ID"
#>   ..- attr(*, "formula")= chr ""
#>   ..- attr(*, "formulaMessage")= chr ""
#>   ..- attr(*, "parentId")= int 0
#>   ..- attr(*, "width")= int 100
#>   ..- attr(*, "type")= chr "string"
#>   ..- attr(*, "importName")= chr ""
#>   ..- attr(*, "description")= chr ""
#>   ..- attr(*, "transform")= int 0
#>   ..- attr(*, "edits")= list()
#>   ..- attr(*, "missingValues")= list()
#>  $ V_1 : int  1 2 3 2 6 1 2 2 6 3 ...
#>   ..- attr(*, "name")= chr "V_1"
#>   ..- attr(*, "id")= int 2
#>   ..- attr(*, "columnType")= chr "Data"
#>   ..- attr(*, "dataType")= chr "Integer"
#>   ..- attr(*, "measureType")= chr "Continuous"
#>   ..- attr(*, "formula")= chr ""
#>   ..- attr(*, "formulaMessage")= chr ""
#>   ..- attr(*, "parentId")= int 0
#>   ..- attr(*, "width")= int 100
#>   ..- attr(*, "type")= chr "integer"
#>   ..- attr(*, "importName")= chr ""
#>   ..- attr(*, "description")= chr ""
#>   ..- attr(*, "transform")= int 0
#>   ..- attr(*, "edits")= list()
#>   ..- attr(*, "missingValues")= list()
#>  $ V_2 : int  4 1 6 3 3 5 2 3 3 5 ...
#>   ..- attr(*, "name")= chr "V_2"
#>   ..- attr(*, "id")= int 3
#>   ..- attr(*, "columnType")= chr "Data"
#>   ..- attr(*, "dataType")= chr "Integer"
#>   ..- attr(*, "measureType")= chr "Continuous"
#>   ..- attr(*, "formula")= chr ""
#>   ..- attr(*, "formulaMessage")= chr ""
#>   ..- attr(*, "parentId")= int 0
#>   ..- attr(*, "width")= int 100
#>   ..- attr(*, "type")= chr "integer"
#>   ..- attr(*, "importName")= chr ""
#>   ..- attr(*, "description")= chr ""
#>   ..- attr(*, "transform")= int 0
#>   ..- attr(*, "edits")= list()
#>   ..- attr(*, "missingValues")= list()
#>  $ V_3 : int  1 5 2 2 1 5 1 3 6 3 ...
#>   ..- attr(*, "name")= chr "V_3"
#>   ..- attr(*, "id")= int 4
#>   ..- attr(*, "columnType")= chr "Data"
#>   ..- attr(*, "dataType")= chr "Integer"
#>   ..- attr(*, "measureType")= chr "Continuous"
#>   ..- attr(*, "formula")= chr ""
#>   ..- attr(*, "formulaMessage")= chr ""
#>   ..- attr(*, "parentId")= int 0
#>   ..- attr(*, "width")= int 100
#>   ..- attr(*, "type")= chr "integer"
#>   ..- attr(*, "importName")= chr ""
#>   ..- attr(*, "description")= chr ""
#>   ..- attr(*, "transform")= int 0
#>   ..- attr(*, "edits")= list()
#>   ..- attr(*, "missingValues")= list()
#>  $ V_4 : int  2 5 2 6 4 6 3 1 3 4 ...
#>   ..- attr(*, "name")= chr "V_4"
#>   ..- attr(*, "id")= int 5
#>   ..- attr(*, "columnType")= chr "Data"
#>   ..- attr(*, "dataType")= chr "Integer"
#>   ..- attr(*, "measureType")= chr "Continuous"
#>   ..- attr(*, "formula")= chr ""
#>   ..- attr(*, "formulaMessage")= chr ""
#>   ..- attr(*, "parentId")= int 0
#>   ..- attr(*, "width")= int 100
#>   ..- attr(*, "type")= chr "integer"
#>   ..- attr(*, "importName")= chr ""
#>   ..- attr(*, "description")= chr ""
#>   ..- attr(*, "transform")= int 0
#>   ..- attr(*, "edits")= list()
#>   ..- attr(*, "missingValues")= list()
#>  $ V_5 : int  5 1 6 6 5 1 3 5 3 2 ...
#>   ..- attr(*, "name")= chr "V_5"
#>   ..- attr(*, "id")= int 6
#>   ..- attr(*, "columnType")= chr "Data"
#>   ..- attr(*, "dataType")= chr "Integer"
#>   ..- attr(*, "measureType")= chr "Continuous"
#>   ..- attr(*, "formula")= chr ""
#>   ..- attr(*, "formulaMessage")= chr ""
#>   ..- attr(*, "parentId")= int 0
#>   ..- attr(*, "width")= int 100
#>   ..- attr(*, "type")= chr "integer"
#>   ..- attr(*, "importName")= chr ""
#>   ..- attr(*, "description")= chr ""
#>   ..- attr(*, "transform")= int 0
#>   ..- attr(*, "edits")= list()
#>   ..- attr(*, "missingValues")= list()
#>  $ V_6 : int  3 1 4 2 1 1 2 6 4 1 ...
#>   ..- attr(*, "name")= chr "V_6"
#>   ..- attr(*, "id")= int 7
#>   ..- attr(*, "columnType")= chr "Data"
#>   ..- attr(*, "dataType")= chr "Integer"
#>   ..- attr(*, "measureType")= chr "Continuous"
#>   ..- attr(*, "formula")= chr ""
#>   ..- attr(*, "formulaMessage")= chr ""
#>   ..- attr(*, "parentId")= int 0
#>   ..- attr(*, "width")= int 100
#>   ..- attr(*, "type")= chr "integer"
#>   ..- attr(*, "importName")= chr ""
#>   ..- attr(*, "description")= chr ""
#>   ..- attr(*, "transform")= int 0
#>   ..- attr(*, "edits")= list()
#>   ..- attr(*, "missingValues")= list()
#>  $ V_7 : int  6 6 4 5 1 3 2 6 1 4 ...
#>   ..- attr(*, "name")= chr "V_7"
#>   ..- attr(*, "id")= int 8
#>   ..- attr(*, "columnType")= chr "Data"
#>   ..- attr(*, "dataType")= chr "Integer"
#>   ..- attr(*, "measureType")= chr "Continuous"
#>   ..- attr(*, "formula")= chr ""
#>   ..- attr(*, "formulaMessage")= chr ""
#>   ..- attr(*, "parentId")= int 0
#>   ..- attr(*, "width")= int 100
#>   ..- attr(*, "type")= chr "integer"
#>   ..- attr(*, "importName")= chr ""
#>   ..- attr(*, "description")= chr ""
#>   ..- attr(*, "transform")= int 0
#>   ..- attr(*, "edits")= list()
#>   ..- attr(*, "missingValues")= list()
#>  $ V_8 : int  2 5 4 2 6 6 5 6 1 1 ...
#>   ..- attr(*, "name")= chr "V_8"
#>   ..- attr(*, "id")= int 9
#>   ..- attr(*, "columnType")= chr "Data"
#>   ..- attr(*, "dataType")= chr "Integer"
#>   ..- attr(*, "measureType")= chr "Continuous"
#>   ..- attr(*, "formula")= chr ""
#>   ..- attr(*, "formulaMessage")= chr ""
#>   ..- attr(*, "parentId")= int 0
#>   ..- attr(*, "width")= int 100
#>   ..- attr(*, "type")= chr "integer"
#>   ..- attr(*, "importName")= chr ""
#>   ..- attr(*, "description")= chr ""
#>   ..- attr(*, "transform")= int 0
#>   ..- attr(*, "edits")= list()
#>   ..- attr(*, "missingValues")= list()
#>  $ V_9 : int  3 5 2 6 4 2 2 4 4 4 ...
#>   ..- attr(*, "name")= chr "V_9"
#>   ..- attr(*, "id")= int 10
#>   ..- attr(*, "columnType")= chr "Data"
#>   ..- attr(*, "dataType")= chr "Integer"
#>   ..- attr(*, "measureType")= chr "Continuous"
#>   ..- attr(*, "formula")= chr ""
#>   ..- attr(*, "formulaMessage")= chr ""
#>   ..- attr(*, "parentId")= int 0
#>   ..- attr(*, "width")= int 100
#>   ..- attr(*, "type")= chr "integer"
#>   ..- attr(*, "importName")= chr ""
#>   ..- attr(*, "description")= chr ""
#>   ..- attr(*, "transform")= int 0
#>   ..- attr(*, "edits")= list()
#>   ..- attr(*, "missingValues")= list()
#>  $ V_10: int  3 2 4 6 5 2 5 4 2 2 ...
#>   ..- attr(*, "name")= chr "V_10"
#>   ..- attr(*, "id")= int 11
#>   ..- attr(*, "columnType")= chr "Data"
#>   ..- attr(*, "dataType")= chr "Integer"
#>   ..- attr(*, "measureType")= chr "Continuous"
#>   ..- attr(*, "formula")= chr ""
#>   ..- attr(*, "formulaMessage")= chr ""
#>   ..- attr(*, "parentId")= int 0
#>   ..- attr(*, "width")= int 100
#>   ..- attr(*, "type")= chr "integer"
#>   ..- attr(*, "importName")= chr ""
#>   ..- attr(*, "description")= chr ""
#>   ..- attr(*, "transform")= int 0
#>   ..- attr(*, "edits")= list()
#>   ..- attr(*, "missingValues")= list()
#>  $ V_11: int  1 2 1 6 5 3 4 1 6 5 ...
#>   ..- attr(*, "name")= chr "V_11"
#>   ..- attr(*, "id")= int 12
#>   ..- attr(*, "columnType")= chr "Data"
#>   ..- attr(*, "dataType")= chr "Integer"
#>   ..- attr(*, "measureType")= chr "Continuous"
#>   ..- attr(*, "formula")= chr ""
#>   ..- attr(*, "formulaMessage")= chr ""
#>   ..- attr(*, "parentId")= int 0
#>   ..- attr(*, "width")= int 100
#>   ..- attr(*, "type")= chr "integer"
#>   ..- attr(*, "importName")= chr ""
#>   ..- attr(*, "description")= chr ""
#>   ..- attr(*, "transform")= int 0
#>   ..- attr(*, "edits")= list()
#>   ..- attr(*, "missingValues")= list()
#>  $ V_12: int  5 6 6 1 4 6 5 5 1 2 ...
#>   ..- attr(*, "name")= chr "V_12"
#>   ..- attr(*, "id")= int 13
#>   ..- attr(*, "columnType")= chr "Data"
#>   ..- attr(*, "dataType")= chr "Integer"
#>   ..- attr(*, "measureType")= chr "Continuous"
#>   ..- attr(*, "formula")= chr ""
#>   ..- attr(*, "formulaMessage")= chr ""
#>   ..- attr(*, "parentId")= int 0
#>   ..- attr(*, "width")= int 100
#>   ..- attr(*, "type")= chr "integer"
#>   ..- attr(*, "importName")= chr ""
#>   ..- attr(*, "description")= chr ""
#>   ..- attr(*, "transform")= int 0
#>   ..- attr(*, "edits")= list()
#>   ..- attr(*, "missingValues")= list()
#>  $ V_13: int  5 1 1 3 6 2 4 5 2 2 ...
#>   ..- attr(*, "name")= chr "V_13"
#>   ..- attr(*, "id")= int 14
#>   ..- attr(*, "columnType")= chr "Data"
#>   ..- attr(*, "dataType")= chr "Integer"
#>   ..- attr(*, "measureType")= chr "Continuous"
#>   ..- attr(*, "formula")= chr ""
#>   ..- attr(*, "formulaMessage")= chr ""
#>   ..- attr(*, "parentId")= int 0
#>   ..- attr(*, "width")= int 100
#>   ..- attr(*, "type")= chr "integer"
#>   ..- attr(*, "importName")= chr ""
#>   ..- attr(*, "description")= chr ""
#>   ..- attr(*, "transform")= int 0
#>   ..- attr(*, "edits")= list()
#>   ..- attr(*, "missingValues")= list()
#>  $ V_14: int  2 4 4 3 5 4 6 6 3 2 ...
#>   ..- attr(*, "name")= chr "V_14"
#>   ..- attr(*, "id")= int 15
#>   ..- attr(*, "columnType")= chr "Data"
#>   ..- attr(*, "dataType")= chr "Integer"
#>   ..- attr(*, "measureType")= chr "Continuous"
#>   ..- attr(*, "formula")= chr ""
#>   ..- attr(*, "formulaMessage")= chr ""
#>   ..- attr(*, "parentId")= int 0
#>   ..- attr(*, "width")= int 100
#>   ..- attr(*, "type")= chr "integer"
#>   ..- attr(*, "importName")= chr ""
#>   ..- attr(*, "description")= chr ""
#>   ..- attr(*, "transform")= int 0
#>   ..- attr(*, "edits")= list()
#>   ..- attr(*, "missingValues")= list()
#>  $ V_15: int  6 1 1 6 4 3 1 1 4 3 ...
#>   ..- attr(*, "name")= chr "V_15"
#>   ..- attr(*, "id")= int 16
#>   ..- attr(*, "columnType")= chr "Data"
#>   ..- attr(*, "dataType")= chr "Integer"
#>   ..- attr(*, "measureType")= chr "Continuous"
#>   ..- attr(*, "formula")= chr ""
#>   ..- attr(*, "formulaMessage")= chr ""
#>   ..- attr(*, "parentId")= int 0
#>   ..- attr(*, "width")= int 100
#>   ..- attr(*, "type")= chr "integer"
#>   ..- attr(*, "importName")= chr ""
#>   ..- attr(*, "description")= chr ""
#>   ..- attr(*, "transform")= int 0
#>   ..- attr(*, "edits")= list()
#>   ..- attr(*, "missingValues")= list()
#>  $ V_16: int  6 4 6 4 4 5 3 3 1 1 ...
#>   ..- attr(*, "name")= chr "V_16"
#>   ..- attr(*, "id")= int 17
#>   ..- attr(*, "columnType")= chr "Data"
#>   ..- attr(*, "dataType")= chr "Integer"
#>   ..- attr(*, "measureType")= chr "Continuous"
#>   ..- attr(*, "formula")= chr ""
#>   ..- attr(*, "formulaMessage")= chr ""
#>   ..- attr(*, "parentId")= int 0
#>   ..- attr(*, "width")= int 100
#>   ..- attr(*, "type")= chr "integer"
#>   ..- attr(*, "importName")= chr ""
#>   ..- attr(*, "description")= chr ""
#>   ..- attr(*, "transform")= int 0
#>   ..- attr(*, "edits")= list()
#>   ..- attr(*, "missingValues")= list()
#>  - attr(*, "removedRows")= list()
#>  - attr(*, "addedRows")= list()
#>  - attr(*, "transforms")= list()
# if no varNme-parameter is given, generic variable names are created (V_...)
jmvReadWrite::transpose_omv(dtaInp = tmpDF, fleOut = nmeOut, varNme = sprintf("Trl_%02d", seq(16)))
dtaFrm <- jmvReadWrite::read_omv(nmeOut)
unlink(nmeOut)
str(dtaFrm)
#> 'data.frame':    75 obs. of  17 variables:
#>  $ ID    : chr  "sbj_001" "sbj_002" "sbj_003" "sbj_004" ...
#>   ..- attr(*, "jmv-id")= logi TRUE
#>   ..- attr(*, "name")= chr "ID"
#>   ..- attr(*, "id")= int 1
#>   ..- attr(*, "columnType")= chr "Data"
#>   ..- attr(*, "dataType")= chr "Text"
#>   ..- attr(*, "measureType")= chr "ID"
#>   ..- attr(*, "formula")= chr ""
#>   ..- attr(*, "formulaMessage")= chr ""
#>   ..- attr(*, "parentId")= int 0
#>   ..- attr(*, "width")= int 100
#>   ..- attr(*, "type")= chr "string"
#>   ..- attr(*, "importName")= chr ""
#>   ..- attr(*, "description")= chr ""
#>   ..- attr(*, "transform")= int 0
#>   ..- attr(*, "edits")= list()
#>   ..- attr(*, "missingValues")= list()
#>  $ Trl_01: int  1 2 3 2 6 1 2 2 6 3 ...
#>   ..- attr(*, "name")= chr "Trl_01"
#>   ..- attr(*, "id")= int 2
#>   ..- attr(*, "columnType")= chr "Data"
#>   ..- attr(*, "dataType")= chr "Integer"
#>   ..- attr(*, "measureType")= chr "Continuous"
#>   ..- attr(*, "formula")= chr ""
#>   ..- attr(*, "formulaMessage")= chr ""
#>   ..- attr(*, "parentId")= int 0
#>   ..- attr(*, "width")= int 100
#>   ..- attr(*, "type")= chr "integer"
#>   ..- attr(*, "importName")= chr ""
#>   ..- attr(*, "description")= chr ""
#>   ..- attr(*, "transform")= int 0
#>   ..- attr(*, "edits")= list()
#>   ..- attr(*, "missingValues")= list()
#>  $ Trl_02: int  4 1 6 3 3 5 2 3 3 5 ...
#>   ..- attr(*, "name")= chr "Trl_02"
#>   ..- attr(*, "id")= int 3
#>   ..- attr(*, "columnType")= chr "Data"
#>   ..- attr(*, "dataType")= chr "Integer"
#>   ..- attr(*, "measureType")= chr "Continuous"
#>   ..- attr(*, "formula")= chr ""
#>   ..- attr(*, "formulaMessage")= chr ""
#>   ..- attr(*, "parentId")= int 0
#>   ..- attr(*, "width")= int 100
#>   ..- attr(*, "type")= chr "integer"
#>   ..- attr(*, "importName")= chr ""
#>   ..- attr(*, "description")= chr ""
#>   ..- attr(*, "transform")= int 0
#>   ..- attr(*, "edits")= list()
#>   ..- attr(*, "missingValues")= list()
#>  $ Trl_03: int  1 5 2 2 1 5 1 3 6 3 ...
#>   ..- attr(*, "name")= chr "Trl_03"
#>   ..- attr(*, "id")= int 4
#>   ..- attr(*, "columnType")= chr "Data"
#>   ..- attr(*, "dataType")= chr "Integer"
#>   ..- attr(*, "measureType")= chr "Continuous"
#>   ..- attr(*, "formula")= chr ""
#>   ..- attr(*, "formulaMessage")= chr ""
#>   ..- attr(*, "parentId")= int 0
#>   ..- attr(*, "width")= int 100
#>   ..- attr(*, "type")= chr "integer"
#>   ..- attr(*, "importName")= chr ""
#>   ..- attr(*, "description")= chr ""
#>   ..- attr(*, "transform")= int 0
#>   ..- attr(*, "edits")= list()
#>   ..- attr(*, "missingValues")= list()
#>  $ Trl_04: int  2 5 2 6 4 6 3 1 3 4 ...
#>   ..- attr(*, "name")= chr "Trl_04"
#>   ..- attr(*, "id")= int 5
#>   ..- attr(*, "columnType")= chr "Data"
#>   ..- attr(*, "dataType")= chr "Integer"
#>   ..- attr(*, "measureType")= chr "Continuous"
#>   ..- attr(*, "formula")= chr ""
#>   ..- attr(*, "formulaMessage")= chr ""
#>   ..- attr(*, "parentId")= int 0
#>   ..- attr(*, "width")= int 100
#>   ..- attr(*, "type")= chr "integer"
#>   ..- attr(*, "importName")= chr ""
#>   ..- attr(*, "description")= chr ""
#>   ..- attr(*, "transform")= int 0
#>   ..- attr(*, "edits")= list()
#>   ..- attr(*, "missingValues")= list()
#>  $ Trl_05: int  5 1 6 6 5 1 3 5 3 2 ...
#>   ..- attr(*, "name")= chr "Trl_05"
#>   ..- attr(*, "id")= int 6
#>   ..- attr(*, "columnType")= chr "Data"
#>   ..- attr(*, "dataType")= chr "Integer"
#>   ..- attr(*, "measureType")= chr "Continuous"
#>   ..- attr(*, "formula")= chr ""
#>   ..- attr(*, "formulaMessage")= chr ""
#>   ..- attr(*, "parentId")= int 0
#>   ..- attr(*, "width")= int 100
#>   ..- attr(*, "type")= chr "integer"
#>   ..- attr(*, "importName")= chr ""
#>   ..- attr(*, "description")= chr ""
#>   ..- attr(*, "transform")= int 0
#>   ..- attr(*, "edits")= list()
#>   ..- attr(*, "missingValues")= list()
#>  $ Trl_06: int  3 1 4 2 1 1 2 6 4 1 ...
#>   ..- attr(*, "name")= chr "Trl_06"
#>   ..- attr(*, "id")= int 7
#>   ..- attr(*, "columnType")= chr "Data"
#>   ..- attr(*, "dataType")= chr "Integer"
#>   ..- attr(*, "measureType")= chr "Continuous"
#>   ..- attr(*, "formula")= chr ""
#>   ..- attr(*, "formulaMessage")= chr ""
#>   ..- attr(*, "parentId")= int 0
#>   ..- attr(*, "width")= int 100
#>   ..- attr(*, "type")= chr "integer"
#>   ..- attr(*, "importName")= chr ""
#>   ..- attr(*, "description")= chr ""
#>   ..- attr(*, "transform")= int 0
#>   ..- attr(*, "edits")= list()
#>   ..- attr(*, "missingValues")= list()
#>  $ Trl_07: int  6 6 4 5 1 3 2 6 1 4 ...
#>   ..- attr(*, "name")= chr "Trl_07"
#>   ..- attr(*, "id")= int 8
#>   ..- attr(*, "columnType")= chr "Data"
#>   ..- attr(*, "dataType")= chr "Integer"
#>   ..- attr(*, "measureType")= chr "Continuous"
#>   ..- attr(*, "formula")= chr ""
#>   ..- attr(*, "formulaMessage")= chr ""
#>   ..- attr(*, "parentId")= int 0
#>   ..- attr(*, "width")= int 100
#>   ..- attr(*, "type")= chr "integer"
#>   ..- attr(*, "importName")= chr ""
#>   ..- attr(*, "description")= chr ""
#>   ..- attr(*, "transform")= int 0
#>   ..- attr(*, "edits")= list()
#>   ..- attr(*, "missingValues")= list()
#>  $ Trl_08: int  2 5 4 2 6 6 5 6 1 1 ...
#>   ..- attr(*, "name")= chr "Trl_08"
#>   ..- attr(*, "id")= int 9
#>   ..- attr(*, "columnType")= chr "Data"
#>   ..- attr(*, "dataType")= chr "Integer"
#>   ..- attr(*, "measureType")= chr "Continuous"
#>   ..- attr(*, "formula")= chr ""
#>   ..- attr(*, "formulaMessage")= chr ""
#>   ..- attr(*, "parentId")= int 0
#>   ..- attr(*, "width")= int 100
#>   ..- attr(*, "type")= chr "integer"
#>   ..- attr(*, "importName")= chr ""
#>   ..- attr(*, "description")= chr ""
#>   ..- attr(*, "transform")= int 0
#>   ..- attr(*, "edits")= list()
#>   ..- attr(*, "missingValues")= list()
#>  $ Trl_09: int  3 5 2 6 4 2 2 4 4 4 ...
#>   ..- attr(*, "name")= chr "Trl_09"
#>   ..- attr(*, "id")= int 10
#>   ..- attr(*, "columnType")= chr "Data"
#>   ..- attr(*, "dataType")= chr "Integer"
#>   ..- attr(*, "measureType")= chr "Continuous"
#>   ..- attr(*, "formula")= chr ""
#>   ..- attr(*, "formulaMessage")= chr ""
#>   ..- attr(*, "parentId")= int 0
#>   ..- attr(*, "width")= int 100
#>   ..- attr(*, "type")= chr "integer"
#>   ..- attr(*, "importName")= chr ""
#>   ..- attr(*, "description")= chr ""
#>   ..- attr(*, "transform")= int 0
#>   ..- attr(*, "edits")= list()
#>   ..- attr(*, "missingValues")= list()
#>  $ Trl_10: int  3 2 4 6 5 2 5 4 2 2 ...
#>   ..- attr(*, "name")= chr "Trl_10"
#>   ..- attr(*, "id")= int 11
#>   ..- attr(*, "columnType")= chr "Data"
#>   ..- attr(*, "dataType")= chr "Integer"
#>   ..- attr(*, "measureType")= chr "Continuous"
#>   ..- attr(*, "formula")= chr ""
#>   ..- attr(*, "formulaMessage")= chr ""
#>   ..- attr(*, "parentId")= int 0
#>   ..- attr(*, "width")= int 100
#>   ..- attr(*, "type")= chr "integer"
#>   ..- attr(*, "importName")= chr ""
#>   ..- attr(*, "description")= chr ""
#>   ..- attr(*, "transform")= int 0
#>   ..- attr(*, "edits")= list()
#>   ..- attr(*, "missingValues")= list()
#>  $ Trl_11: int  1 2 1 6 5 3 4 1 6 5 ...
#>   ..- attr(*, "name")= chr "Trl_11"
#>   ..- attr(*, "id")= int 12
#>   ..- attr(*, "columnType")= chr "Data"
#>   ..- attr(*, "dataType")= chr "Integer"
#>   ..- attr(*, "measureType")= chr "Continuous"
#>   ..- attr(*, "formula")= chr ""
#>   ..- attr(*, "formulaMessage")= chr ""
#>   ..- attr(*, "parentId")= int 0
#>   ..- attr(*, "width")= int 100
#>   ..- attr(*, "type")= chr "integer"
#>   ..- attr(*, "importName")= chr ""
#>   ..- attr(*, "description")= chr ""
#>   ..- attr(*, "transform")= int 0
#>   ..- attr(*, "edits")= list()
#>   ..- attr(*, "missingValues")= list()
#>  $ Trl_12: int  5 6 6 1 4 6 5 5 1 2 ...
#>   ..- attr(*, "name")= chr "Trl_12"
#>   ..- attr(*, "id")= int 13
#>   ..- attr(*, "columnType")= chr "Data"
#>   ..- attr(*, "dataType")= chr "Integer"
#>   ..- attr(*, "measureType")= chr "Continuous"
#>   ..- attr(*, "formula")= chr ""
#>   ..- attr(*, "formulaMessage")= chr ""
#>   ..- attr(*, "parentId")= int 0
#>   ..- attr(*, "width")= int 100
#>   ..- attr(*, "type")= chr "integer"
#>   ..- attr(*, "importName")= chr ""
#>   ..- attr(*, "description")= chr ""
#>   ..- attr(*, "transform")= int 0
#>   ..- attr(*, "edits")= list()
#>   ..- attr(*, "missingValues")= list()
#>  $ Trl_13: int  5 1 1 3 6 2 4 5 2 2 ...
#>   ..- attr(*, "name")= chr "Trl_13"
#>   ..- attr(*, "id")= int 14
#>   ..- attr(*, "columnType")= chr "Data"
#>   ..- attr(*, "dataType")= chr "Integer"
#>   ..- attr(*, "measureType")= chr "Continuous"
#>   ..- attr(*, "formula")= chr ""
#>   ..- attr(*, "formulaMessage")= chr ""
#>   ..- attr(*, "parentId")= int 0
#>   ..- attr(*, "width")= int 100
#>   ..- attr(*, "type")= chr "integer"
#>   ..- attr(*, "importName")= chr ""
#>   ..- attr(*, "description")= chr ""
#>   ..- attr(*, "transform")= int 0
#>   ..- attr(*, "edits")= list()
#>   ..- attr(*, "missingValues")= list()
#>  $ Trl_14: int  2 4 4 3 5 4 6 6 3 2 ...
#>   ..- attr(*, "name")= chr "Trl_14"
#>   ..- attr(*, "id")= int 15
#>   ..- attr(*, "columnType")= chr "Data"
#>   ..- attr(*, "dataType")= chr "Integer"
#>   ..- attr(*, "measureType")= chr "Continuous"
#>   ..- attr(*, "formula")= chr ""
#>   ..- attr(*, "formulaMessage")= chr ""
#>   ..- attr(*, "parentId")= int 0
#>   ..- attr(*, "width")= int 100
#>   ..- attr(*, "type")= chr "integer"
#>   ..- attr(*, "importName")= chr ""
#>   ..- attr(*, "description")= chr ""
#>   ..- attr(*, "transform")= int 0
#>   ..- attr(*, "edits")= list()
#>   ..- attr(*, "missingValues")= list()
#>  $ Trl_15: int  6 1 1 6 4 3 1 1 4 3 ...
#>   ..- attr(*, "name")= chr "Trl_15"
#>   ..- attr(*, "id")= int 16
#>   ..- attr(*, "columnType")= chr "Data"
#>   ..- attr(*, "dataType")= chr "Integer"
#>   ..- attr(*, "measureType")= chr "Continuous"
#>   ..- attr(*, "formula")= chr ""
#>   ..- attr(*, "formulaMessage")= chr ""
#>   ..- attr(*, "parentId")= int 0
#>   ..- attr(*, "width")= int 100
#>   ..- attr(*, "type")= chr "integer"
#>   ..- attr(*, "importName")= chr ""
#>   ..- attr(*, "description")= chr ""
#>   ..- attr(*, "transform")= int 0
#>   ..- attr(*, "edits")= list()
#>   ..- attr(*, "missingValues")= list()
#>  $ Trl_16: int  6 4 6 4 4 5 3 3 1 1 ...
#>   ..- attr(*, "name")= chr "Trl_16"
#>   ..- attr(*, "id")= int 17
#>   ..- attr(*, "columnType")= chr "Data"
#>   ..- attr(*, "dataType")= chr "Integer"
#>   ..- attr(*, "measureType")= chr "Continuous"
#>   ..- attr(*, "formula")= chr ""
#>   ..- attr(*, "formulaMessage")= chr ""
#>   ..- attr(*, "parentId")= int 0
#>   ..- attr(*, "width")= int 100
#>   ..- attr(*, "type")= chr "integer"
#>   ..- attr(*, "importName")= chr ""
#>   ..- attr(*, "description")= chr ""
#>   ..- attr(*, "transform")= int 0
#>   ..- attr(*, "edits")= list()
#>   ..- attr(*, "missingValues")= list()
#>  - attr(*, "removedRows")= list()
#>  - attr(*, "addedRows")= list()
#>  - attr(*, "transforms")= list()
# alternatively, the character vector with the desired variable names (of the same length as
# the number of rows in tmpDF) may be given, "Trl" can easily be exchanged by the name of your
# questionnaire, experimental conditions, etc.

```
