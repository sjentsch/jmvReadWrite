# Label columns / variables in .omv-files for the statistical spreadsheet 'jamovi' (<https://www.jamovi.org>)

Label columns / variables in .omv-files for the statistical spreadsheet
'jamovi' (<https://www.jamovi.org>)

## Usage

``` r
label_vars_omv(
  dtaInp = NULL,
  fleOut = "",
  varLbl = NULL,
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

- varLbl:

  Variable (default: NULL) containing either a character (a file name;
  the file must contain two columns one with variable names, the other
  with the labels), a data frame (one column the variable names, the
  other the labels), or a character vector (with the same length as the
  data set, containing the variable labels). See Details for more
  information.

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

- `varLbl` can be either (1) a character with a file name to read (the
  file must contain to columns, one with the variable names, the other
  with the variable labels); (2) a data frame with two columns (one with
  the variable names, the other with the variable labels), or (3) a
  character vector containing the variable labels (with a length equal
  to the number of variables in the input data set).

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

`label_vars_omv` internally uses the following functions for reading and
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
# use one of the data files included in the package, but only the first 28 columns
# (the latter columns contain data for testing calculations, etc.)
nmeInp <- system.file("extdata", "bfi_sample.omv", package = "jmvReadWrite")
dtaInp <- jmvReadWrite::read_omv(nmeInp)[1:28]
nmeOut <- tempfile(fileext = ".omv")
# in the original file, the variable labels – attr(*, "jmv-desc") - are empty
lapply(dtaInp, attr, "jmv-desc")
#> $ID
#> NULL
#> 
#> $A1
#> NULL
#> 
#> $A2
#> NULL
#> 
#> $A3
#> NULL
#> 
#> $A4
#> NULL
#> 
#> $A5
#> NULL
#> 
#> $C1
#> NULL
#> 
#> $C2
#> NULL
#> 
#> $C3
#> NULL
#> 
#> $C4
#> NULL
#> 
#> $C5
#> NULL
#> 
#> $E1
#> NULL
#> 
#> $E2
#> NULL
#> 
#> $E3
#> NULL
#> 
#> $E4
#> NULL
#> 
#> $E5
#> NULL
#> 
#> $N1
#> NULL
#> 
#> $N2
#> NULL
#> 
#> $N3
#> NULL
#> 
#> $N4
#> NULL
#> 
#> $N5
#> NULL
#> 
#> $O1
#> NULL
#> 
#> $O2
#> NULL
#> 
#> $O3
#> NULL
#> 
#> $O4
#> NULL
#> 
#> $O5
#> NULL
#> 
#> $gender
#> NULL
#> 
#> $age
#> NULL
#> 
# the definition of the variable labels can be read from a file with two columns,
# the first containing the variable name, the second the variable labels
# you can easily create such a file in Excel and save it as CSV
# if your CSV contains column names (e.g., varNme and varLbl) in the first row are they ignored
lblFle <- system.file("extdata", "label_example.csv", package = "jmvReadWrite")
lblDtF <- utils::read.csv(lblFle, header = FALSE)
str(lblDtF)
#> 'data.frame':    28 obs. of  2 variables:
#>  $ V1: chr  "ID" "A1" "A2" "A3" ...
#>  $ V2: chr  "Respondent ID" "Am indifferent to the feelings of others. (R)" "Inquire about others' well-being." "Know how to comfort others." ...

# there are three options to give the varLbl parameter:
# (1) as file name, ...
jmvReadWrite::label_vars_omv(dtaInp = dtaInp, fleOut = nmeOut, varLbl = lblFle)
lapply(jmvReadWrite::read_omv(nmeOut), attr, "jmv-desc")
#> $ID
#> [1] "Respondent ID"
#> 
#> $A1
#> [1] "Am indifferent to the feelings of others. (R)"
#> 
#> $A2
#> [1] "Inquire about others' well-being."
#> 
#> $A3
#> [1] "Know how to comfort others."
#> 
#> $A4
#> [1] "Love children."
#> 
#> $A5
#> [1] "Make people feel at ease."
#> 
#> $C1
#> [1] "Am exacting in my work."
#> 
#> $C2
#> [1] "Continue until everything is perfect."
#> 
#> $C3
#> [1] "Do things according to a plan."
#> 
#> $C4
#> [1] "Do things in a half-way manner. (R)"
#> 
#> $C5
#> [1] "Waste my time. (R)"
#> 
#> $E1
#> [1] "Don't talk a lot. (R)"
#> 
#> $E2
#> [1] "Find it difficult to approach others. (R)"
#> 
#> $E3
#> [1] "Know how to captivate people."
#> 
#> $E4
#> [1] "Make friends easily."
#> 
#> $E5
#> [1] "Take charge."
#> 
#> $N1
#> [1] "Get angry easily."
#> 
#> $N2
#> [1] "Get irritated easily."
#> 
#> $N3
#> [1] "Have frequent mood swings."
#> 
#> $N4
#> [1] "Often feel blue."
#> 
#> $N5
#> [1] "Panic easily."
#> 
#> $O1
#> [1] "Am full of ideas."
#> 
#> $O2
#> [1] "Avoid difficult reading material. (R)"
#> 
#> $O3
#> [1] "Carry the conversation to a higher level."
#> 
#> $O4
#> [1] "Spend time reflecting on things."
#> 
#> $O5
#> [1] "Will not probe deeply into a subject. (R)"
#> 
#> $gender
#> [1] "Gender of the respondent"
#> 
#> $age
#> [1] "Age of the respondent (years)"
#> 
unlink(nmeOut)

# (2) as data frame (using lblDtF from above), or ...
jmvReadWrite::label_vars_omv(dtaInp = dtaInp, fleOut = nmeOut, varLbl = lblDtF)
lapply(jmvReadWrite::read_omv(nmeOut), attr, "jmv-desc")
#> $ID
#> [1] "Respondent ID"
#> 
#> $A1
#> [1] "Am indifferent to the feelings of others. (R)"
#> 
#> $A2
#> [1] "Inquire about others' well-being."
#> 
#> $A3
#> [1] "Know how to comfort others."
#> 
#> $A4
#> [1] "Love children."
#> 
#> $A5
#> [1] "Make people feel at ease."
#> 
#> $C1
#> [1] "Am exacting in my work."
#> 
#> $C2
#> [1] "Continue until everything is perfect."
#> 
#> $C3
#> [1] "Do things according to a plan."
#> 
#> $C4
#> [1] "Do things in a half-way manner. (R)"
#> 
#> $C5
#> [1] "Waste my time. (R)"
#> 
#> $E1
#> [1] "Don't talk a lot. (R)"
#> 
#> $E2
#> [1] "Find it difficult to approach others. (R)"
#> 
#> $E3
#> [1] "Know how to captivate people."
#> 
#> $E4
#> [1] "Make friends easily."
#> 
#> $E5
#> [1] "Take charge."
#> 
#> $N1
#> [1] "Get angry easily."
#> 
#> $N2
#> [1] "Get irritated easily."
#> 
#> $N3
#> [1] "Have frequent mood swings."
#> 
#> $N4
#> [1] "Often feel blue."
#> 
#> $N5
#> [1] "Panic easily."
#> 
#> $O1
#> [1] "Am full of ideas."
#> 
#> $O2
#> [1] "Avoid difficult reading material. (R)"
#> 
#> $O3
#> [1] "Carry the conversation to a higher level."
#> 
#> $O4
#> [1] "Spend time reflecting on things."
#> 
#> $O5
#> [1] "Will not probe deeply into a subject. (R)"
#> 
#> $gender
#> [1] "Gender of the respondent"
#> 
#> $age
#> [1] "Age of the respondent (years)"
#> 
unlink(nmeOut)

# (3) as character vector (with the same length as there are columns in the input data set)
lblChr <- lblDtF[[2]]
head(lblChr)
#> [1] "Respondent ID"                                
#> [2] "Am indifferent to the feelings of others. (R)"
#> [3] "Inquire about others' well-being."            
#> [4] "Know how to comfort others."                  
#> [5] "Love children."                               
#> [6] "Make people feel at ease."                    
jmvReadWrite::label_vars_omv(dtaInp = dtaInp, fleOut = nmeOut, varLbl = lblChr)
lapply(jmvReadWrite::read_omv(nmeOut), attr, "jmv-desc")
#> $ID
#> [1] "Respondent ID"
#> 
#> $A1
#> [1] "Am indifferent to the feelings of others. (R)"
#> 
#> $A2
#> [1] "Inquire about others' well-being."
#> 
#> $A3
#> [1] "Know how to comfort others."
#> 
#> $A4
#> [1] "Love children."
#> 
#> $A5
#> [1] "Make people feel at ease."
#> 
#> $C1
#> [1] "Am exacting in my work."
#> 
#> $C2
#> [1] "Continue until everything is perfect."
#> 
#> $C3
#> [1] "Do things according to a plan."
#> 
#> $C4
#> [1] "Do things in a half-way manner. (R)"
#> 
#> $C5
#> [1] "Waste my time. (R)"
#> 
#> $E1
#> [1] "Don't talk a lot. (R)"
#> 
#> $E2
#> [1] "Find it difficult to approach others. (R)"
#> 
#> $E3
#> [1] "Know how to captivate people."
#> 
#> $E4
#> [1] "Make friends easily."
#> 
#> $E5
#> [1] "Take charge."
#> 
#> $N1
#> [1] "Get angry easily."
#> 
#> $N2
#> [1] "Get irritated easily."
#> 
#> $N3
#> [1] "Have frequent mood swings."
#> 
#> $N4
#> [1] "Often feel blue."
#> 
#> $N5
#> [1] "Panic easily."
#> 
#> $O1
#> [1] "Am full of ideas."
#> 
#> $O2
#> [1] "Avoid difficult reading material. (R)"
#> 
#> $O3
#> [1] "Carry the conversation to a higher level."
#> 
#> $O4
#> [1] "Spend time reflecting on things."
#> 
#> $O5
#> [1] "Will not probe deeply into a subject. (R)"
#> 
#> $gender
#> [1] "Gender of the respondent"
#> 
#> $age
#> [1] "Age of the respondent (years)"
#> 
unlink(nmeOut)

```
