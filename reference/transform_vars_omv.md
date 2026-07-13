# Transform skewed variables (aiming at they conform to a normal distribution) in .omv-files for the statistical spreadsheet 'jamovi' (<https://www.jamovi.org>)

Transform skewed variables (aiming at they conform to a normal
distribution) in .omv-files for the statistical spreadsheet 'jamovi'
(<https://www.jamovi.org>)

## Usage

``` r
transform_vars_omv(
  dtaInp = NULL,
  fleOut = "",
  varXfm = NULL,
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

- varXfm:

  Named list variable where the name indicates which transformation is
  to be carried out and where each list entry points to one or more
  variables to be transformed using this transformation. See Details for
  more information.

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

- `varXfm` has to be a named list variable where the names can either
  indicate the type of transformation or the kind and degree of skewness
  that shall be corrected. For the type of transformation, the following
  names are valid: `posSqr`, `negSqr`, `posLog`, `negLog`, `posInv`,
  `negInv`; where the second part of the name indicates the
  transformation to be carried out: `...Sqr` - square root, `...Log` -
  logarithm to the basis 10, `...Inv` - inversion, i.e., 1 / original
  value), and where the first part of the name indicates whether the
  original value is used (`pos...`) or whether the original value is
  subtracted from the maximum value of that variable (`neg...`; a
  constant of 1 is added to the maximum value for `...Log` and `...Inv`
  transformations). For the degree and kind of skewness, the following
  names are valid: `mdrPos`, `strPos`, `svrPos`, `mdrNeg`, `strNeg`,
  `svrNeg` (degree: moderate, strong, severe; kind: positive or
  negative).

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

`transform_vars_omv` internally uses the following functions for reading
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
# generate skewed variables
set.seed(335)
dtaInp <- data.frame(MP = rnorm(1000) * 1e-1 + rexp(1000, 2) * (1 - 1e-1),
                     MN = rnorm(1000) * 1e-1 - rexp(1000, 2) * (1 - 1e-1),
                     SP = rnorm(1000) * 1e-2 + rexp(1000, 2) * (1 - 1e-2),
                     SN = rnorm(1000) * 1e-2 - rexp(1000, 2) * (1 - 1e-2),
                     EP = rnorm(1000) * 1e-4 + rexp(1000, 2) * (1 - 1e-4),
                     EN = rnorm(1000) * 1e-4 - rexp(1000, 2) * (1 - 1e-4))
jmv::descriptives(data = dtaInp, skew = TRUE, sw = TRUE)
#> 
#>  DESCRIPTIVES
#> 
#>  Descriptives                                                                                                    
#>  ─────────────────────────────────────────────────────────────────────────────────────────────────────────────── 
#>                           MP            MN            SP             SN            EP             EN             
#>  ─────────────────────────────────────────────────────────────────────────────────────────────────────────────── 
#>    N                            1000          1000           1000          1000           1000            1000   
#>    Missing                         0             0              0             0              0               0   
#>    Mean                    0.4480184    -0.4646020      0.4781537    -0.4923456      0.5047809      -0.4990873   
#>    Median                  0.3380596    -0.3269513      0.3197633    -0.3401608      0.3404835      -0.3466678   
#>    Standard deviation      0.4304019     0.4796432      0.4789503     0.4825046      0.5134605       0.5080463   
#>    Minimum                -0.2340510     -2.634444    -0.01486472     -3.474560    0.001293439       -4.066375   
#>    Maximum                  3.068939     0.1981469       3.250690    0.01231214       3.872901    -3.689731e-5   
#>    Skewness                 1.526837     -1.538163       1.730588     -1.869945       1.954079       -2.093286   
#>    Std. error skewness    0.07734382    0.07734382     0.07734382    0.07734382     0.07734382      0.07734382   
#>    Shapiro-Wilk W          0.8884226     0.8645966      0.8252225     0.8271758      0.8121985       0.8008598   
#>    Shapiro-Wilk p         < .0000001    < .0000001     < .0000001    < .0000001     < .0000001      < .0000001   
#>  ─────────────────────────────────────────────────────────────────────────────────────────────────────────────── 
#> 

crrXfm <- list(posSqr = c("MP"), negSqr = c("MN"), posLog = c("MP", "SP"), negLog = c("SN"),
               posInv = c("MP", "SP", "EP"), negInv = c("EN"))
dtaOut <- jmvReadWrite::transform_vars_omv(dtaInp = dtaInp, varXfm = crrXfm)
jmv::descriptives(data = dtaOut, skew = TRUE, sw = TRUE)
#> 
#>  DESCRIPTIVES
#> 
#>  Descriptives                                                                                                                                                                                                                                  
#>  ───────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────── 
#>                           MP            MN            SP             SN            EP             EN              MP_SQR        MN_SQR        MP_LOG        SP_LOG        SN_LOG        MP_INV        SP_INV        EP_INV        EN_INV       
#>  ───────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────── 
#>    N                            1000          1000           1000          1000           1000            1000          1000          1000          1000          1000          1000          1000          1000          1000          1000   
#>    Missing                         0             0              0             0              0               0             0             0             0             0             0             0             0             0             0   
#>    Mean                    0.4480184    -0.4646020      0.4781537    -0.4923456      0.5047809      -0.4990873     0.7886966     0.7667946     0.2135253     0.1558936     0.1594012     0.6272808     0.7231910     0.7235345     0.7235556   
#>    Median                  0.3380596    -0.3269513      0.3197633    -0.3401608      0.3404835      -0.3466678     0.7563799     0.7246364     0.1964831     0.1253601     0.1311286     0.6360876     0.7492730     0.7467201     0.7425942   
#>    Standard deviation      0.4304019     0.4796432      0.4789503     0.4825046      0.5134605       0.5080463     0.2451266     0.2735867     0.1005615     0.1201892     0.1197994     0.1348379     0.1761416     0.1832728     0.1792720   
#>    Minimum                -0.2340510     -2.634444    -0.01486472     -3.474560    0.001293439       -4.066375      0.000000      0.000000      0.000000      0.000000      0.000000     0.2323965     0.2344361     0.2052711     0.1973812   
#>    Maximum                  3.068939     0.1981469       3.250690    0.01231214       3.872901    -3.689731e-5      1.817413      1.683030     0.6337704     0.6299755     0.6519437      1.000000      1.000000      1.000000      1.000000   
#>    Skewness                 1.526837     -1.538163       1.730588     -1.869945       1.954079       -2.093286     0.5789399     0.6582018     0.7445348     0.9976868     0.9711986    -0.1693792    -0.4780434    -0.4496755    -0.4746157   
#>    Std. error skewness    0.07734382    0.07734382     0.07734382    0.07734382     0.07734382      0.07734382    0.07734382    0.07734382    0.07734382    0.07734382    0.07734382    0.07734382    0.07734382    0.07734382    0.07734382   
#>    Shapiro-Wilk W          0.8884226     0.8645966      0.8252225     0.8271758      0.8121985       0.8008598     0.9795875     0.9697485     0.9640099     0.9125518     0.9233129     0.9934879     0.9567561     0.9590432     0.9606668   
#>    Shapiro-Wilk p         < .0000001    < .0000001     < .0000001    < .0000001     < .0000001      < .0000001    < .0000001    < .0000001    < .0000001    < .0000001    < .0000001     0.0002339    < .0000001    < .0000001    < .0000001   
#>  ───────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────── 
#> 

crrXfm <- list(mdrPos = c("MP"), mdrNeg = c("MN"), strPos = c("SP"), strNeg = c("SN"),
               svrPos = c("EP"), svrNeg = c("EN"))
dtaOut <- jmvReadWrite::transform_vars_omv(dtaInp = dtaInp, varXfm = crrXfm)
jmv::descriptives(data = dtaOut, skew = TRUE, sw = TRUE)
#> 
#>  DESCRIPTIVES
#> 
#>  Descriptives                                                                                                                                                                                        
#>  ─────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────── 
#>                           MP            MN            SP             SN            EP             EN              MP_SQR        MN_SQR        SP_LOG        SN_LOG        EP_INV        EN_INV       
#>  ─────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────── 
#>    N                            1000          1000           1000          1000           1000            1000          1000          1000          1000          1000          1000          1000   
#>    Missing                         0             0              0             0              0               0             0             0             0             0             0             0   
#>    Mean                    0.4480184    -0.4646020      0.4781537    -0.4923456      0.5047809      -0.4990873     0.7886966     0.7667946     0.1558936     0.1594012     0.7235345     0.7235556   
#>    Median                  0.3380596    -0.3269513      0.3197633    -0.3401608      0.3404835      -0.3466678     0.7563799     0.7246364     0.1253601     0.1311286     0.7467201     0.7425942   
#>    Standard deviation      0.4304019     0.4796432      0.4789503     0.4825046      0.5134605       0.5080463     0.2451266     0.2735867     0.1201892     0.1197994     0.1832728     0.1792720   
#>    Minimum                -0.2340510     -2.634444    -0.01486472     -3.474560    0.001293439       -4.066375      0.000000      0.000000      0.000000      0.000000     0.2052711     0.1973812   
#>    Maximum                  3.068939     0.1981469       3.250690    0.01231214       3.872901    -3.689731e-5      1.817413      1.683030     0.6299755     0.6519437      1.000000      1.000000   
#>    Skewness                 1.526837     -1.538163       1.730588     -1.869945       1.954079       -2.093286     0.5789399     0.6582018     0.9976868     0.9711986    -0.4496755    -0.4746157   
#>    Std. error skewness    0.07734382    0.07734382     0.07734382    0.07734382     0.07734382      0.07734382    0.07734382    0.07734382    0.07734382    0.07734382    0.07734382    0.07734382   
#>    Shapiro-Wilk W          0.8884226     0.8645966      0.8252225     0.8271758      0.8121985       0.8008598     0.9795875     0.9697485     0.9125518     0.9233129     0.9590432     0.9606668   
#>    Shapiro-Wilk p         < .0000001    < .0000001     < .0000001    < .0000001     < .0000001      < .0000001    < .0000001    < .0000001    < .0000001    < .0000001    < .0000001    < .0000001   
#>  ─────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────── 
#> 

```
