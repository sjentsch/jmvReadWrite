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
  any supported file type, see Details below.

- fleOut:

  Name of the data set / file to be written (including the path, if
  required; "FILE_OUT.omv"; default: ""); if empty, the resulting data
  frame is returned instead.

- varXfm:

  Named list variable where the name indicates which transformation is
  to be carried out and where each list entry points to one or more
  variables to be transformed using this transformation. See Details for
  more information.

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
  negative). Since variables either are positively or negatively skewed
  they can only appear once within those entries requesting
  transformations for positively skewed variables (i.e., `posSqr`,
  `posLog`, or `posInv`) OR those entries requesting transformations for
  negatively skewed variables (i.e., `negSqr`, `negLog`, or `negInv`).
  If a variable appears, e.g., under `posSqr` AND `negSqr` an error
  message is shown.

- The ellipsis-parameter (`...`) can be used to submit arguments /
  parameters to the functions that are used for reading the data. By
  clicking on the respective function under “See also”, you can get a
  more detailed overview over which parameters each of those functions
  take.

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
#>  ───────────────────────────────────────────────────────────────────────────────────────────── 
#>                           MP         MN         SP          SN         EP          EN          
#>  ───────────────────────────────────────────────────────────────────────────────────────────── 
#>    N                         1000       1000        1000       1000        1000         1000   
#>    Missing                      0          0           0          0           0            0   
#>    Mean                    0.4480    -0.4646      0.4782    -0.4923      0.5048      -0.4991   
#>    Median                  0.3381    -0.3270      0.3198    -0.3402      0.3405      -0.3467   
#>    Standard deviation      0.4304     0.4796      0.4790     0.4825      0.5135       0.5080   
#>    Minimum                -0.2341     -2.634    -0.01486     -3.475    0.001293       -4.066   
#>    Maximum                  3.069     0.1981       3.251    0.01231       3.873    -3.690e-5   
#>    Skewness                 1.527     -1.538       1.731     -1.870       1.954       -2.093   
#>    Std. error skewness    0.07734    0.07734     0.07734    0.07734     0.07734      0.07734   
#>    Shapiro-Wilk W          0.8884     0.8646      0.8252     0.8272      0.8122       0.8009   
#>    Shapiro-Wilk p          < .001     < .001      < .001     < .001      < .001       < .001   
#>  ───────────────────────────────────────────────────────────────────────────────────────────── 
#> 

crrXfm <- list(posSqr = c("MP"), negSqr = c("MN"), posLog = c("MP", "SP"), negLog = c("SN"),
               posInv = c("MP", "SP", "EP"), negInv = c("EN"))
dtaOut <- jmvReadWrite::transform_vars_omv(dtaInp = dtaInp, varXfm = crrXfm)
jmv::descriptives(data = dtaOut, skew = TRUE, sw = TRUE)
#> 
#>  DESCRIPTIVES
#> 
#>  Descriptives                                                                                                                                                                                     
#>  ──────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────── 
#>                           MP         MN         SP          SN         EP          EN           MP_SQR     MN_SQR     MP_LOG     SP_LOG     SN_LOG     MP_INV     SP_INV     EP_INV     EN_INV    
#>  ──────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────── 
#>    N                         1000       1000        1000       1000        1000         1000       1000       1000       1000       1000       1000       1000       1000       1000       1000   
#>    Missing                      0          0           0          0           0            0          0          0          0          0          0          0          0          0          0   
#>    Mean                    0.4480    -0.4646      0.4782    -0.4923      0.5048      -0.4991     0.7887     0.7668     0.2135     0.1559     0.1594     0.6273     0.7232     0.7235     0.7236   
#>    Median                  0.3381    -0.3270      0.3198    -0.3402      0.3405      -0.3467     0.7564     0.7246     0.1965     0.1254     0.1311     0.6361     0.7493     0.7467     0.7426   
#>    Standard deviation      0.4304     0.4796      0.4790     0.4825      0.5135       0.5080     0.2451     0.2736     0.1006     0.1202     0.1198     0.1348     0.1761     0.1833     0.1793   
#>    Minimum                -0.2341     -2.634    -0.01486     -3.475    0.001293       -4.066      0.000      0.000      0.000      0.000      0.000     0.2324     0.2344     0.2053     0.1974   
#>    Maximum                  3.069     0.1981       3.251    0.01231       3.873    -3.690e-5      1.817      1.683     0.6338     0.6300     0.6519      1.000      1.000      1.000      1.000   
#>    Skewness                 1.527     -1.538       1.731     -1.870       1.954       -2.093     0.5789     0.6582     0.7445     0.9977     0.9712    -0.1694    -0.4780    -0.4497    -0.4746   
#>    Std. error skewness    0.07734    0.07734     0.07734    0.07734     0.07734      0.07734    0.07734    0.07734    0.07734    0.07734    0.07734    0.07734    0.07734    0.07734    0.07734   
#>    Shapiro-Wilk W          0.8884     0.8646      0.8252     0.8272      0.8122       0.8009     0.9796     0.9697     0.9640     0.9126     0.9233     0.9935     0.9568     0.9590     0.9607   
#>    Shapiro-Wilk p          < .001     < .001      < .001     < .001      < .001       < .001     < .001     < .001     < .001     < .001     < .001     < .001     < .001     < .001     < .001   
#>  ──────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────── 
#> 

crrXfm <- list(mdrPos = c("MP"), mdrNeg = c("MN"), strPos = c("SP"), strNeg = c("SN"),
               svrPos = c("EP"), svrNeg = c("EN"))
dtaOut <- jmvReadWrite::transform_vars_omv(dtaInp = dtaInp, varXfm = crrXfm)
jmv::descriptives(data = dtaOut, skew = TRUE, sw = TRUE)
#> 
#>  DESCRIPTIVES
#> 
#>  Descriptives                                                                                                                                                    
#>  ─────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────── 
#>                           MP         MN         SP          SN         EP          EN           MP_SQR     MN_SQR     SP_LOG     SN_LOG     EP_INV     EN_INV    
#>  ─────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────── 
#>    N                         1000       1000        1000       1000        1000         1000       1000       1000       1000       1000       1000       1000   
#>    Missing                      0          0           0          0           0            0          0          0          0          0          0          0   
#>    Mean                    0.4480    -0.4646      0.4782    -0.4923      0.5048      -0.4991     0.7887     0.7668     0.1559     0.1594     0.7235     0.7236   
#>    Median                  0.3381    -0.3270      0.3198    -0.3402      0.3405      -0.3467     0.7564     0.7246     0.1254     0.1311     0.7467     0.7426   
#>    Standard deviation      0.4304     0.4796      0.4790     0.4825      0.5135       0.5080     0.2451     0.2736     0.1202     0.1198     0.1833     0.1793   
#>    Minimum                -0.2341     -2.634    -0.01486     -3.475    0.001293       -4.066      0.000      0.000      0.000      0.000     0.2053     0.1974   
#>    Maximum                  3.069     0.1981       3.251    0.01231       3.873    -3.690e-5      1.817      1.683     0.6300     0.6519      1.000      1.000   
#>    Skewness                 1.527     -1.538       1.731     -1.870       1.954       -2.093     0.5789     0.6582     0.9977     0.9712    -0.4497    -0.4746   
#>    Std. error skewness    0.07734    0.07734     0.07734    0.07734     0.07734      0.07734    0.07734    0.07734    0.07734    0.07734    0.07734    0.07734   
#>    Shapiro-Wilk W          0.8884     0.8646      0.8252     0.8272      0.8122       0.8009     0.9796     0.9697     0.9126     0.9233     0.9590     0.9607   
#>    Shapiro-Wilk p          < .001     < .001      < .001     < .001      < .001       < .001     < .001     < .001     < .001     < .001     < .001     < .001   
#>  ─────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────── 
#> 

```
