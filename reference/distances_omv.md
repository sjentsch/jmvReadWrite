# Calculates distances (returning a symmetric matrix) from a raw data matrix in .omv-files for the statistical spreadsheet 'jamovi' (<https://www.jamovi.org>)

Calculates distances (returning a symmetric matrix) from a raw data
matrix in .omv-files for the statistical spreadsheet 'jamovi'
(<https://www.jamovi.org>)

## Usage

``` r
distances_omv(
  dtaInp = NULL,
  fleOut = "",
  varDst = c(),
  clmDst = TRUE,
  stdDst = "none",
  nmeDst = "euclid",
  mtxSps = FALSE,
  mtxTrL = FALSE,
  mtxDgn = TRUE,
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

  Name of the data file to be written (including the path, if required;
  "FILE_OUT.omv"; default: ""); if empty, the resulting data frame is
  returned instead.

- varDst:

  Variable (default: c()) containing a character vector with the names
  of the variables for which distances are to be calculated. See Details
  for more information.

- clmDst:

  Whether the distances shall be calculated between columns (TRUE) or
  rows (FALSE; default: TRUE). See Details for more information.

- stdDst:

  Character string indicating whether the variables in varDst are to be
  standardized and how (default: "none"). See Details for more
  information.

- nmeDst:

  Character string indicating which distance measure is to be calculated
  calculated (default: "euclidean"). See Details for more information.

- mtxSps:

  Whether the symmetric matrix to be returned should be sparse (default:
  FALSE)

- mtxTrL:

  Whether the symmetric matrix to be returned should only contain the
  lower triangular (default: FALSE)

- mtxDgn:

  Whether the symmetric matrix to be returned should retain the values
  in the main diagonal (default: TRUE)

- usePkg:

  Name of the package: "foreign" or "haven" that shall be used to read
  SPSS, Stata, and SAS files; "foreign" is the default (it comes with
  base R), but "haven" is newer and more comprehensive.

- selSet:

  Name of the data set that is to be selected from the workspace (only
  applies when reading .RData-files)

- ...:

  Additional arguments passed on to methods; see Details below.

## Value

a data frame containing a symmetric matrix (only returned if `fleOut` is
empty) containing the distances between the variables / columns (clmDst
== TRUE) or rows (clmDst == FALSE)

## Details

- `varDst` must a character vector containing the variables to
  calculated distances over. If `clmDst` is set to TRUE, distances are
  calculated between all possible variable pairs and over subjects /
  rows in the original data frame. If `clmDst` is set to FALSE,
  distances are calculated between participants and over all variables
  given in `varDst`. If `clmDst` is set to `TRUE`, the symmetric matrix
  that is returned has the size V x V (V being the number of variables
  in varDst; if `mtxSps` is set to `TRUE`, the size is V - 1 x V - 1,
  see below); if `clmDst` is set to `FALSE`, the symmetric matrix that
  is returned has the size R x R (R being the number of rows in the
  original dataset; it is if `mtxSps` is set to `TRUE`, the size is R -
  1 x R - 1, see below).

- `stdDst` can be one of the following calculations to standardize the
  selected variables before calculating the distances: `none` (do not
  standardize; default), `z` (z scores), `sd` (divide by the std. dev.),
  `range` (divide by the range), `max` (divide by the absolute maximum),
  `mean` (divide by the mean), `rescale` (subtract the mean and divide
  by the range).

- `nmeDst` can be one of the following distance measures. (1) For
  interval data: `euclid` (Euclidean), `seuclid` (squared Euclidean),
  `block` (city block / Manhattan), `canberra` (Canberra). `chebychev`
  (maximum distance / supremum norm / Chebychev), `minkowski_p`
  (Minkowski with power p; NB: needs p), `power_p_r` (Minkowski with
  power p, and the r-th root; NB: needs p and r), `cosine` (cosine
  between the two vectors), `correlation` (correlation between the two
  vectors). (2) For frequency count data: `chisq` (chi-square
  dissimilarity between two sets of frequencies), `ph2` (chi-square
  dissimilarity normalized by the square root of the number of values
  used in the calculation). (3) For binary data, all measure have to
  optional parts `p` and `np` which indicate presence (`p`; defaults to
  1 if not given) or absence (`np`; defaults to zero if not given). (a)
  matching coefficients: `rr_p_np` (Russell and Rao), `sm_p_np` (simple
  matching), `jaccard_p_np` / `jaccards_p_np` (Jaccard similarity; as in
  SPSS), `jaccardd_p_np` (Jaccard dissimiliarity; as in
  `dist(..., "binary")` in R), `dice_p_np` (Dice or Czekanowski or
  Sorenson similarity), `ss1_p_np` (Sokal and Sneath measure 1),
  `rt_p_np` (Rogers and Tanimoto), `ss2_p_np` (Sokal and Sneath measure
  2), `k1_p_np` (Kulczynski measure 1), `ss3_p_np` (Sokal and Sneath
  measure 3). (b) conditional probabilities: `k2_p_np` (Kulczynski
  measure 2), `ss4_p_np` (Sokal and Sneath measure 4), `hamann_p_np`
  (Hamann). (c) predictability measures: `lambda_p_np` (Goodman and
  Kruskal Lambda), `d_p_np` (Anderberg’s D), `y_p_np` (Yule’s Y
  coefficient of colligation), `q_p_np` (Yule’s Q). (d) other measures:
  `ochiai_p_np` (Ochiai), `ss5_p_np` (Sokal and Sneath measure 5),
  `phi_p_np` (fourfold point correlation), `beuclid_p_np` (binary
  Euclidean distance), `bseuclid_p_np` (binary squared Euclidean
  distance), `size_p_np` (size difference), `pattern_p_np` (pattern
  difference), `bshape_p_np` (binary Shape difference), `disper_p_np`
  (dispersion similarity), `variance_p_np` (variance dissimilarity),
  `blwmn_p_np` (binary Lance and Williams non-metric dissimilarity). (4)
  `none` (only carry out standardization, if stdDst is different from
  `none`).

- If `mtxSps` is set, a sparse matrix is returned. Those matrices are
  similar to the format one often finds for correlation matrices. The
  values are only retained in the lower triangular, the columns range
  from the first to the variable that is second to the last in `varDst`
  (or respectively, the columns contain the first to the second to the
  last row of the original dataset when `clmDst` is set to `FALSE`), and
  the rows contain the second to the last variable in `varDst` (or
  respectively, the rows contain the second to the last row of the
  original dataset when `clmDst` is set to `FALSE`).

- By default, a full symmetric matrix is returned (i.e., a matrix that
  has no NAs in any cell). This behaviour can be changed with setting
  `mtxTrL` and `mtxDgn`: If `mtxTrL` is set to `TRUE`, the values from
  the upper triangular matrix are removed / replaced with NAs; if
  `mtxDgn` is set to `FALSE`, the values from the main diagonal are
  removed / replaced with NAs.

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

`distances_omv` internally uses the following function for calculating
the distances for interval data
[`stats::dist()`](https://rdrr.io/r/stats/dist.html). It furthermore
uses the following functions for reading and writing data files in
different formats:
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
# create matrices for the different types of distance measures: continuous
# (cntFrm), frequency counts (frqFrm) or binary (binFrm); all 20 R x 5 C
set.seed(1)
cntFrm <- stats::setNames(as.data.frame(matrix(rnorm(100, sd = 10),
            ncol = 5)), sprintf("C_%02d", seq(5)))
frqFrm <- stats::setNames(as.data.frame(matrix(sample(seq(10), 100,
            replace = TRUE), ncol = 5)), sprintf("F_%02d", seq(5)))
binFrm <- stats::setNames(as.data.frame(matrix(sample(c(TRUE, FALSE), 100,
            replace = TRUE), ncol = 5)), sprintf("B_%02d", seq(5)))
nmeOut <- tempfile(fileext = ".omv")

# calculates the distances between columns, nmeDst is not required: "euclid"
# is the default
jmvReadWrite::distances_omv(dtaInp = cntFrm, fleOut = nmeOut, varDst =
  names(cntFrm), nmeDst = "euclid")
dtaFrm <- jmvReadWrite::read_omv(nmeOut)
unlink(nmeOut)
# the resulting matrix (10 x 10) with the Euclidian distances
print(dtaFrm)
#>       C_01     C_02     C_03     C_04     C_05
#> 1  0.00000 61.34075 44.71434 65.20404 65.90022
#> 2 61.34075  0.00000 59.09327 54.25283 56.95269
#> 3 44.71434 59.09327  0.00000 51.37643 49.35176
#> 4 65.20404 54.25283 51.37643  0.00000 68.46490
#> 5 65.90022 56.95269 49.35176 68.46490  0.00000

# calculates the (Euclidean) distances between rows (clmDst = FALSE)
jmvReadWrite::distances_omv(dtaInp = cntFrm, fleOut = nmeOut, varDst =
  names(cntFrm), clmDst = FALSE, nmeDst = "euclid")
dtaFrm <- jmvReadWrite::read_omv(nmeOut)
unlink(nmeOut)
# the resulting matrix (20 x 20) with the Euclidian distances
print(dtaFrm)
#>           1         2         3        4         5        6        7        8
#> 1   0.00000 26.131543 27.350739 45.23326 35.383875 26.44391 47.99352 30.64342
#> 2  26.13154  0.000000 21.676641 35.01735 11.244887 14.79294 24.30533 29.53035
#> 3  27.35074 21.676641  0.000000 42.33862 24.433205 17.19017 28.54954 27.69381
#> 4  45.23326 35.017352 42.338618  0.00000 38.781160 38.26722 38.31024 21.45148
#> 5  35.38388 11.244887 24.433205 38.78116  0.000000 16.48204 17.55941 35.13815
#> 6  26.44391 14.792935 17.190167 38.26722 16.482038  0.00000 27.16255 29.40360
#> 7  47.99352 24.305328 28.549545 38.31024 17.559410 27.16255  0.00000 38.10638
#> 8  30.64342 29.530354 27.693815 21.45148 35.138152 29.40360 38.10638  0.00000
#> 9  30.55541 14.332681 19.731562 27.15150 15.659990 15.76215 21.57344 19.91058
#> 10 14.82706 25.909314 18.605862 41.61574 33.937495 26.36965 42.04666 23.44241
#> 11 29.65218 17.180385 32.023400 35.21951 24.293889 30.84166 33.35057 31.27000
#> 12 38.87806 17.910528 22.818187 37.92538  9.537864 17.46241 14.78549 33.04212
#> 13 25.95182 18.049475  5.264775 42.60625 20.427063 14.82885 27.14785 28.91468
#> 14 41.39663 29.524358 28.493849 51.93825 26.774265 18.75533 32.30149 45.93174
#> 15 53.75718 35.821951 32.305671 35.61084 32.200139 37.09996 19.02030 34.01429
#> 16 35.27272 26.581168 17.477468 34.05918 30.671408 28.31329 27.58806 23.06398
#> 17 32.76102 17.038073 30.549965 25.05542 22.000511 19.66510 28.59081 27.58129
#> 18 31.55798 14.507930 31.236666 27.59670 16.974705 20.20535 28.53359 27.45392
#> 19 29.17899 15.442762 31.566741 31.99236 24.559690 28.46439 32.39750 30.72126
#> 20 32.35799  7.745134 27.520743 32.46818 12.489624 20.62853 22.23422 31.74860
#>           9       10       11        12        13       14       15       16
#> 1  30.55541 14.82706 29.65218 38.878061 25.951817 41.39663 53.75718 35.27272
#> 2  14.33268 25.90931 17.18039 17.910528 18.049475 29.52436 35.82195 26.58117
#> 3  19.73156 18.60586 32.02340 22.818187  5.264775 28.49385 32.30567 17.47747
#> 4  27.15150 41.61574 35.21951 37.925376 42.606250 51.93825 35.61084 34.05918
#> 5  15.65999 33.93749 24.29389  9.537864 20.427063 26.77427 32.20014 30.67141
#> 6  15.76215 26.36965 30.84166 17.462409 14.828854 18.75533 37.09996 28.31329
#> 7  21.57344 42.04666 33.35057 14.785487 27.147849 32.30149 19.02030 27.58806
#> 8  19.91058 23.44241 31.27000 33.042120 28.914676 45.93174 34.01429 23.06398
#> 9   0.00000 25.79688 23.33919 13.683037 17.950287 32.08199 26.37753 21.96242
#> 10 25.79688  0.00000 28.20642 34.897196 19.051886 42.12985 43.65010 23.65112
#> 11 23.33919 28.20642  0.00000 29.806311 29.007094 46.63294 40.28531 30.52709
#> 12 13.68304 34.89720 29.80631  0.000000 19.793327 27.13234 26.04911 29.03732
#> 13 17.95029 19.05189 29.00709 19.793327  0.000000 27.37625 33.18633 20.31720
#> 14 32.08199 42.12985 46.63294 27.132339 27.376247  0.00000 45.12002 40.03875
#> 15 26.37753 43.65010 40.28531 26.049113 33.186331 45.12002  0.00000 24.57344
#> 16 21.96242 23.65112 30.52709 29.037323 20.317198 40.03875 24.57344  0.00000
#> 17 18.68052 33.95234 27.15950 25.599634 29.204786 31.11091 37.90084 30.69191
#> 18 14.46393 33.04019 21.53910 20.429258 28.187645 35.32704 37.61112 34.08393
#> 19 23.59061 29.08503 10.95177 30.921575 29.353719 42.61095 40.79366 28.74388
#> 20 16.74755 31.84056 16.18782 19.647393 24.401274 33.22717 34.65123 28.48199
#>          17       18       19        20
#> 1  32.76102 31.55798 29.17899 32.357991
#> 2  17.03807 14.50793 15.44276  7.745134
#> 3  30.54996 31.23667 31.56674 27.520743
#> 4  25.05542 27.59670 31.99236 32.468177
#> 5  22.00051 16.97470 24.55969 12.489624
#> 6  19.66510 20.20535 28.46439 20.628532
#> 7  28.59081 28.53359 32.39750 22.234216
#> 8  27.58129 27.45392 30.72126 31.748597
#> 9  18.68052 14.46393 23.59061 16.747548
#> 10 33.95234 33.04019 29.08503 31.840557
#> 11 27.15950 21.53910 10.95177 16.187822
#> 12 25.59963 20.42926 30.92158 19.647393
#> 13 29.20479 28.18764 29.35372 24.401274
#> 14 31.11091 35.32704 42.61095 33.227175
#> 15 37.90084 37.61112 40.79366 34.651232
#> 16 30.69191 34.08393 28.74388 28.481989
#> 17  0.00000 14.77914 20.20579 15.595960
#> 18 14.77914  0.00000 20.96006 14.086098
#> 19 20.20579 20.96006  0.00000 12.910776
#> 20 15.59596 14.08610 12.91078  0.000000

# calculates the (Euclidean) distances between columns; the original data
# are z-standardized before calculating the distances (stdDst = "z")
jmvReadWrite::distances_omv(dtaInp = cntFrm, fleOut = nmeOut, varDst =
  names(cntFrm), stdDst = "z", nmeDst = "euclid")
dtaFrm <- jmvReadWrite::read_omv(nmeOut)
unlink(nmeOut)
# the resulting matrix (10 x 10) with the Euclidian distances using the
# z-standardized data
print(dtaFrm)
#>       C_01     C_02     C_03     C_04     C_05
#> 1 0.000000 6.801907 5.166205 6.617378 7.217520
#> 2 6.801907 0.000000 6.985124 5.589434 6.358651
#> 3 5.166205 6.985124 0.000000 5.450948 5.723096
#> 4 6.617378 5.589434 5.450948 0.000000 6.972127
#> 5 7.217520 6.358651 5.723096 6.972127 0.000000

# calculates the correlations between columns
jmvReadWrite::distances_omv(dtaInp = cntFrm, fleOut = nmeOut, varDst =
  names(cntFrm), nmeDst = "correlation")
dtaFrm <- jmvReadWrite::read_omv(nmeOut)
unlink(nmeOut)
# the resulting matrix (10 x 10) with the correlations
print(dtaFrm)
#>         C_01        C_02       C_03       C_04        C_05
#> 1  1.0000000 -0.21752487  0.2976402 -0.1523604 -0.37085773
#> 2 -0.2175249  1.00000000 -0.2839989  0.1778480 -0.06401162
#> 3  0.2976402 -0.28399885  1.0000000  0.2180834  0.13805728
#> 4 -0.1523604  0.17784796  0.2180834  1.0000000 -0.27922504
#> 5 -0.3708577 -0.06401162  0.1380573 -0.2792250  1.00000000

# calculates the chi-square dissimilarity (nmeDst = "chisq") between columns
jmvReadWrite::distances_omv(dtaInp = frqFrm, fleOut = nmeOut, varDst =
  names(frqFrm), nmeDst = "chisq")
dtaFrm <- jmvReadWrite::read_omv(nmeOut)
unlink(nmeOut)
# the resulting matrix (10 x 10) with the chi-square dissimilarities
print(dtaFrm)
#>       F_01      F_02     F_03      F_04     F_05
#> 1 0.000000  8.247183 5.142857  5.911111 8.990482
#> 2 8.247183  0.000000 4.678393 10.148313 4.085714
#> 3 5.142857  4.678393 0.000000  9.252381 6.890820
#> 4 5.911111 10.148313 9.252381  0.000000 7.033762
#> 5 8.990482  4.085714 6.890820  7.033762 0.000000

# calculates the Jaccard similarity (nmeDst = "jaccard") between columns
jmvReadWrite::distances_omv(dtaInp = binFrm, fleOut = nmeOut, varDst =
  names(binFrm), nmeDst = "jaccard")
dtaFrm <- jmvReadWrite::read_omv(nmeOut)
unlink(nmeOut)
# the resulting matrix (10 x 10) with the Jaccard similarities
print(dtaFrm)
#>     B_01      B_02      B_03      B_04      B_05
#> 1 1.0000 0.4375000 0.6000000 0.4000000 0.4000000
#> 2 0.4375 1.0000000 0.4705882 0.3750000 0.4666667
#> 3 0.6000 0.4705882 1.0000000 0.2777778 0.3529412
#> 4 0.4000 0.3750000 0.2777778 1.0000000 0.3333333
#> 5 0.4000 0.4666667 0.3529412 0.3333333 1.0000000

```
