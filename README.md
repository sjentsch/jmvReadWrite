
<!-- README.md is generated from README.Rmd. Please edit that file -->

# jmvReadWrite <a href="https://sjentsch.github.io/jmvReadWrite/"><img src="man/figures/logo.svg" align="right" width="20%" /></a>

<!---
<br clear="all">
--->
<!-- badges: start -->

[![CRAN](https://www.r-pkg.org/badges/version/jmvReadWrite)](https://cran.r-project.org/package=jmvReadWrite)
[![Project Status: Active – The project has reached a stable, usable
state and is being actively
developed.](https://www.repostatus.org/badges/latest/active.svg)](https://www.repostatus.org/#active)
[![License](https://img.shields.io/badge/License-AGPL%20v3-green.svg)](https://www.gnu.org/licenses/agpl-3.0.html)
[![Downloads](https://cranlogs.r-pkg.org/badges/jmvReadWrite?color=brightgreen)](https://cran.r-project.org/package=jmvReadWrite)
[![Last
commit](https://img.shields.io/github/last-commit/sjentsch/jmvReadWrite?logo=GitHub)](https://github.com/sjentsch/jmvReadWrite)
[![Register an
issue](https://img.shields.io/github/issues/sjentsch/jmvReadWrite?color=%23fa251e&logo=GitHub)](https://github.com/sjentsch/jmvReadWrite/issues)
[![R-hub](https://github.com/sjentsch/jmvReadWrite/actions/workflows/rhub.yaml/badge.svg)](https://github.com/sjentsch/jmvReadWrite/actions/workflows/rhub.yaml)
[![R-CMD-check](https://github.com/sjentsch/jmvReadWrite/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/sjentsch/jmvReadWrite/actions/workflows/R-CMD-check.yaml)
[![CI](https://github.com/sjentsch/jmvReadWrite/actions/workflows/CI.yml/badge.svg)](https://github.com/sjentsch/jmvReadWrite/actions/workflows/CI.yml)
[![pkgcheck](https://github.com/sjentsch/jmvReadWrite/workflows/pkgcheck/badge.svg)](https://github.com/sjentsch/jmvReadWrite/actions?query=workflow%3Apkgcheck)
[![Codecov
coverage](https://codecov.io/gh/sjentsch/jmvReadWrite/branch/main/graph/badge.svg)](https://app.codecov.io/gh/sjentsch/jmvReadWrite?branch=main)
[![Documentation](https://img.shields.io/badge/documentation-is_here-blue)](https://sjentsch.github.io/jmvReadWrite/)
<!-- badges: end -->

<!---
[//]: [![Build Status](https://app.travis-ci.com/sjentsch/jmvReadWrite.svg?branch=main)](https://app.travis-ci.com/sjentsch/jmvReadWrite)
--->

The R-package `jmvReadWrite` reads and writes the .omv-files that are
used by the statistical spreadsheet `jamovi` (<https://www.jamovi.org>).
It is supposed to ease using jamovi and R together, provide helper
functions for some often required data management tasks, and to adjust
and use syntax for statistical analyses that were created using the GUI
in `jamovi` in R (in connection with the R-library `jmv`). More
recently, `jmvReadWrite` became easily available from within `jamovi` by
becoming part of the `Rj` module (where you can use it by writing R
commands, documented below), and via the `jTransform` module that
provides a graphical user interface for most helper functions.

If you find the package useful, please cite it as:

<div style="background-color:rgb(245, 245, 245); padding: 20px 20px 1px; border-radius:5px;">

<p>
Bulus M, Jentschke S (2026). <em>pwrss: Statistical Power, Sample Size,
and Detectable Effect Calculations</em>. R package version 1.3.3,
<a href="https://doi.org/10.32614/CRAN.package.pwrss">https://doi.org/10.32614/CRAN.package.pwrss</a>.
</p>

</div>

</br>

## Installation

In R, you can either install a stable version of `jmvReadWrite` which is
available on [CRAN](https://cran.r-project.org/package=jmvReadWrite)
using the following command:

``` r
install.packages("jmvReadWrite")
```

or you can install the development version of the `jmvReadWrite` package
from [GitHub](https://github.com/sjentsch/jmvReadWrite/):

``` r
if (!require(devtools)) install.packages("devtools")
devtools::install_github("sjentsch/jmvReadWrite")
```

## How to use the package?

[**read_omv**](https://sjentsch.github.io/jmvReadWrite/reference/read_omv.html)

The following code uses the ToothGrowth-data set that is part of the
data sets included in R (the current file contains some modifications
though for testing the reading and writing routines: `read_omv` and
`write_omv`). With this data set, a syntax to conduct an ANOVA is run.

The results should be similar to those obtained when running the same
analysis in jamovi (using the GUI). To do so, open the file menu (☰)
choose `Open`, `Data Library` and `ToothGrowth`. Afterwards, click on
the `ANOVA`-button in the `Analyses`-tab and choose `ANOVA`. There, you
assign the variable `len` to `Dependent Variable` and `supp` and `dose`
to `Fixed Factors`. Afterwards, you choose / tick `Overall Model Test`
and `ω²`. Open the drop-down menu `Assumption Checks` and tick
`Homogeneity test` and `Normality test`. The results should be identical
apart from that the table output looks nicer in `jamovi` (not only text,
as below), numbers are rounded and maybe one or two other cosmetic
differences.

If you want to copy the syntax generated in jamovi, you have to switch
on the
[`Syntax Mode`](https://jamovi.readthedocs.io/en/latest/usermanual/um_6_jamovi_and_R/#syntax-mode).
Afterwards, the syntax is shown at the top of the analysis and can be
copied from there.

``` r
fleOMV <- system.file("extdata", "ToothGrowth.omv", package = "jmvReadWrite")
data <- jmvReadWrite::read_omv(fleOMV)
# if the "jmv"-package is installed, we can run a test analysis with the data
if (nzchar(system.file(package = "jmv"))) {
    jmv::ANOVA(
        formula = len ~ supp + dose + supp:dose,
        data = data,
        effectSize = "omega",
        modelTest = TRUE,
        homo = TRUE,
        norm = TRUE)
    }
```

<div class="jmv-results">
<style>
.jmv-results {
    font-family: "Segoe UI", Roboto, Helvetica, Arial, sans-serif, "Segoe UI Emoji", "Segoe UI Symbol";
    color: #333333;
    font-size: 12px;
}
.jmv-results h1, .jmv-results h2, .jmv-results h3,
.jmv-results h4, .jmv-results h5, .jmv-results h6 {
    font-family: inherit; font-weight: bold; border: none; padding: 0;
    margin-top: 16px; margin-bottom: 12px; font-size: 110%; color: inherit;
}
.jmv-results h1 { font-size: 160%; color: #3E6DA9; margin-top: 24px; }
.jmv-results h2 { font-size: 130%; color: #3E6DA9; margin-top: 24px; }
.jmv-results table { width: auto; margin: 0; }
.jmv-results th { font-weight: normal; }
.jmv-results img { max-width: 100%; height: auto; }
.jmv-results a { color: #3E6DA9; }
</style>
<h1>ANOVA</h1>
<table data-quarto-disable-processing="true" style="border-collapse: collapse;"><thead><tr><th colspan="7" style="padding: 4px 8px 4px 0px; position: relative; text-align: left; border-bottom: 1px solid #333333;">ANOVA - len</th></tr><tr><th style="padding: 4px 8px 4px 8px; position: relative; text-align: center; vertical-align: bottom; border-bottom: 1px solid #333333;"></th><th style="padding: 4px 8px 4px 8px; position: relative; text-align: center; vertical-align: bottom; border-bottom: 1px solid #333333;">Sum of Squares</th><th style="padding: 4px 8px 4px 8px; position: relative; text-align: center; vertical-align: bottom; border-bottom: 1px solid #333333;">df</th><th style="padding: 4px 8px 4px 8px; position: relative; text-align: center; vertical-align: bottom; border-bottom: 1px solid #333333;">Mean Square</th><th style="padding: 4px 8px 4px 8px; position: relative; text-align: center; vertical-align: bottom; border-bottom: 1px solid #333333;">F</th><th style="padding: 4px 8px 4px 8px; position: relative; text-align: center; vertical-align: bottom; border-bottom: 1px solid #333333;">p</th><th style="padding: 4px 8px 4px 8px; position: relative; text-align: center; vertical-align: bottom; border-bottom: 1px solid #333333;">ω²</th></tr></thead><tbody><tr><td style="padding: 8px 8px 8px 8px; position: relative; text-align: left;">Overall model</td><td style="padding: 8px 20px 8px 8px; position: relative; text-align: right;">2740.1</td><td style="padding: 8px 20px 8px 8px; position: relative; text-align: right;">5</td><td style="padding: 8px 20px 8px 8px; position: relative; text-align: right;">548.02</td><td style="padding: 8px 20px 8px 8px; position: relative; text-align: right;">41.557</td><td style="padding: 8px 20px 8px 8px; position: relative; text-align: right;">&lt; .001</td><td style="padding: 4px 8px 4px 8px; position: relative;"></td></tr><tr><td style="padding: 8px 8px 4px 8px; position: relative; text-align: left;">supp</td><td style="padding: 8px 20px 4px 8px; position: relative; text-align: right;">205.4</td><td style="padding: 8px 20px 4px 8px; position: relative; text-align: right;">1</td><td style="padding: 8px 20px 4px 8px; position: relative; text-align: right;">205.35</td><td style="padding: 8px 20px 4px 8px; position: relative; text-align: right;">15.572</td><td style="padding: 8px 20px 4px 8px; position: relative; text-align: right;">&lt; .001</td><td style="padding: 8px 20px 4px 8px; position: relative; text-align: right;">0.0555</td></tr><tr><td style="padding: 4px 8px 4px 8px; position: relative; text-align: left;">dose</td><td style="padding: 4px 20px 4px 8px; position: relative; text-align: right;">2426.4</td><td style="padding: 4px 20px 4px 8px; position: relative; text-align: right;">2</td><td style="padding: 4px 20px 4px 8px; position: relative; text-align: right;">1213.22</td><td style="padding: 4px 20px 4px 8px; position: relative; text-align: right;">92.000</td><td style="padding: 4px 20px 4px 8px; position: relative; text-align: right;">&lt; .001</td><td style="padding: 4px 20px 4px 8px; position: relative; text-align: right;">0.6926</td></tr><tr><td style="padding: 4px 8px 8px 8px; position: relative; text-align: left;">supp:dose</td><td style="padding: 4px 20px 8px 8px; position: relative; text-align: right;">108.3</td><td style="padding: 4px 20px 8px 8px; position: relative; text-align: right;">2</td><td style="padding: 4px 20px 8px 8px; position: relative; text-align: right;">54.16</td><td style="padding: 4px 20px 8px 8px; position: relative; text-align: right;">4.107</td><td style="padding: 4px 20px 8px 8px; position: relative; text-align: right;">.022</td><td style="padding: 4px 20px 8px 8px; position: relative; text-align: right;">0.0236</td></tr><tr><td style="padding: 8px 8px 8px 8px; position: relative; text-align: left; border-bottom: 2px solid #333333;">Residuals</td><td style="padding: 8px 20px 8px 8px; position: relative; text-align: right; border-bottom: 2px solid #333333;">712.1</td><td style="padding: 8px 20px 8px 8px; position: relative; text-align: right; border-bottom: 2px solid #333333;">54</td><td style="padding: 8px 20px 8px 8px; position: relative; text-align: right; border-bottom: 2px solid #333333;">13.19</td><td style="padding: 4px 8px 4px 8px; position: relative; border-bottom: 2px solid #333333;"></td><td style="padding: 4px 8px 4px 8px; position: relative; border-bottom: 2px solid #333333;"></td><td style="padding: 4px 8px 4px 8px; position: relative; border-bottom: 2px solid #333333;"></td></tr></tbody></table>
<p></p>
<h2>Assumption Checks</h2>
<table data-quarto-disable-processing="true" style="border-collapse: collapse;"><thead><tr><th colspan="4" style="padding: 4px 8px 4px 0px; position: relative; text-align: left; border-bottom: 1px solid #333333;">Homogeneity of Variances Test (Levene's)</th></tr><tr><th style="padding: 4px 8px 4px 8px; position: relative; text-align: center; vertical-align: bottom; border-bottom: 1px solid #333333;">F</th><th style="padding: 4px 8px 4px 8px; position: relative; text-align: center; vertical-align: bottom; border-bottom: 1px solid #333333;">df1</th><th style="padding: 4px 8px 4px 8px; position: relative; text-align: center; vertical-align: bottom; border-bottom: 1px solid #333333;">df2</th><th style="padding: 4px 8px 4px 8px; position: relative; text-align: center; vertical-align: bottom; border-bottom: 1px solid #333333;">p</th></tr></thead><tbody><tr><td style="padding: 4px 20px 4px 8px; position: relative; text-align: right; border-bottom: 2px solid #333333;">1.940</td><td style="padding: 4px 20px 4px 8px; position: relative; text-align: right; border-bottom: 2px solid #333333;">5</td><td style="padding: 4px 20px 4px 8px; position: relative; text-align: right; border-bottom: 2px solid #333333;">54</td><td style="padding: 4px 20px 4px 8px; position: relative; text-align: right; border-bottom: 2px solid #333333;">.103</td></tr></tbody></table>
<p></p>
<table data-quarto-disable-processing="true" style="border-collapse: collapse;"><thead><tr><th colspan="2" style="padding: 4px 8px 4px 0px; position: relative; text-align: left; border-bottom: 1px solid #333333;">Normality Test (Shapiro-Wilk)</th></tr><tr><th style="padding: 4px 8px 4px 8px; position: relative; text-align: center; vertical-align: bottom; border-bottom: 1px solid #333333;">Statistic</th><th style="padding: 4px 8px 4px 8px; position: relative; text-align: center; vertical-align: bottom; border-bottom: 1px solid #333333;">p</th></tr></thead><tbody><tr><td style="padding: 4px 20px 4px 8px; position: relative; text-align: right; border-bottom: 2px solid #333333;">0.9850</td><td style="padding: 4px 20px 4px 8px; position: relative; text-align: right; border-bottom: 2px solid #333333;">.669</td></tr></tbody></table>
<p></p>
</div>

Since version 0.2.0,
[`read_omv`](https://sjentsch.github.io/jmvReadWrite/reference/read_omv.html)
also extracts the syntax from analyses that you may have conducted in
the jamovi-GUI and that are stored in the .omv-file. To extract them,
you have to set the parameter `getSyn = TRUE` when calling
[`read_omv`](https://sjentsch.github.io/jmvReadWrite/reference/read_omv.html)
(default is `FALSE`). When the parameter is set, the analyses are stored
in the attribute `syntax`. They can be used as shown in the following
examples:

``` r
fleOMV <- system.file("extdata", "ToothGrowth.omv", package = "jmvReadWrite")
data <- jmvReadWrite::read_omv(fleOMV, getSyn = TRUE)
# shows the syntax of the analyses from the .omv-file
# please note that syntax extraction may not work on all systems
# if the syntax couldn't be extracted, an empty list (length = 0) is returned,
# otherwise, the syntax of the analyses from the .omv-file is shown and
# the commands of the first and the second analysis are run, with the
# output of the second analysis assigned to the variable result
if (length(attr(data, "syntax")) >= 2) {
    attr(data, "syntax")
    # if the "jmv"-package is installed, we can run the analyses in "syntax"
    if (nzchar(system.file(package = "jmv"))) {
        eval(parse(text = attr(data, "syntax")[[1]]))
        eval(parse(text = paste0("result = ", attr(data, "syntax")[[2]])))
        names(result)
        # -> "main"      "assump"    "contrasts" "postHoc"   "emm"
        # (the names of the five output tables)
    }
}
#> [1] "main"      "assump"    "contrasts" "postHoc"   "emm"       "residsOV"
```

<br/>

[**write_omv**](https://sjentsch.github.io/jmvReadWrite/reference/write_omv.html)

The `jmvReadWrite`-package also enables you to write `.omv`-files in
order to use them in `jamovi`. Let’s assume that you have a large
collection of log-files (e.g., from an experiment) that you compile and
process (summarize, filter, etc.) in R in order to later analyse them in
`jamovi`. You will have those processed log-files stored in a data frame
(called, e.g., `data`) which you then write to a file that you can open
in jamovi afterwards.

``` r
# use the data set "ToothGrowth" and, if it exists, write it as jamovi-file
# using write_omv()
data("ToothGrowth", package = "jmvReadWrite")
# "retDbg" has to be set in order to return debug information to wrtDta
wrtDta <- jmvReadWrite::write_omv(ToothGrowth, "Trial.omv", retDbg = TRUE)
names(wrtDta)
#> [1] "mtaDta" "xtdDta" "dtaFrm"
# -> "mtaDta" "xtdDta" "dtaFrm"
# this debug information contains a list with the metadata ("mtaDta", e.g.,
# column and data type), the extended data ("xtdDta", e.g., variable lables),
# and the data frame (dtaFrm) for checking (understanding the file format) and
# debugging

# check whether the file was written to the disk, get the file information (size, etc.)
# and delete the file afterwards
list.files(".", "Trial.omv")
#> [1] "Trial.omv"
file.info("Trial.omv")[, c("size", "isdir", "mode")]
#>           size isdir mode
#> Trial.omv 2522 FALSE  644
unlink("Trial.omv")
```

Although jamovi reads R-data files (.RData, .rda, .rds)
[`write_omv`](https://sjentsch.github.io/jmvReadWrite/reference/write_omv.html)
permits to store `jamovi`-specific attributes (such as variable labels)
in addition. Please note that if you are reading from an `.omv`-file in
order to write back to an `.omv`-file (perhaps after some
modifications), it is recommended to leave the `sveAtt`-attribute set to
`TRUE` (which is the default).

``` r
# reading and writing a file with the "sveAtt"-parameter permits you to keep
# essential meta-data to ensure that the written file looks and works like the
# original file (plus you modifications)
fleOMV <- system.file("extdata", "ToothGrowth.omv", package = "jmvReadWrite")
data <- jmvReadWrite::read_omv(fleOMV, sveAtt = TRUE)
# shows the names of the attributes for the whole data set (e.g., number of
# rows and columns) and the names of the attributes of the first column
names(attributes(data))
#> [1] "names"       "row.names"   "class"       "fltLst"      "removedRows"
#> [6] "addedRows"   "transforms"
names(attributes(data[[1]]))
#>  [1] "name"           "id"             "columnType"     "dataType"      
#>  [5] "measureType"    "formula"        "formulaMessage" "parentId"      
#>  [9] "width"          "type"           "importName"     "description"   
#> [13] "transform"      "edits"          "missingValues"  "filterNo"      
#> [17] "active"
#
# perhaps do some modifications to the file here and write it back afterwards
jmvReadWrite::write_omv(data, "Trial.omv")
unlink("Trial.omv")
```

If `Trial.omv` in the example above would have been kept, it should look
like the original file (plus your possible modifications). If you, e.g.,
added a new column, you could adjust some attributes (e.g., to enforce a
specific `columnType` or `measurementType`): just look at how attributes
are stored for other columns. <br /><br />

**Helper functions**

`jmvReadWrite` contains a number of helper functions that assist you
with data management tasks that are frequently required:

- [`aggregate_omv`](https://sjentsch.github.io/jmvReadWrite/reference/aggregate_omv.html):
  Aggregates data (calculating, e.g., mean, median, etc.) from data sets
  in long format.

- [`arrange_cols_omv`](https://sjentsch.github.io/jmvReadWrite/reference/arrange_cols_omv.html):
  Re-arranges the columns of your data file in a requested order.

- [`combine_cols_omv`](https://sjentsch.github.io/jmvReadWrite/reference/combine_cols_omv.html):
  Combines the data from two columns into one. The function assumes that
  the data contained in the two columns are the same. It checks for (and
  thereby ensures) equality and replaces missing values in one column by
  replacing those with values from the second column (if those are not
  missing as well).

- [`convert_to_omv`](https://sjentsch.github.io/jmvReadWrite/reference/convert_to_omv.html):
  Converts data sets from other file formats into jamovi-format. This
  function may be helpful if you have to convert a larger amount of
  files.

- [`describe_omv`](https://sjentsch.github.io/jmvReadWrite/reference/describe_omv.html):
  Adds a title and a description to a data set. This function may be
  helpful for documenting what is contained in a data set, e.g. for
  publishing them in a repository such as OSF, or for generated data
  sets, e.g. those used in teaching.

- [`distances_omv`](https://sjentsch.github.io/jmvReadWrite/reference/distances_omv.html):
  Calculates a wide range of distances measures (for continuous,
  frequency or binary data). If can be determined, whether the
  calculation of the distances should be carried out between columns /
  variables or between rows / units of observation. The original data
  can be standardized before the distances are calculated.

- [`label_vars_omv`](https://sjentsch.github.io/jmvReadWrite/reference/label_vars_omv.html):
  Labels columns / variables in jamovi data files, using either a CSV
  file, a data frame (each containing two columns, one with the variable
  names, the other with the variable labels), or a character vector with
  the variable labels.

- [`long2wide_omv`](https://sjentsch.github.io/jmvReadWrite/reference/long2wide_omv.html):
  Converts a data set from long to wide format: Time points for repeated
  measurements are arranged as rows in the original and converted into
  columns.

- [`wide2long_omv`](https://sjentsch.github.io/jmvReadWrite/reference/wide2long_omv.html):
  Converts a data set from wide to long format: Time points for repeated
  measurements are arranged as columns in the original and converted
  into rows.

- [`merge_cols_omv`](https://sjentsch.github.io/jmvReadWrite/reference/merge_cols_omv.html):
  Add variables from several data sets, i.e. the variables / columns in
  the second, etc. input data set are added as columns to the first data
  set.

- [`merge_rows_omv`](https://sjentsch.github.io/jmvReadWrite/reference/merge_rows_omv.html):
  Add cases from several data sets, i.e. the cases / rows in the second,
  etc. data set are added as rows to the first data set.

- [`sort_omv`](https://sjentsch.github.io/jmvReadWrite/reference/sort_omv.html):
  Sort a data set according to one or more variable(s).

- [`transform_vars_omv`](https://sjentsch.github.io/jmvReadWrite/reference/transform_vars_omv.html):
  Transform skewed variables (aiming at they better conform to a normal
  distribution).

- [`transpose_omv`](https://sjentsch.github.io/jmvReadWrite/reference/transpose_omv.html):
  Transpose a data set: Make rows into columns and vice versa.

------------------------------------------------------------------------

[Changelog](https://github.com/sjentsch/jmvReadWrite/blob/main/NEWS.md)

## Authors

[Sebastian Jentschke](https://www.uib.no/en/persons/Sebastian.Jentschke)

## License

[AGPL 3](https://github.com/sjentsch/jmvReadWrite/blob/main/LICENSE)

## Giving back

If you find this package helpful, please consider donating to the jamovi
project (via the Patreon-link on the left side). If you can’t give
money, but would like to support us in another way, you may contribute
to translating [jamovi](https://hosted.weblate.org/engage/jamovi/), the
[jamovi documentation](https://hosted.weblate.org/engage/jamovidocs/),
or the textbook [”learning statistics with
jamovi“](https://hosted.weblate.org/engage/jamovi/) into your language.

Thank you for your support!
