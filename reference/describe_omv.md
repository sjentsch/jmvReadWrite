# Adds a title and a description for a data set stored as .omv-file for the statistical spreadsheet 'jamovi' (<https://www.jamovi.org>)

Adds a title and a description for a data set stored as .omv-file for
the statistical spreadsheet 'jamovi' (<https://www.jamovi.org>)

## Usage

``` r
describe_omv(
  dtaInp = NULL,
  fleOut = "",
  dtaTtl = c(),
  dtaDsc = c(),
  lngDsc = "EN",
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

- dtaTtl:

  Character vector with a title to be added to the data set (see
  Details; default: "")

- dtaDsc:

  Description of the data set, either as character vector
  (HTML-formatted) or as named list with the entries "description",
  "variables", "references", and "license" (see Details; default: "")

- lngDsc:

  Language of the description (localizes the description components:
  "Description", "Variables", "References", and "License"; default:
  "EN")

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

a data frame (only returned if `fleOut` is empty) where a description
and a title are added to the input data

## Details

- The aim of this function is to add a title and a data set description
  to jamovi data files. Two typical use cases would be (1) to help
  creating data sets to be used in teaching (i.e., either creating or
  using data sets in R, and afterwards adding a description to those),
  and (2) to provide "properly described" data when publishing in a
  repository such as the OSF).

- NB: The data set should not contain any existing analyses. These will
  be overwritten (a warning is issued informing you about that).

- `dtaTtl` is a title for the dataset (at the top of the results output,
  i.e., that title which initially is "Results" when you create a new
  data set in jamovi).

- `dtaDsc` can either be a character vector (with length = 1) containing
  HTML-formatted text that describes the data set (see `chrDsc` in the
  examples for HTML tags that are currently implemented; putting
  "unformatted" text is not a problem, but then the result is just plain
  text without formatting). Alternatively, `dtaDcs` can be a named list
  with the entries `description`, `variables`, `references`, `license`.
  All entries except from `variables` contain character vectors (length
  = 1); `variables` shall be a named list with the variable name as name
  and a description what the variable contains as entry. `description`
  and `variables` must be given, `references` and `license` can be left
  blank (""; but the names must be present in the list). An example for
  both a named list with a description (`lstDsc`), as well as a
  character vector with all HTML tags that are implemented (`chrDsc`)
  can be found in the examples below.

- The ellipsis-parameter (`...`) can be used to submit arguments /
  parameters to the functions that are used for reading and writing the
  data. By clicking on the respective function under "See also", you can
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

`describe_omv` internally uses the following functions for reading and
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
dtaFrm <- jmvReadWrite::ToothGrowth[, c("len", "supp", "dose")]
nmeOut <- tempfile(fileext = ".omv")

# the paste's underneath are only for readability (without them, the vignettes are misformatted)
lstDsc <- list(description = paste("The response is the length of odontoblasts (cells responsible",
                                   "for tooth growth) in 60 guinea pigs. Each animal received one",
                                   "of three dose levels of vitamin C (0.5, 1, and 2 mg / day) by",
                                   "one of two delivery methods, orange juice or ascorbic acid (a",
                                   "form of vitamin C and coded as VC)."),
               variables = list(len  = "Tooth length",
                                supp = "Supplement type (VC or OJ)",
                                dose = "Dose (in milligrams / day"),
               references = paste("Crampton, E. W. (1947). The growth of the odontoblast of the",
                                  "incisor teeth as a criterion of vitamin C intake of the guinea",
                                  "pig. <em>The Journal of Nutrition, 33</em>(5), 491-504.",
                                  "https://doi.org/10.1093/jn/33.5.491"),
               license = "")
jmvReadWrite::describe_omv(dtaInp = dtaFrm, fleOut = nmeOut, dtaTtl = "ToothGrowth",
  dtaDsc = lstDsc)
# don't include the unlink, if you copy the code and want to look at the resulting output file
unlink(nmeOut)

# the code underneath should cover all formatting options jamovi is able to use (paste0 is only
# for readability)
chrDsc <- paste0("<p><strong>Trial - all formattings:</strong><br/><strong>bold</strong><br/>",
                 "<strong><em>bold, italics</em></strong><br/><em>italics</em><br/><u>underlined",
                 "</u><br/><s>strikethrough</s><br/>C<sub>2</sub>H<sub>5</sub>OH<br/>R<sup>2",
                 "</sup><br/><span style=\"background-color:#e60000\">background colour: red",
                 "</span><br/><span style=\"color:#e60000\">foreground color: red",
                 "</span></p><p class=\"ql-align-center\">centered</p><p class=\"ql-align-right\">",
                 "right</p><p class=\"ql-align-justify\">justify justify justify justify justify ",
                 "justify justify justify justify justify justify justify justify justify justify ",
                 "justify justify justify justify justify justify justify justify justify justify",
                 "</p><p><br/></p><ol><li>numbered list</li><li>numbered list</li></ol><p><br/>",
                 "</p><ul><li>bullet point</li><li>bullet point</li></ul><p class=\"ql-indent-1\">",
                 "indented once</p><p class=\"ql-indent-2\">indented twice</p><p ",
                 "class=\"ql-indent-1\">indented once</p><p>Formula: <span class=\"ql-formula\">",
                 "e=mc^2</span></p><pre>Preformatted</pre><p>normal again</p><h2>Heading</h2>")
jmvReadWrite::describe_omv(dtaInp = dtaFrm, fleOut = nmeOut, dtaTtl = "ToothGrowth",
  dtaDsc = chrDsc)
unlink(nmeOut)

```
