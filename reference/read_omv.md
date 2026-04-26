# Read files created of the statistical spreadsheet 'jamovi' (<https://www.jamovi.org>)

Read files created of the statistical spreadsheet 'jamovi'
(<https://www.jamovi.org>)

## Usage

``` r
read_omv(
  fleInp = "",
  useFlt = FALSE,
  rmMsVl = FALSE,
  sveAtt = TRUE,
  getSyn = FALSE,
  getHTM = FALSE
)
```

## Arguments

- fleInp:

  Name (including the path, if required) of the 'jamovi'-file to be read
  ("FILENAME.omv"; default: "")

- useFlt:

  Apply filters (remove the lines where the filter is set to 0; default:
  FALSE)?

- rmMsVl:

  Remove values defined as missing values (replace them with NA;
  default: FALSE)?

- sveAtt:

  Store attributes that are not required in the data set (if you want to
  write the same data set using write_omv; default: FALSE)?

- getSyn:

  Extract syntax from the analyses in the 'jamovi'-file and store it in
  the attribute "syntax" (default: FALSE)?

- getHTM:

  Store index.html in the attribute "HTML" (default: FALSE)?

## Value

data frame (can be directly used with functions included in the
R-package `jmv` and syntax from 'jamovi'; also compatible with the
format of the R-package `foreign`)

## Examples

``` r
nmeInp <- system.file("extdata", "ToothGrowth.omv", package = "jmvReadWrite")
data <- jmvReadWrite::read_omv(fleInp = nmeInp)
str(data)
#> 'data.frame':    60 obs. of  16 variables:
#>  $ Filter 1          : logi  TRUE TRUE TRUE TRUE TRUE TRUE ...
#>   ..- attr(*, "name")= chr "Filter 1"
#>   ..- attr(*, "id")= int 18
#>   ..- attr(*, "columnType")= chr "Filter"
#>   ..- attr(*, "dataType")= chr "Integer"
#>   ..- attr(*, "measureType")= chr "Nominal"
#>   ..- attr(*, "formula")= chr "logLen < 1.5"
#>   ..- attr(*, "formulaMessage")= chr ""
#>   ..- attr(*, "parentId")= int 0
#>   ..- attr(*, "width")= int 78
#>   ..- attr(*, "type")= chr "integer"
#>   ..- attr(*, "importName")= chr ""
#>   ..- attr(*, "description")= chr ""
#>   ..- attr(*, "transform")= int 0
#>   ..- attr(*, "edits")= list()
#>   ..- attr(*, "missingValues")= list()
#>   ..- attr(*, "filterNo")= int 0
#>   ..- attr(*, "active")= logi FALSE
#>  $ ID                : chr  "1" "2" "3" "4" ...
#>   ..- attr(*, "jmv-id")= logi TRUE
#>   ..- attr(*, "name")= chr "ID"
#>   ..- attr(*, "id")= int 24
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
#>  $ logLen            : num  0.623 1.061 0.863 0.763 0.806 ...
#>   ..- attr(*, "jmv-desc")= chr "Trial with log10"
#>   ..- attr(*, "name")= chr "logLen"
#>   ..- attr(*, "id")= int 10
#>   ..- attr(*, "columnType")= chr "Computed"
#>   ..- attr(*, "dataType")= chr "Decimal"
#>   ..- attr(*, "measureType")= chr "Continuous"
#>   ..- attr(*, "formula")= chr "LOG10(len)"
#>   ..- attr(*, "formulaMessage")= chr ""
#>   ..- attr(*, "parentId")= int 0
#>   ..- attr(*, "width")= int 100
#>   ..- attr(*, "type")= chr "number"
#>   ..- attr(*, "importName")= chr ""
#>   ..- attr(*, "description")= chr "Trial with log10"
#>   ..- attr(*, "transform")= int 0
#>   ..- attr(*, "edits")= list()
#>   ..- attr(*, "missingValues")= list()
#>  $ supp - Transform 1: Factor w/ 2 levels "1","2": 1 1 1 1 1 1 1 1 1 1 ...
#>   ..- attr(*, "values")= int [1:2] 1 2
#>   ..- attr(*, "name")= chr "supp - Transform 1"
#>   ..- attr(*, "id")= int 12
#>   ..- attr(*, "columnType")= chr "Recoded"
#>   ..- attr(*, "dataType")= chr "Integer"
#>   ..- attr(*, "measureType")= chr "Nominal"
#>   ..- attr(*, "formula")= chr "RECODE(`supp`, IFMISS($source, NA, 1), MATCH($source, 'VC', 'OJ'))"
#>   ..- attr(*, "formulaMessage")= chr ""
#>   ..- attr(*, "parentId")= int 3
#>   ..- attr(*, "width")= int 100
#>   ..- attr(*, "type")= chr "integer"
#>   ..- attr(*, "importName")= chr ""
#>   ..- attr(*, "description")= chr ""
#>   ..- attr(*, "transform")= int 1
#>   ..- attr(*, "edits")= list()
#>   ..- attr(*, "missingValues")= list()
#>   ..- attr(*, "trimLevels")= logi TRUE
#>  $ len               : num  4.2 11.5 7.3 5.8 6.4 10 11.2 11.2 5.2 7 ...
#>   ..- attr(*, "name")= chr "len"
#>   ..- attr(*, "id")= int 2
#>   ..- attr(*, "columnType")= chr "Data"
#>   ..- attr(*, "dataType")= chr "Decimal"
#>   ..- attr(*, "measureType")= chr "Continuous"
#>   ..- attr(*, "formula")= chr ""
#>   ..- attr(*, "formulaMessage")= chr ""
#>   ..- attr(*, "parentId")= int 0
#>   ..- attr(*, "width")= int 100
#>   ..- attr(*, "type")= chr "number"
#>   ..- attr(*, "importName")= chr "len"
#>   ..- attr(*, "description")= chr ""
#>   ..- attr(*, "transform")= int 0
#>   ..- attr(*, "edits")= list()
#>   ..- attr(*, "missingValues")= list()
#>  $ supp              : Factor w/ 2 levels "OJ","VC": 2 2 2 2 2 2 2 2 2 2 ...
#>   ..- attr(*, "name")= chr "supp"
#>   ..- attr(*, "id")= int 3
#>   ..- attr(*, "columnType")= chr "Data"
#>   ..- attr(*, "dataType")= chr "Text"
#>   ..- attr(*, "measureType")= chr "Nominal"
#>   ..- attr(*, "formula")= chr ""
#>   ..- attr(*, "formulaMessage")= chr ""
#>   ..- attr(*, "parentId")= int 0
#>   ..- attr(*, "width")= int 100
#>   ..- attr(*, "type")= chr "integer"
#>   ..- attr(*, "importName")= chr "supp"
#>   ..- attr(*, "description")= chr ""
#>   ..- attr(*, "transform")= int 0
#>   ..- attr(*, "edits")= list()
#>   ..- attr(*, "missingValues")= list()
#>   ..- attr(*, "trimLevels")= logi TRUE
#>  $ dose              : num  0.5 0.5 0.5 0.5 0.5 0.5 0.5 0.5 0.5 0.5 ...
#>   ..- attr(*, "name")= chr "dose"
#>   ..- attr(*, "id")= int 4
#>   ..- attr(*, "columnType")= chr "Data"
#>   ..- attr(*, "dataType")= chr "Decimal"
#>   ..- attr(*, "measureType")= chr "Continuous"
#>   ..- attr(*, "formula")= chr ""
#>   ..- attr(*, "formulaMessage")= chr ""
#>   ..- attr(*, "parentId")= int 0
#>   ..- attr(*, "width")= int 211
#>   ..- attr(*, "type")= chr "number"
#>   ..- attr(*, "importName")= chr "dose"
#>   ..- attr(*, "description")= chr ""
#>   ..- attr(*, "transform")= int 0
#>   ..- attr(*, "edits")= list()
#>   ..- attr(*, "missingValues")= list()
#>  $ dose2             : Ord.factor w/ 3 levels "0.5"<"1.0"<"2.0": 1 1 1 1 1 1 1 1 1 1 ...
#>   ..- attr(*, "name")= chr "dose2"
#>   ..- attr(*, "id")= int 31
#>   ..- attr(*, "columnType")= chr "Data"
#>   ..- attr(*, "dataType")= chr "Text"
#>   ..- attr(*, "measureType")= chr "Ordinal"
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
#>   ..- attr(*, "trimLevels")= logi TRUE
#>  $ dose3             : Factor w/ 3 levels "500","1000","2000": 1 1 1 1 1 1 1 1 1 1 ...
#>   ..- attr(*, "values")= int [1:3] 500 1000 2000
#>   ..- attr(*, "name")= chr "dose3"
#>   ..- attr(*, "id")= int 49
#>   ..- attr(*, "columnType")= chr "Data"
#>   ..- attr(*, "dataType")= chr "Integer"
#>   ..- attr(*, "measureType")= chr "Nominal"
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
#>   ..- attr(*, "trimLevels")= logi TRUE
#>  $ Trial             : Factor w/ 17 levels "A","B","C","D",..: 1 2 2 3 3 3 4 4 4 4 ...
#>   ..- attr(*, "jmv-desc")= chr "Trial"
#>   ..- attr(*, "name")= chr "Trial"
#>   ..- attr(*, "id")= int 5
#>   ..- attr(*, "columnType")= chr "Data"
#>   ..- attr(*, "dataType")= chr "Text"
#>   ..- attr(*, "measureType")= chr "Nominal"
#>   ..- attr(*, "formula")= chr ""
#>   ..- attr(*, "formulaMessage")= chr ""
#>   ..- attr(*, "parentId")= int 0
#>   ..- attr(*, "width")= int 100
#>   ..- attr(*, "type")= chr "integer"
#>   ..- attr(*, "importName")= chr ""
#>   ..- attr(*, "description")= chr "Trial"
#>   ..- attr(*, "transform")= int 0
#>   ..- attr(*, "edits")= list()
#>   ..- attr(*, "missingValues")= list()
#>   ..- attr(*, "trimLevels")= logi FALSE
#>  $ Residuals         : num  -3.78 3.52 -0.68 -2.18 -1.58 ...
#>   ..- attr(*, "jmv-desc")= chr "Residuals from ANOVA"
#>   ..- attr(*, "name")= chr "Residuals"
#>   ..- attr(*, "id")= int 32
#>   ..- attr(*, "columnType")= chr "Output"
#>   ..- attr(*, "dataType")= chr "Decimal"
#>   ..- attr(*, "measureType")= chr "Continuous"
#>   ..- attr(*, "formula")= chr ""
#>   ..- attr(*, "formulaMessage")= chr ""
#>   ..- attr(*, "parentId")= int 0
#>   ..- attr(*, "width")= int 100
#>   ..- attr(*, "type")= chr "number"
#>   ..- attr(*, "outputAnalysisId")= int 2
#>   ..- attr(*, "outputOptionName")= chr "residsOV"
#>   ..- attr(*, "outputName")= chr "1"
#>   ..- attr(*, "outputDesiredColumnName")= chr "Residuals"
#>   ..- attr(*, "outputAssignedColumnName")= chr "Residuals"
#>   ..- attr(*, "importName")= chr ""
#>   ..- attr(*, "description")= chr "Residuals from ANOVA"
#>   ..- attr(*, "transform")= int 0
#>   ..- attr(*, "edits")= list()
#>   ..- attr(*, "missingValues")= list()
#>  $ J                 : num  0.25 0.25 0.25 0.25 0.25 0.25 0.25 0.25 0.25 0.25 ...
#>   ..- attr(*, "name")= chr "J"
#>   ..- attr(*, "id")= int 33
#>   ..- attr(*, "columnType")= chr "Computed"
#>   ..- attr(*, "dataType")= chr "Decimal"
#>   ..- attr(*, "measureType")= chr "Continuous"
#>   ..- attr(*, "formula")= chr "dose ^ 2"
#>   ..- attr(*, "formulaMessage")= chr ""
#>   ..- attr(*, "parentId")= int 0
#>   ..- attr(*, "width")= int 100
#>   ..- attr(*, "type")= chr "number"
#>   ..- attr(*, "importName")= chr ""
#>   ..- attr(*, "description")= chr ""
#>   ..- attr(*, "transform")= int 0
#>   ..- attr(*, "edits")= list()
#>   ..- attr(*, "missingValues")= list()
#>  $ K                 : num  -1.64 2.01 -0.09 -0.84 -0.54 ...
#>   ..- attr(*, "name")= chr "K"
#>   ..- attr(*, "id")= int 34
#>   ..- attr(*, "columnType")= chr "Computed"
#>   ..- attr(*, "dataType")= chr "Decimal"
#>   ..- attr(*, "measureType")= chr "Continuous"
#>   ..- attr(*, "formula")= chr "MEAN(dose, Residuals)"
#>   ..- attr(*, "formulaMessage")= chr ""
#>   ..- attr(*, "parentId")= int 0
#>   ..- attr(*, "width")= int 100
#>   ..- attr(*, "type")= chr "number"
#>   ..- attr(*, "importName")= chr ""
#>   ..- attr(*, "description")= chr ""
#>   ..- attr(*, "transform")= int 0
#>   ..- attr(*, "edits")= list()
#>   ..- attr(*, "missingValues")= list()
#>  $ L                 : int  1 2 3 4 5 6 7 8 9 10 ...
#>   ..- attr(*, "name")= chr "L"
#>   ..- attr(*, "id")= int 35
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
#>   ..- attr(*, "missingValues")=List of 2
#>   .. ..$ : chr "== 99"
#>   .. ..$ : chr "== 88"
#>  $ M                 : logi  TRUE FALSE FALSE FALSE FALSE TRUE ...
#>   ..- attr(*, "name")= chr "M"
#>   ..- attr(*, "id")= int 55
#>   ..- attr(*, "columnType")= chr "Data"
#>   ..- attr(*, "dataType")= chr "Integer"
#>   ..- attr(*, "measureType")= chr "Nominal"
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
#>   ..- attr(*, "trimLevels")= logi TRUE
#>  $ weights           : int  1 1 1 1 1 1 1 1 1 1 ...
#>   ..- attr(*, "name")= chr "weights"
#>   ..- attr(*, "id")= int 36
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
#>  - attr(*, "fltLst")= chr "Filter 1"
#>  - attr(*, "removedRows")= list()
#>  - attr(*, "addedRows")= list()
#>  - attr(*, "transforms")=List of 1
#>   ..$ :List of 7
#>   .. ..$ name          : chr "Transform 1"
#>   .. ..$ id            : int 1
#>   .. ..$ suffix        : chr ""
#>   .. ..$ formula       :List of 1
#>   .. .. ..$ : chr "MATCH($source, 'VC', 'OJ')"
#>   .. ..$ formulaMessage:List of 1
#>   .. .. ..$ : chr ""
#>   .. ..$ measureType   : chr "None"
#>   .. ..$ description   : chr ""
# shows the data frame including all jamovi attributes:
# 'data.frame':  60 obs. of  16 variables:
#  $ Filter 1          : logi  TRUE TRUE TRUE TRUE TRUE TRUE ...
#   ..- attr(*, "name")= chr "Filter 1"
#   ..- attr(*, "id")= int 18
#   ..- attr(*, "columnType")= chr "Filter"
#   ..- attr(*, "dataType")= chr "Integer"
#   ..- attr(*, "measureType")= chr "Nominal"
#   ..- attr(*, "formula")= chr "logLen < 1.5"
#   ..- attr(*, "formulaMessage")= chr ""
#   ..- attr(*, "parentId")= int 0
#   ..- attr(*, "width")= int 78
#   ..- attr(*, "type")= chr "integer"
#   ..- attr(*, "importName")= chr ""
#   ..- attr(*, "description")= chr ""
#   ..- attr(*, "transform")= int 0
#   ..- attr(*, "edits")= list()
#   ..- attr(*, "missingValues")= list()
#   ..- attr(*, "filterNo")= int 0
#   ..- attr(*, "active")= logi FALSE
# ... (continued)

# getSyn requires jmvcore and RProtoBuf, and is thus not run here
if (FALSE) { # \dontrun{
data <- jmvReadWrite::read_omv(fleInp = nmeInp, getSyn = TRUE)
} # }
# if the syntax couldn't be extracted, an empty list - length = 0 - is returned,
# otherwise, the commands are shown and the first analysis is run, with the output
# from the second analysis being assigned to the variable result
if (length(attr(data, "syntax")) >= 1) {
    print(attr(data, "syntax"))
    if (nzchar(system.file(package = "jmv"))) {
        # the print-function is only used to force devtools::run_examples() to show output
        eval(parse(text = paste0("result = ", attr(data, "syntax")[1])))
        # without assigning the output to a variable, the command would be:
        # eval(parse(text = attr(data, "syntax")[1]))
        print(names(result))
        print(result$main)
        # -> "main"      "assump"    "contrasts" "postHoc"   "emm"       "residsOV"
        # (the names of the six output tables)
    }
}
```
