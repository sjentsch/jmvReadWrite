test_that("read_omv works", {
    nmeInp <- file.path("..", "ToothGrowth.omv")

    # read the data set (without default arguments) and test its properties: data frame, size, correct column type, attributes (data frame and fourth column),
    # whether the "missingValues"-attribute is an empty list, and whether the factor levels at columns 4, 6, and 8 have the correct length and content
    df4Chk <- read_omv(fleInp = nmeInp, useFlt = FALSE, rmMsVl = FALSE, sveAtt = FALSE, getSyn = FALSE, getHTM = FALSE)
    expect_s3_class(df4Chk, "data.frame")
    expect_equal(dim(df4Chk), c(60, 16))
    expect_equal(vapply(df4Chk, typeof, character(1), USE.NAMES = FALSE),
      c("logical", "character", "double", "integer", "double", "integer", "double", "integer", "integer", "integer", "double", "double", "double", "integer", "logical", "integer"))
    expect_equal(names(attributes(df4Chk)), c("names", "row.names", "class", "fltLst"))
    expect_equal(names(attributes(df4Chk[[2]])),  c("jmv-id", "missingValues"))
    expect_equal(names(attributes(df4Chk[[4]])),  c("levels", "class", "values", "missingValues"))
    expect_equal(names(attributes(df4Chk[[9]])),  c("levels", "class", "values", "missingValues"))
    expect_equal(names(attributes(df4Chk[[10]])), c("levels", "class", "jmv-desc", "missingValues"))
    expect_vector(attr(df4Chk[[4]],  "missingValues"), list(), 0)
    expect_vector(attr(df4Chk[[14]], "missingValues"), list(), 2)
    expect_vector(attr(df4Chk[[4]], "levels"), c(), 2)
    expect_vector(attr(df4Chk[[6]], "levels"), c(), 2)
    expect_vector(attr(df4Chk[[8]], "levels"), c(), 3)
    expect_equal(attr(df4Chk[[4]], "levels"), c("1", "2"))
    expect_equal(attr(df4Chk[[6]], "levels"), c("OJ", "VC"))
    expect_equal(attr(df4Chk[[8]], "levels"), c("0.5", "1.0", "2.0"))
    expect_equal(attr(df4Chk[[14]], "missingValues"), list("== 99", "== 88"))

    # read the data set (with the useFlt-argument set TRUE) and test its properties: data frame, size, correct column type (NB: two rows and the first column - "Filter 1" are removed)
    df4Chk <- read_omv(fleInp = nmeInp, useFlt = TRUE, rmMsVl = FALSE, sveAtt = FALSE, getSyn = FALSE, getHTM = FALSE)
    expect_s3_class(df4Chk, "data.frame")
    expect_equal(dim(df4Chk), c(58, 15))
    expect_equal(vapply(df4Chk, typeof, character(1), USE.NAMES = FALSE),
      c("character", "double", "integer", "double", "integer", "double", "integer", "integer", "integer", "double", "double", "double", "integer", "logical", "integer"))

    # read the data set (with the rmMsVl-argument set TRUE) and test its properties: data frame, size, correct column type
    df4Chk <- read_omv(fleInp = nmeInp, useFlt = FALSE, rmMsVl = TRUE, sveAtt = FALSE, getSyn = FALSE, getHTM = FALSE)
    expect_s3_class(df4Chk, "data.frame")
    expect_equal(dim(df4Chk), c(60, 16))
    expect_equal(vapply(df4Chk, typeof, character(1), USE.NAMES = FALSE),
      c("logical", "character", "double", "integer", "double", "integer", "double", "integer", "integer", "integer", "double", "double", "double", "integer", "logical", "integer"))

    # read the data set (with the sveAtt-argument set TRUE) and test its properties: data frame, size, correct column type, and several attributes of the whole data frame and columns within it
    df4Chk <- read_omv(fleInp = nmeInp, useFlt = FALSE, rmMsVl = FALSE, sveAtt = TRUE, getSyn = FALSE, getHTM = FALSE)
    expect_s3_class(df4Chk, "data.frame")
    expect_equal(dim(df4Chk), c(60, 16))
    expect_equal(vapply(df4Chk, typeof, character(1), USE.NAMES = FALSE),
      c("logical", "character", "double", "integer", "double", "integer", "double", "integer", "integer", "integer", "double", "double", "double", "integer", "logical", "integer"))
    expect_equal(names(attributes(df4Chk)), c("names", "row.names", "class", "fltLst", "removedRows", "addedRows", "transforms"))
    expect_equal(names(attributes(df4Chk[[4]])),  c("levels", "class", "values", "name", "id", "columnType", "dataType", "measureType", "formula", "formulaMessage",
                                                    "parentId", "width", "type", "importName", "description", "transform", "edits", "missingValues", "trimLevels"))
    expect_equal(names(attributes(df4Chk[[9]])),  c("levels", "class", "values", "name", "id", "columnType", "dataType", "measureType", "formula", "formulaMessage",
                                                    "parentId", "width", "type", "importName", "description", "transform", "edits", "missingValues", "trimLevels"))
    expect_equal(names(attributes(df4Chk[[11]])), c("jmv-desc", "name", "id", "columnType", "dataType", "measureType", "formula", "formulaMessage", "parentId", "width",
                                                    "type", "outputAnalysisId", "outputOptionName", "outputName", "outputDesiredColumnName", "outputAssignedColumnName", "importName",
                                                    "description", "transform", "edits", "missingValues"))

    # read the data set (with the getSyn-argument set TRUE) and test its properties: correct attributes (should contain "syntax" and "protobuf"), as well as the type, size and content of "syntax"
    # (should start with "jmv::")
    df4Chk <- read_omv(fleInp = nmeInp, useFlt = FALSE, rmMsVl = FALSE, sveAtt = FALSE, getSyn = TRUE, getHTM = FALSE)
    expect_equal(names(attributes(df4Chk)), c("names", "row.names", "class", "fltLst", "syntax", "protobuf"))
    # the next two command actually work in both cases: when a list with "syntax" is filled with command and if it's empty
    expect_vector(attr(df4Chk, "syntax"), c(), 2)
    expect_true(all(grepl("^jmv::", attr(df4Chk, "syntax"))))
    expect_equal(attr(df4Chk, "syntax"),
      c(paste("jmv::ANOVA(formula = len ~ supp + dose2 + supp:dose2, data = data, effectSize = \"partEta\", modelTest = TRUE, qq = TRUE,",
              "contrasts = list(list(var=\"supp\", type=\"none\"), list(var=\"dose2\", type=\"polynomial\")), postHoc = ~ supp + dose2, emMeans = ~ dose2:supp)"),
           "jmv::ancova(formula = len ~ supp + dose, data = data, effectSize = \"partEta\", modelTest = TRUE)"))

    # read the data set (with the getSyn-argument set TRUE) and test its properties: correct attributes (should contain "syntax" and "protobuf"), as well as the type, size and content of "syntax"
    df4Chk <- read_omv(fleInp = nmeInp, useFlt = FALSE, rmMsVl = FALSE, sveAtt = FALSE, getSyn = FALSE, getHTM = TRUE)
    expect_equal(names(attributes(df4Chk)), c("names", "row.names", "class", "fltLst", "HTML"))
    expect_vector(attr(df4Chk, "HTML"), character(), 257)
    expect_equal(attr(df4Chk, "HTML")[1],   "<!DOCTYPE html>")
    expect_equal(attr(df4Chk, "HTML")[2],   "<html>")
    expect_equal(attr(df4Chk, "HTML")[257], "</html>")
    expect_equal(length(grep("<.*?>", attr(df4Chk, "HTML"))), 72)

    # test cases for code coverage ============================================================================================================================
    # fleInp is not given or empty
    expect_error(read_omv(),   regexp = "^File name to the input data file needs to be given as parameter \\(fleInp = \\.\\.\\.\\)\\.")
    expect_error(read_omv(""), regexp = "^File name to the input data file needs to be given as parameter \\(fleInp = \\.\\.\\.\\)\\.")
    # fleInp is not a jamovi-file (.omv)
    expect_error(read_omv("Trial.rds"), regexp = "^read_omv only reads jamovi files \\(\\.omv\\), use convert_to_omv first, if you want to read other files types\\.")
    # the manifest must have a file name as second parameter and
    # the file has to be a valid manifest file (which is not the
    # case for "index.html" [exists, but isn't a manifest])
    expect_error(chkMnf(nmeInp, c()), regexp = "^File \".*?\" has not the correct file format \\(is missing the jamovi-file-manifest\\)\\.")
    expect_error(chkMnf(nmeInp, "index.html"),
      regexp = "^The file you are trying to read \\(ToothGrowth\\.omv\\) has an improper manifest file \\(meta\\) and is likely corrupted\\.")

    # .omv-file isn't a ZIP
    nmeTmp <- tempfile(fileext = ".omv")
    writeBin("", con = nmeTmp)
    expect_error(chkFle(nmeTmp, isZIP = TRUE), regexp = "^chkFle: File \".*\" has not the correct file format \\(is not a ZIP archive\\)\\.")
    unlink(nmeTmp)

    # invalid manifest (wrong version number)
    nmeTmp <- tempfile(fileext = ".omv")
    add2ZIP(nmeTmp, crrFle = c("meta", "wb"), txtOut = gsub("jamovi-Archive-Version: 11.0", "jamovi-Archive-Version: 99.0", mnfTxt()))
    suppressMessages(expect_warning(expect_error(read_omv(nmeTmp), regexp = "^'con' is not a connection"),
      regexp = "^The file that you are trying to read \\(.*\\) was written with a version of jamovi that currently is not implemented"))
    suppressMessages(expect_null(getHdl(fleOMV = nmeTmp, crrFle = "MANIFEST.MF")))
    unlink(nmeTmp)

    # unimplemented columnType when assigning value labels
    expect_error(valLbl(mtaCol = list(name = "Trial", columnType = "Trial", dataType = "Trial"), xtdDta = list(Trial = NULL)),
      regexp = "Error when reading value label - likely the column type is not implemented \\(yet\\): Trial - Trial - Trial")
})

test_that("read_all works", {
    # read_all should work as read_omv (with the sveAtt-argument set TRUE) and have the same properties: data frame, size, correct column type, and several attributes of the data frame and columns
    nmeInp <- file.path("..", "ToothGrowth.omv")
    df4Chk <- read_all(nmeInp)
    expect_s3_class(df4Chk, "data.frame")
    expect_equal(dim(df4Chk), c(60, 16))
    expect_equal(vapply(df4Chk, typeof, character(1), USE.NAMES = FALSE),
      c("logical", "character", "double", "integer", "double", "integer", "double", "integer", "integer", "integer", "double", "double", "double", "integer", "logical", "integer"))
    expect_equal(names(attributes(df4Chk)), c("names", "row.names", "class", "fltLst", "removedRows", "addedRows", "transforms"))
    expect_equal(names(attributes(df4Chk[[4]])),  c("levels", "class", "values", "name", "id", "columnType", "dataType", "measureType", "formula", "formulaMessage",
                                                    "parentId", "width", "type", "importName", "description", "transform", "edits", "missingValues", "trimLevels"))
    expect_equal(names(attributes(df4Chk[[9]])),  c("levels", "class", "values", "name", "id", "columnType", "dataType", "measureType", "formula", "formulaMessage",
                                                    "parentId", "width", "type", "importName", "description", "transform", "edits", "missingValues", "trimLevels"))

    # read_all for ToothGrowth as Rdata-set has fewer columns and attributes, check data frame, size, correct column type, and several attributes of the data frame and columns
    nmeTmp <- tempfile(fileext = ".rds")
    saveRDS(jmvReadWrite::ToothGrowth, nmeTmp)
    df4Chk <- read_all(nmeTmp)
    expect_equal(dim(df4Chk), c(60, 7))
    expect_equal(vapply(df4Chk, typeof,                  character(1), USE.NAMES = FALSE), c("character", "integer", "integer", "double", "integer", "double", "double"))
    expect_equal(vapply(df4Chk, function(x) class(x)[1], character(1), USE.NAMES = FALSE), c("character", "factor", "factor", "numeric", "ordered", "numeric", "numeric"))
    expect_equal(names(attributes(df4Chk)), c("names", "row.names", "class"))
    expect_equal(names(attributes(df4Chk[[3]])),  c("levels", "class", "description"))
    expect_equal(names(attributes(df4Chk[[5]])),  c("levels", "class"))
    unlink(nmeTmp)

    # test cases for code coverage ============================================================================================================================
    # empty file name
    expect_error(read_all(),   regexp = "^File name to the input data file needs to be given as parameter \\(fleInp = \\.\\.\\.\\)\\.")
    expect_error(read_all(""), regexp = "^File name to the input data file needs to be given as parameter \\(fleInp = \\.\\.\\.\\)\\.")

    # replace strings in attributes, etc.
    # actual replacement
    expect_equal(rplStr(strMod = "<c4><d6><dc><df><e4><f6><fc>", crrAtt = "Trial"), "ÄÖÜßäöü")
    expect_error(rplStr(strMod = "<c3><28>", crrAtt = "Trial"),
      regexp = "^The current data set still contains an invalid character \\(\".*\"\\) in attribute: \".*\"\\.")
    # finding attributes that may contain such strings
    tmpDF <- read_omv(nmeInp)
    attr(tmpDF, "fltLst") <- "Filter <c4><d6><dc><df><e4><f6><fc>"
    attr(tmpDF[["weights"]], "description") <- "<c4><d6><dc><df><e4><f6><fc>"
    df4Chk <- rplAtt(dtaFrm = tmpDF)
    expect_equal(attr(df4Chk, "fltLst"), "Filter ÄÖÜßäöü")
    expect_equal(attr(df4Chk[["weights"]], "description"), "ÄÖÜßäöü")

    # more than one object when using Rdata
    D1 <- data.frame(A = runif(n = 100))
    D2 <- data.frame(B = runif(n = 100))
    nmeInp <- tempfile(fileext = ".rda")
    save(list = ls()[grepl("D[1-2]", ls())], file = nmeInp)
    # throw error when selSet is not sepcified
    suppressMessages(expect_error(read_all(nmeInp),
      regexp = "^Input data are either not a data frame or have incorrect \\(only one or more than two\\) dimensions\\."))
    # check whether reading works correct with using selSet
    df4Chk <- read_all(nmeInp, selSet = "D1")
    expect_s3_class(df4Chk, class = "data.frame")
    expect_equal(dim(df4Chk), c(100, 1))
    unlink(nmeInp)

    # test cases for CSV / TSV
    nmeInp <- tempfile(fileext = ".tsv")
    write.table(D1, file = nmeInp, sep = "\t")
    df4Chk <- read_all(nmeInp)
    expect_s3_class(df4Chk, class = "data.frame")
    expect_equal(dim(df4Chk), c(100, 1))
    unlink(nmeInp)

    nmeInp <- tempfile(fileext = ".csv")
    write.table(D1, file = nmeInp, sep = ",")
    df4Chk <- read_all(nmeInp)
    expect_s3_class(df4Chk, class = "data.frame")
    expect_equal(dim(df4Chk), c(100, 1))
    unlink(nmeInp)

    nmeInp <- tempfile(fileext = ".csv")
    write.table(D1, file = nmeInp, sep = ";")
    # if the separator is not given, the input column can't be converted into a number
    expect_type(read_all(nmeInp)$A, "character")
    df4Chk <- read_all(nmeInp, sep = ";")
    expect_s3_class(df4Chk, class = "data.frame")
    expect_equal(dim(df4Chk), c(100, 1))
    expect_type(df4Chk$A, "double")
    unlink(nmeInp)

    nmeInp <- tempfile(fileext = ".csv")
    writeBin("X1,X2\n1.2,2.2\n7.1,3.2", nmeInp)
    suppressMessages(expect_error(read_all(nmeInp),
      regexp = "^Input data are either not a data frame or have incorrect \\(only one or more than two\\) dimensions\\."))
    unlink(nmeInp)

    fleInp <- tempfile(fileext = ".sav")
    writeBin("$FL2@(#) IBM SPSS STATISTICS 64-bit Linux 25.0.0.0              \002", con = fleInp)
    suppressMessages(expect_error(read_all(fleInp, usePkg = "haven"),
      regexp = "^Input data are either not a data frame or have incorrect \\(only one or more than two\\) dimensions\\."))
    suppressMessages(expect_error(read_all(fleInp, usePkg = "foreign"),
      regexp = "^Input data are either not a data frame or have incorrect \\(only one or more than two\\) dimensions\\."))
    unlink(fleInp)

    fleInp <- tempfile(fileext = ".dta")
    writeBin("<stata_dta><header><release>117</release><byteorder>LSF</byteorder><K>\x8f", con = fleInp)
    suppressMessages(expect_error(read_all(fleInp, usePkg = "haven"),
      regexp = "^Input data are either not a data frame or have incorrect \\(only one or more than two\\) dimensions\\."))
    writeBin("", con = fleInp)
    suppressMessages(expect_error(read_all(fleInp, usePkg = "foreign"),
      regexp = "^Input data are either not a data frame or have incorrect \\(only one or more than two\\) dimensions\\."))
    unlink(fleInp)

    fleInp <- tempfile(fileext = ".sas7bdat")
    writeBin("", con = fleInp)
    suppressMessages(expect_error(capture_output(read_all(fleInp, usePkg = "haven")),
      regexp = "^Input data are either not a data frame or have incorrect \\(only one or more than two\\) dimensions\\."))
    suppressMessages(expect_error(read_all(fleInp, usePkg = "foreign"),
      regexp = "^Input data are either not a data frame or have incorrect \\(only one or more than two\\) dimensions\\."))
    unlink(fleInp)

    fleInp <- tempfile(fileext = ".xpt")
    writeBin("HEADER RECORD*******LIBRARY HEADER RECORD!!!!!!!", con = fleInp)
    suppressMessages(expect_error(read_all(fleInp, usePkg = "haven"),
      regexp = "^Input data are either not a data frame or have incorrect \\(only one or more than two\\) dimensions\\."))
    suppressMessages(expect_error(read_all(fleInp, usePkg = "foreign"),
      regexp = "^Input data are either not a data frame or have incorrect \\(only one or more than two\\) dimensions\\."))
    unlink(fleInp)

    dtaTmp <- jmvReadWrite::ToothGrowth
    expect_null(attributes(hvnAdj(dtaTmp, c("jmv-id", "jmv-desc"))[[1]]))
    expect_null(attributes(hvnAdj(dtaTmp, c("jmv-id", "jmv-desc"))[[7]]))
    attr(dtaTmp[[6]], "label") <- "Trial for label conversion"
    expect_equal(attributes(hvnAdj(dtaTmp, jmvLbl = TRUE)[[6]]), list(`jmv-desc` = "Trial for label conversion"))

    dtaTmp <- jmvReadWrite::AlbumSales[-1]
    attr(dtaTmp, "variable.labels") <- c(Adverts = "Advertsing budget (thousands)",
                                         Airplay = "No. of plays on radio",
                                         Image = "Band image rating (0-10)",
                                         Sales = "Album sales (thousands)")
    expect_identical(vapply(fgnLbl(dtaTmp), attr, character(1), "jmv-desc"), attr(dtaTmp, "variable.labels"))
})
