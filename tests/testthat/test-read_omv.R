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
    expect_error(read_omv("Trial.rds"),
      regexp = "^read_omv only reads jamovi files \\(\\.omv\\ / \\.omt\\), use convert_to_omv first, if you want to read other files types\\.")
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

    # convert vector of 0 and 1 into logical
    tmpCol <- sample(c(0, 1), 100, replace = TRUE)
    cl4Chk <- valLbl(crrCol = tmpCol, mtaCol = list(name = "A", columnType = "Data", dataType = "Integer"),
                     xtdDta = list(A = list(labels = list(list(0, "0", "0", FALSE), list(1, "1", "1", FALSE)))))
    expect_true(is.logical(cl4Chk))
    expect_null(attributes(cl4Chk))
    expect_equal(c(mean(cl4Chk), sd(cl4Chk)), c(mean(tmpCol), sd(tmpCol)))

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

    # more than one object when using Rdata
    D1 <- data.frame(A = runif(n = 100))
    D2 <- data.frame(B = runif(n = 100))
    nmeInp <- tempfile(fileext = ".rda")
    save(list = ls()[grepl("D[1-2]", ls())], file = nmeInp)
    # throw error when selSet is not sepcified
    expect_message(expect_null(read_all(nmeInp)), regexp = "^File \".*\" couldn't be read\\.\nThe error message was: The Rdata-file must include only one object\\.")
    # check whether reading works correct with using selSet
    df4Chk <- read_all(nmeInp, selSet = "D1")
    expect_s3_class(df4Chk, class = "data.frame")
    expect_equal(dim(df4Chk), c(100, 1))
    unlink(nmeInp)

    # test cases for clnTbl in RData and RDS
    fleInp <- tempfile()
    if (requireNamespace("haven", quietly = TRUE)) {
        haven::write_sav(jmvReadWrite::ToothGrowth, paste0(fleInp, ".sav"))
        tmpDta <- haven::read_sav(paste0(fleInp, ".sav"))
        unlink(paste0(fleInp, ".sav"))
        save(tmpDta, file = paste0(fleInp, ".RData"))
        df4Chk <- read_all(paste0(fleInp, ".RData"))
        expect_equal(attributes(df4Chk), list(row.names = seq(60), names = c("ID", "supp", "supp2", "dose", "dose2", "len", "logLen"), class = "data.frame"))
        expect_equal(lapply(df4Chk, attributes), list(ID = NULL, supp = list(levels = c("OJ", "VC"), class = "factor"), supp2 = list(levels = c("1", "2"), class = "factor"),
                                                      dose = NULL, dose2 = list(levels = c("0.5", "1.0", "2.0"), class = "factor"), len = NULL, logLen = NULL))
        unlink(paste0(fleInp, ".RData"))
        saveRDS(tmpDta, file = paste0(fleInp, ".rds"))
        df4Chk <- read_all(paste0(fleInp, ".rds"))
        expect_equal(attributes(df4Chk), list(row.names = seq(60), names = c("ID", "supp", "supp2", "dose", "dose2", "len", "logLen"), class = "data.frame"))
        expect_equal(lapply(df4Chk, attributes), list(ID = NULL, supp = list(levels = c("OJ", "VC"), class = "factor"), supp2 = list(levels = c("1", "2"), class = "factor"),
                                                      dose = NULL, dose2 = list(levels = c("0.5", "1.0", "2.0"), class = "factor"), len = NULL, logLen = NULL))
        unlink(paste0(fleInp, ".rds"))
    }

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
    expect_message(expect_null(read_all(nmeInp)),
      regexp = "^Warnings were issued when reading the file \".*\"\\.\nThe warning was: line 3 appears to contain embedded nulls")
    unlink(nmeInp)

    fleInp <- tempfile(fileext = ".sav")
    writeBin("$FL2@(#) IBM SPSS STATISTICS 64-bit Linux 25.0.0.0              \002", con = fleInp)
    expect_message(expect_null(read_all(fleInp, usePkg = "haven")),
      regexp = "^File \".*\" couldn't be read\\.\nThe error message was: Failed to parse .*: Unable to read from file\\.")
    expect_message(expect_null(read_all(fleInp, usePkg = "foreign")), regexp = "^File \".*\" couldn't be read\\.")
    unlink(fleInp)
    if (requireNamespace("haven", quietly = TRUE)) {
        haven::write_sav(jmvReadWrite::ToothGrowth, fleInp)
        df4Chk <- read_all(fleInp, usePkg = "haven")
        expect_equal(attributes(df4Chk), list(row.names = seq(60), names = c("ID", "supp", "supp2", "dose", "dose2", "len", "logLen"), class = "data.frame"))
        expect_equal(lapply(df4Chk, attributes), list(ID = NULL, supp = list(levels = c("OJ", "VC"), class = "factor"), supp2 = list(levels = c("1", "2"), class = "factor"),
                                                      dose = NULL, dose2 = list(levels = c("0.5", "1.0", "2.0"), class = "factor"), len = NULL, logLen = NULL))
        unlink(fleInp)
    }

    fleInp <- tempfile(fileext = ".dta")
    writeBin("<stata_dta><header><release>117</release><byteorder>LSF</byteorder><K>\x8f", con = fleInp)
    suppressWarnings(expect_message(expect_null(read_all(fleInp, usePkg = "haven")),
      regexp = "File \".*\" couldn't be read\\.\nThe error message was: Failed to parse .*: Unable to read from file\\."))
    suppressWarnings(expect_message(expect_null(read_all(fleInp, usePkg = "foreign")),
      regexp = "File \".*\" couldn't be read\\.\nThe error message was: not a Stata version 5-12 .dta file"))
    unlink(fleInp)
    if (requireNamespace("haven", quietly = TRUE)) {
        haven::write_dta(jmvReadWrite::ToothGrowth, fleInp)
        df4Chk <- read_all(fleInp, usePkg = "haven")
        expect_equal(attributes(df4Chk), list(row.names = seq(60), names = c("ID", "supp", "supp2", "dose", "dose2", "len", "logLen"), class = "data.frame"))
        expect_equal(lapply(df4Chk, attributes), list(ID = NULL, supp = list(levels = c("OJ", "VC"), class = "factor"), supp2 = list(levels = c("1", "2"), class = "factor"),
                                                      dose = NULL, dose2 = list(levels = c("0.5", "1.0", "2.0"), class = "factor"), len = NULL, logLen = NULL))
        unlink(fleInp)
    }

    fleInp <- tempfile(fileext = ".sas7bdat")
    writeBin("", con = fleInp)
    invisible(capture.output(expect_message(expect_null(read_all(fleInp, usePkg = "haven")), "File \".*\" couldn't be read")))
    expect_error(read_all(fleInp, usePkg = "foreign"), "In order to read the SAS-file \".*\" the R-packages \"haven\" needs to be installed\\.")
    unlink(fleInp)
    if (requireNamespace("haven", quietly = TRUE)) {
        suppressWarnings(haven::write_sas(jmvReadWrite::ToothGrowth, fleInp))
        df4Chk <- read_all(fleInp, usePkg = "haven")
        expect_equal(attributes(df4Chk), list(class = "data.frame", row.names = seq(60), names = c("ID", "supp", "supp2", "dose", "dose2", "len", "logLen")))
        expect_equal(lapply(df4Chk, attributes), list(ID = NULL, supp = NULL, supp2 = NULL, dose = NULL, dose2 = NULL, len = NULL, logLen = NULL))
        unlink(fleInp)
    }

    fleInp <- tempfile(fileext = ".xpt")
    writeBin("HEADER RECORD*******LIBRARY HEADER RECORD!!!!!!!", con = fleInp)
    expect_message(expect_null(read_all(fleInp, usePkg = "haven")),
      regexp = "^File \".*\" couldn't be read\\.\nThe error message was: Failed to parse .*: Unable to read from file\\.")
    expect_message(expect_null(read_all(fleInp, usePkg = "foreign")),
      regexp = "^File \".*\" couldn't be read\\.\nThe error message was: file not in SAS transfer format")
    unlink(fleInp)
    if (requireNamespace("haven", quietly = TRUE)) {
        haven::write_xpt(jmvReadWrite::ToothGrowth, fleInp)
        df4Chk <- read_all(fleInp, usePkg = "haven")
        expect_equal(attributes(df4Chk), list(class = "data.frame", row.names = seq(60), names = c("ID", "supp", "supp2", "dose", "dose2", "len", "logLen")))
        expect_equal(lapply(df4Chk, attributes), list(ID = NULL, supp = NULL, supp2 = NULL, dose = NULL, dose2 = NULL, len = NULL, logLen = NULL))
        unlink(fleInp)
    }

    dtaTmp <- jmvReadWrite::ToothGrowth
    expect_null(attributes(clnTbb(dtaTmp, c("jmv-id", "jmv-desc"))[[1]]))
    expect_null(attributes(clnTbb(dtaTmp, c("jmv-id", "jmv-desc"))[[7]]))
    attr(dtaTmp[[6]], "label") <- "Trial for label conversion"
    expect_equal(attributes(clnTbb(dtaTmp, jmvLbl = TRUE)[[6]]), list(`jmv-desc` = "Trial for label conversion"))

    set.seed(2)
    dtaTmp <- structure(list(value = structure(sample(c(1, 2, 4), 200, replace = TRUE), format.sas = "LEVELS", class = c("haven_labelled", "vctrs_vctr", "double"),
                             labels = c(`Level 1` = 1, `Level 2` = 2, `Level 3` = 4))), class = c("tbl_df", "tbl", "data.frame"), row.names = c(NA, -200L))
    expect_s3_class(clnTbb(dtaTmp), "data.frame")
    expect_s3_class(clnTbb(dtaTmp)[, "value"], "factor")
    expect_equal(attributes(clnTbb(dtaTmp)), list(names = "value", row.names = seq(200), class = "data.frame"))
    expect_equal(attributes(clnTbb(dtaTmp)[, "value"]), list(levels = sprintf("Level %d", seq(3)), class = "factor", format.sas = "LEVELS"))
    expect_equal(attributes(clnTbb(dtaTmp, rmvAtt = "format.sas")[, "value"]), list(levels = sprintf("Level %d", seq(3)), class = "factor"))
    expect_equal(as.integer(table(clnTbb(dtaTmp)[, "value"])), c(71, 69, 60))
    expect_equal(names(table(clnTbb(dtaTmp)[, "value"])), sprintf("Level %d", seq(3)))

    dtaTmp <- structure(list(value = structure(sample(c(1, 2, 4), 200, replace = TRUE), format.sas = "LEVELS", class = c("haven_labelled", "vctrs_vctr", "double"),
                             labels = setNames(c(1, 2, 4), rep("", 3)))), class = c("tbl_df", "tbl", "data.frame"), row.names = c(NA, -200L))
    expect_s3_class(clnTbb(dtaTmp), "data.frame")
    expect_type(clnTbb(dtaTmp)[, "value"], "integer")
    expect_equal(attributes(clnTbb(dtaTmp)), list(names = "value", row.names = seq(200), class = "data.frame"))
    expect_equal(attributes(clnTbb(dtaTmp)[, "value"]), list(format.sas = "LEVELS"))
    expect_null(attributes(clnTbb(dtaTmp, rmvAtt = "format.sas")[, "value"]))
    expect_equal(as.integer(clnTbb(dtaTmp)[, "value"])[1:10], c(4, 4, 2, 2, 4, 2, 4, 1, 2, 4))
    expect_equal(as.integer(table(clnTbb(dtaTmp)[, "value"])), c(66, 67, 67))
    expect_equal(names(table(clnTbb(dtaTmp)[, "value"])), sprintf("%d", c(1, 2, 4)))

    dtaTmp <- structure(list(value = structure(sample(c(1, 2, 4), 200, replace = TRUE), format.sas = "LEVELS", class = c("haven_labelled", "vctrs_vctr", "double"),
                             labels = setNames(c(1, 2, 4), rep("value label", 3)))), class = c("tbl_df", "tbl", "data.frame"), row.names = c(NA, -200L))
    expect_s3_class(clnTbb(dtaTmp), "data.frame")
    expect_type(clnTbb(dtaTmp)[, "value"], "integer")
    expect_equal(attributes(clnTbb(dtaTmp)), list(names = "value", row.names = seq(200), class = "data.frame"))
    expect_equal(attributes(clnTbb(dtaTmp)[, "value"]), list(format.sas = "LEVELS"))
    expect_null(attributes(clnTbb(dtaTmp, rmvAtt = "format.sas")[, "value"]))
    expect_equal(as.integer(clnTbb(dtaTmp)[, "value"])[1:10], c(1, 1, 2, 1, 1, 4, 1, 2, 1, 1))
    expect_equal(as.integer(table(clnTbb(dtaTmp)[, "value"])), c(70, 72, 58))
    expect_equal(names(table(clnTbb(dtaTmp)[, "value"])), sprintf("%d", c(1, 2, 4)))

    dtaTmp <- structure(list(value = structure(sample(seq(4), 200, replace = TRUE), format.sas = "LEVELS", class = c("haven_labelled", "vctrs_vctr", "double"),
                             labels = setNames(seq(4), c("Smallest", "", "", "Largest")))), class = c("tbl_df", "tbl", "data.frame"), row.names = c(NA, -200L))
    expect_s3_class(clnTbb(dtaTmp), "data.frame")
    expect_s3_class(clnTbb(dtaTmp)[, "value"], "factor")
    expect_equal(attributes(clnTbb(dtaTmp)), list(names = "value", row.names = seq(200), class = "data.frame"))
    expect_equal(attributes(clnTbb(dtaTmp)[, "value"]), list(levels = c("Smallest", "2", "3", "Largest"), class = "factor", format.sas = "LEVELS"))
    expect_equal(attributes(clnTbb(dtaTmp, rmvAtt = "format.sas")[, "value"]), list(levels = c("Smallest", "2", "3", "Largest"), class = "factor"))
    expect_equal(as.integer(clnTbb(dtaTmp)[, "value"])[1:10], c(4, 2, 1, 3, 2, 1, 3, 3, 2, 2))
    expect_equal(as.integer(table(clnTbb(dtaTmp)[, "value"])), c(52, 47, 46, 55))
    expect_equal(names(table(clnTbb(dtaTmp)[, "value"])), c("Smallest", "2", "3", "Largest"))

    dtaTmp <- jmvReadWrite::AlbumSales[-1]
    attr(dtaTmp, "variable.labels") <- c(Adverts = "Advertsing budget (thousands)",
                                         Airplay = "No. of plays on radio",
                                         Image = "Band image rating (0-10)",
                                         Sales = "Album sales (thousands)")
    expect_identical(vapply(clnFgn(dtaTmp), attr, character(1), "jmv-desc"), attr(dtaTmp, "variable.labels"))
})
