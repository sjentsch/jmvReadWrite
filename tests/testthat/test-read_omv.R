test_that("read_omv works", {
    nmeInp <- file.path("..", "ToothGrowth.omv")

    # read the data set (without default arguments) and test its properties: data frame, size, correct column type, attributes (data frame and fourth column),
    # whether the "missingValues"-attribute is an empty list, and whether the factor levels at columns 4, 6, and 8 have the correct length and content
    dtaFrm <- read_omv(fleInp = nmeInp, useFlt = FALSE, rmMsVl = FALSE, sveAtt = FALSE, getSyn = FALSE, getHTM = FALSE)
    expect_s3_class(dtaFrm, "data.frame")
    expect_equal(dim(dtaFrm), c(60, 13))
    expect_equal(as.vector(sapply(dtaFrm, typeof)), c("logical", "character", "double", "integer", "double", "integer", "double", "integer", "integer", "double", "double", "double", "integer"))
    expect_equal(names(attributes(dtaFrm)), c("names", "row.names", "class", "fltLst"))
    expect_equal(names(attributes(dtaFrm[[2]])), c("jmv-id", "missingValues"))
    expect_equal(names(attributes(dtaFrm[[4]])), c("levels", "class", "missingValues"))
    expect_equal(names(attributes(dtaFrm[[9]])), c("levels", "class", "jmv-desc", "missingValues"))
    expect_vector(attr(dtaFrm[[4]], "missingValues"), list(), 0)
    expect_vector(attr(dtaFrm[[4]], "levels"), c(), 2)
    expect_vector(attr(dtaFrm[[6]], "levels"), c(), 2)
    expect_vector(attr(dtaFrm[[8]], "levels"), c(), 3)
    expect_equal(attr(dtaFrm[[4]], "levels"), c("1", "2"))
    expect_equal(attr(dtaFrm[[6]], "levels"), c("OJ", "VC"))
    expect_equal(attr(dtaFrm[[8]], "levels"), c("0.5", "1.0", "2.0"))

    # read the data set (with the useFlt-argument set TRUE) and test its properties: data frame, size, correct column type (NB: two rows and the first column - "Filter 1" are removed)
    dtaFrm <- read_omv(fleInp = nmeInp, useFlt = TRUE, rmMsVl = FALSE, sveAtt = FALSE, getSyn = FALSE, getHTM = FALSE)
    expect_s3_class(dtaFrm, "data.frame")
    expect_equal(dim(dtaFrm), c(58, 12))
    expect_equal(as.vector(sapply(dtaFrm, typeof)), c("character", "double", "integer", "double", "integer", "double", "integer", "integer", "double", "double", "double", "integer"))

    # read the data set (with the rmMsVl-argument set TRUE) and test its properties: data frame, size, correct column type
    dtaFrm <- read_omv(fleInp = nmeInp, useFlt = FALSE, rmMsVl = TRUE, sveAtt = FALSE, getSyn = FALSE, getHTM = FALSE)
    expect_s3_class(dtaFrm, "data.frame")
    expect_equal(dim(dtaFrm), c(60, 13))
    expect_equal(as.vector(sapply(dtaFrm, typeof)), c("logical", "character", "double", "integer", "double", "integer", "double", "integer", "integer", "double", "double", "double", "integer"))

    # read the data set (with the sveAtt-argument set TRUE) and test its properties: data frame, size, correct column type, and several attributes of the whole data frame and columns within it
    dtaFrm <- read_omv(fleInp = nmeInp, useFlt = FALSE, rmMsVl = FALSE, sveAtt = TRUE, getSyn = FALSE, getHTM = FALSE)
    expect_s3_class(dtaFrm, "data.frame")
    expect_equal(dim(dtaFrm), c(60, 13))
    expect_equal(as.vector(sapply(dtaFrm, typeof)), c("logical", "character", "double", "integer", "double", "integer", "double", "integer", "integer", "double", "double", "double", "integer"))
    expect_equal(names(attributes(dtaFrm)), c("names", "row.names", "class", "fltLst", "removedRows", "addedRows", "transforms"))
    expect_equal(names(attributes(dtaFrm[[4]])),  c("levels", "class", "missingValues", "name", "id", "columnType", "dataType", "measureType", "formula", "formulaMessage",
                                                    "parentId", "width", "type", "importName", "description", "transform", "edits", "trimLevels"))
    expect_equal(names(attributes(dtaFrm[[10]])), c("jmv-desc", "missingValues", "name", "id", "columnType", "dataType", "measureType", "formula", "formulaMessage", "parentId", "width",
                                                    "type", "outputAnalysisId", "outputOptionName", "outputName", "outputDesiredColumnName", "outputAssignedColumnName", "importName",
                                                    "description", "transform", "edits"))

    # read the data set (with the getSyn-argument set TRUE) and test its properties: correct attributes (should contain "syntax" and "protobuf"), as well as the type, size and content of "syntax"
    # (should start with "jmv::")
    dtaFrm <- read_omv(fleInp = nmeInp, useFlt = FALSE, rmMsVl = FALSE, sveAtt = FALSE, getSyn = TRUE, getHTM = FALSE)
    expect_equal(names(attributes(dtaFrm)), c("names", "row.names", "class", "fltLst", "syntax", "protobuf"))
    # the next two command actually work in both cases: when a list with "syntax" is filled with command and if it's empty
    expect_vector(attr(dtaFrm, "syntax"), list())
    expect_true(all(grepl("^jmv::", attr(dtaFrm, "syntax"))))

    # read the data set (with the getSyn-argument set TRUE) and test its properties: correct attributes (should contain "syntax" and "protobuf"), as well as the type, size and content of "syntax"
    dtaFrm <- read_omv(fleInp = nmeInp, useFlt = FALSE, rmMsVl = FALSE, sveAtt = FALSE, getSyn = FALSE, getHTM = TRUE)
    expect_equal(names(attributes(dtaFrm)), c("names", "row.names", "class", "fltLst", "HTML"))
    expect_vector(attr(dtaFrm, "HTML"), character(), 266)
    expect_equal(attr(dtaFrm, "HTML")[1],   "<!DOCTYPE html>")
    expect_equal(attr(dtaFrm, "HTML")[2],   "<html>")
    expect_equal(attr(dtaFrm, "HTML")[266], "</html>")
    expect_equal(length(grep("<.*?>", attr(dtaFrm, "HTML"))), 75)

    # test cases for code coverage ============================================================================================================================
    # fleInp is not given or empty
    expect_error(read_omv())
    expect_error(read_omv(""))
    # the manifest must have a file name as second parameter and
    # the file has to be a valid manifest file (which is not the
    # case for "index.html" [exists, but isn't a manifest])
    expect_error(chkMnf(nmeInp, c()))
    expect_error(chkMnf(nmeInp, "index.html"))

    # .omv-file isn't a ZIP
    nmeTmp <- paste0(tempfile(), ".omv")
    writeBin("", con = nmeTmp)
    expect_error(chkFle(fleNme = nmeTmp, isZIP = TRUE))
    unlink(nmeTmp)

    # invalid manifest (wrong version number)
    nmeTmp <- paste0(tempfile(), ".omv")
    mnfHdl <- file(file.path(tempdir(), "meta"), open = "wb")
    add2ZIP(nmeTmp, mnfHdl, txtOut = gsub("jamovi-Archive-Version: 11.0", "jamovi-Archive-Version: 99.0", mnfTxt()), newFle = TRUE)
    rm(mnfHdl)
    suppressMessages(expect_warning(expect_error(read_omv(nmeTmp))))
    suppressMessages(expect_null(getHdl(fleOMV = nmeTmp, crrFle = "MANIFEST.MF")))
    unlink(nmeTmp)
})

test_that("read_all works", {
    # read_all should work as read_omv (with the sveAtt-argument set TRUE) and have the same properties: data frame, size, correct column type, and several attributes of the data frame and columns
    nmeInp <- file.path("..", "ToothGrowth.omv")
    dtaFrm <- read_all(nmeInp)
    expect_s3_class(dtaFrm, "data.frame")
    expect_equal(dim(dtaFrm), c(60, 13))
    expect_equal(as.vector(sapply(dtaFrm, typeof)), c("logical", "character", "double", "integer", "double", "integer", "double", "integer", "integer", "double", "double", "double", "integer"))
    expect_equal(names(attributes(dtaFrm)), c("names", "row.names", "class", "fltLst", "removedRows", "addedRows", "transforms"))
    expect_equal(names(attributes(dtaFrm[[4]])),  c("levels", "class", "missingValues", "name", "id", "columnType", "dataType", "measureType", "formula", "formulaMessage",
                                                    "parentId", "width", "type", "importName", "description", "transform", "edits", "trimLevels"))

    # read_all for ToothGrowth as Rdata-set has fewer columns and attributes, check data frame, size, correct column type, and several attributes of the data frame and columns
    nmeTmp <- paste0(tempfile(), ".rds")
    saveRDS(jmvReadWrite::ToothGrowth, nmeTmp)
    dtaFrm <- read_all(nmeTmp)
    expect_equal(dim(dtaFrm), c(60, 7))
    expect_equal(unname(sapply(dtaFrm, typeof)),                c("character", "integer", "integer", "double", "integer", "double", "double"))
    expect_equal(unname(sapply(sapply(dtaFrm, class), "[", 1)), c("character", "factor", "factor", "numeric", "ordered", "numeric", "numeric"))
    expect_equal(names(attributes(dtaFrm)), c("names", "row.names", "class"))
    expect_equal(names(attributes(dtaFrm[[3]])),  c("levels", "class", "description"))
    expect_equal(names(attributes(dtaFrm[[5]])),  c("levels", "class"))

    unlink(nmeTmp)

    # test cases for code coverage ============================================================================================================================
    # empty file name
    expect_error(read_all())
    expect_error(read_all(""))
    # replace strings in attributes, etc.
    if (l10n_info()$`UTF-8`) {
        expect_error(rplStr(strMod = "<c3><28>", crrAtt = "Trial"))
    }

    # more than one object when using Rdata
    D1 <- data.frame(A = runif(n = 100))
    D2 <- data.frame(B = runif(n = 100))
    nmeInp <- paste0(tempfile(), ".rda")
    save(list = ls()[grepl("D[1-2]", ls())], file = nmeInp)
    # throw error when selSet is not sepcified
    expect_error(suppressMessages(read_all(nmeInp)))
    # check whether reading works correct with using selSet
    dtaFrm <- read_all(nmeInp, selSet = "D1")
    expect_s3_class(dtaFrm, class = "data.frame")
    expect_equal(dim(dtaFrm), c(100, 1))
    unlink(nmeInp)

    # test cases for CSV / TSV
    nmeInp <- paste0(tempfile(), ".tsv")
    write.table(D1, file = nmeInp, sep = "\t")
    dtaFrm <- read_all(nmeInp)
    expect_s3_class(dtaFrm, class = "data.frame")
    expect_equal(dim(dtaFrm), c(100, 1))
    unlink(nmeInp)

    nmeInp <- paste0(tempfile(), ".csv")
    write.table(D1, file = nmeInp, sep = ",")
    dtaFrm <- read_all(nmeInp)
    expect_s3_class(dtaFrm, class = "data.frame")
    expect_equal(dim(dtaFrm), c(100, 1))
    unlink(nmeInp)

    nmeInp <- paste0(tempfile(), ".csv")
    write.table(D1, file = nmeInp, sep = ";")
    # if the separator is not given, the input column can't be converted into a number
    expect_type(read_all(nmeInp)$A, "character")
    dtaFrm <- read_all(nmeInp, sep = ";")
    expect_s3_class(dtaFrm, class = "data.frame")
    expect_equal(dim(dtaFrm), c(100, 1))
    expect_type(dtaFrm$A, "double")
    unlink(nmeInp)

    nmeInp <- paste0(tempfile(), ".csv")
    writeBin("X1,X2\n1.2,2.2\n7.1,3.2", nmeInp)
    expect_error(suppressMessages(read_all(nmeInp)))
    unlink(nmeInp)

    fleInp <- paste0(tempfile(), ".sav")
    writeBin("$FL2@(#) IBM SPSS STATISTICS 64-bit Linux 25.0.0.0              \002", con = fleInp)
    expect_error(suppressMessages(read_all(fleInp, usePkg = "haven")))
    expect_error(suppressMessages(read_all(fleInp, usePkg = "foreign")))
    unlink(fleInp)

    fleInp <- paste0(tempfile(), ".dta")
    writeBin("<stata_dta><header><release>117</release><byteorder>LSF</byteorder><K>\x8f", con = fleInp)
    expect_error(suppressMessages(read_all(fleInp, usePkg = "haven")))
    writeBin("", con = fleInp)
    expect_error(suppressMessages(read_all(fleInp, usePkg = "foreign")))
    unlink(fleInp)

    fleInp <- paste0(tempfile(), ".sas7bdat")
    writeBin("", con = fleInp)
    expect_error(suppressMessages(capture.output(read_all(fleInp, usePkg = "haven"))))
    expect_error(suppressMessages(capture.output(read_all(fleInp, usePkg = "foreign"))))
    unlink(fleInp)

    fleInp <- paste0(tempfile(), ".xpt")
    writeBin("HEADER RECORD*******LIBRARY HEADER RECORD!!!!!!!", con = fleInp)
    expect_error(suppressMessages(read_all(fleInp, usePkg = "haven")))
    expect_error(suppressMessages(read_all(fleInp, usePkg = "foreign")))
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
    expect_identical(sapply(fgnLbl(dtaTmp), attr, "jmv-desc"), attr(dtaTmp, "variable.labels"))
})
