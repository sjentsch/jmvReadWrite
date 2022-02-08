test_that("read_omv works", {
    nmeInp <- file.path("..", "ToothGrowth.omv");

    # read the data set (without default arguments) and test its properties: data frame, size, correct column type, attributes (data frame and fourth column),
    # whether the "missingValues"-attribute is an empty list, and whether the factor levels at columns 4, 6, and 8 have the correct length and content
    dtaFrm <- read_omv(fleInp = nmeInp, useFlt = FALSE, rmMsVl = FALSE, sveAtt = FALSE, getSyn = FALSE, getHTM = FALSE);
    expect_s3_class(dtaFrm, "data.frame")
    expect_equal(dim(dtaFrm), c(60, 12))
    expect_equal(as.vector(sapply(dtaFrm, typeof)), c("logical", "integer", "double", "integer", "double", "integer", "double", "integer", "integer", "double", "double", "double"))
    expect_equal(names(attributes(dtaFrm)), c("row.names", "names", "class", "fltLst"))
    expect_equal(names(attributes(dtaFrm[[4]])), c("levels", "class", "values", "missingValues"))
    expect_equal(names(attributes(dtaFrm[[4]])), c("levels", "class", "values", "missingValues"))
    expect_vector(attr(dtaFrm[[4]], "missingValues"), list(), 0)
    expect_vector(attr(dtaFrm[[4]], "levels"), c(), 2)
    expect_vector(attr(dtaFrm[[6]], "levels"), c(), 2)
    expect_vector(attr(dtaFrm[[8]], "levels"), c(), 3)
    expect_equal(attr(dtaFrm[[4]], "levels"), c("1", "2"))
    expect_equal(attr(dtaFrm[[6]], "levels"), c("OJ", "VC"))
    expect_equal(attr(dtaFrm[[8]], "levels"), c("0.5", "1.0", "2.0"))

    # read the data set (with the useFlt-argument set TRUE) and test its properties: data frame, size, correct column type (NB: two rows and the first column - "Filter 1" are removed)
    dtaFrm <- read_omv(fleInp = nmeInp, useFlt = TRUE, rmMsVl = FALSE, sveAtt = FALSE, getSyn = FALSE, getHTM = FALSE);
    expect_s3_class(dtaFrm, "data.frame")
    expect_equal(dim(dtaFrm), c(58, 11))
    expect_equal(as.vector(sapply(dtaFrm, typeof)), c("integer", "double", "integer", "double", "integer", "double", "integer", "integer", "double", "double", "double"))

    # read the data set (with the sveAtt-argument set TRUE) and test its properties: data frame, size, correct column type, and several attributes of the whole data frame and columns within it
    dtaFrm <- read_omv(fleInp = nmeInp, useFlt = FALSE, rmMsVl = FALSE, sveAtt = TRUE, getSyn = FALSE, getHTM = FALSE);
    expect_s3_class(dtaFrm, "data.frame")
    expect_equal(dim(dtaFrm), c(60, 12))
    expect_equal(as.vector(sapply(dtaFrm, typeof)), c("logical", "integer", "double", "integer", "double", "integer", "double", "integer", "integer", "double", "double", "double"))
    expect_equal(names(attributes(dtaFrm)), c("row.names", "names", "class", "fltLst", "removedRows", "addedRows", "transforms"))
    expect_equal(names(attributes(dtaFrm[[4]])),  c("levels", "class", "values", "missingValues", "name", "id", "columnType", "dataType", "measureType", "formula", "formulaMessage",
                                                    "parentId", "width", "type", "importName", "description", "transform", "edits", "trimLevels"))
    expect_equal(names(attributes(dtaFrm[[10]])), c("jmv-desc", "missingValues", "name", "id", "columnType", "dataType", "measureType", "formula", "formulaMessage", "parentId", "width",
                                                    "type", "outputAnalysisId", "outputOptionName", "outputName", "outputDesiredColumnName", "outputAssignedColumnName", "importName",
                                                    "description", "transform", "edits"))

    # read the data set (with the getSyn-argument set TRUE) and test its properties: correct attributes (should contain "syntax" and "protobuf"), as well as the type, size and content of "syntax"
    # (should start with "jmv::")
    dtaFrm <- read_omv(fleInp = nmeInp, useFlt = FALSE, rmMsVl = FALSE, sveAtt = FALSE, getSyn = TRUE, getHTM = FALSE);
    expect_equal(names(attributes(dtaFrm)), c("row.names", "names", "class", "fltLst", "syntax", "protobuf"))
    expect_vector(attr(dtaFrm, "syntax"), list(), 2)
    expect_true(all(grepl("^jmv::", attr(dtaFrm, "syntax"))))

    # read the data set (with the getSyn-argument set TRUE) and test its properties: correct attributes (should contain "syntax" and "protobuf"), as well as the type, size and content of "syntax"
    dtaFrm <- read_omv(fleInp = nmeInp, useFlt = FALSE, rmMsVl = FALSE, sveAtt = FALSE, getSyn = FALSE, getHTM = TRUE);
    expect_equal(names(attributes(dtaFrm)), c("row.names", "names", "class", "fltLst", "HTML"))
    expect_vector(attr(dtaFrm, "HTML"), character(), 266)
    expect_equal(attr(dtaFrm, "HTML")[1],   "<!DOCTYPE html>")
    expect_equal(attr(dtaFrm, "HTML")[2],   "<html>")
    expect_equal(attr(dtaFrm, "HTML")[266], "</html>")
    expect_equal(length(grep("<.*?>", attr(dtaFrm, "HTML"))), 75);
})

test_that("read_all works", {
    # read_all should work as read_omv (with the sveAtt-argument set TRUE) and have the same properties: data frame, size, correct column type, and several attributes of the data frame and columns
    dtaFrm <- read_all(file.path("..", "ToothGrowth.omv"));
    expect_s3_class(dtaFrm, "data.frame")
    expect_equal(dim(dtaFrm), c(60, 12))
    expect_equal(as.vector(sapply(dtaFrm, typeof)), c("logical", "integer", "double", "integer", "double", "integer", "double", "integer", "integer", "double", "double", "double"))
    expect_equal(names(attributes(dtaFrm)), c("row.names", "names", "class", "fltLst", "removedRows", "addedRows", "transforms"))
    expect_equal(names(attributes(dtaFrm[[4]])),  c("levels", "class", "values", "missingValues", "name", "id", "columnType", "dataType", "measureType", "formula", "formulaMessage",
                                                    "parentId", "width", "type", "importName", "description", "transform", "edits", "trimLevels"))

    # read_all for ToothGrowth as Rdata-set has fewer columns and attributes, check data frame, size, correct column type, and several attributes of the data frame and columns
    nmeTmp <- paste0(tempfile(), ".rds");
    saveRDS(jmvReadWrite::ToothGrowth, nmeTmp);
    dtaFrm <- read_all(nmeTmp);
    expect_equal(dim(dtaFrm), c(60, 7))
    expect_equal(unname(sapply(dtaFrm, typeof)),                c("integer", "integer", "integer", "double", "integer", "double", "double"))
    expect_equal(unname(sapply(sapply(dtaFrm, class), "[", 1)), c("integer", "factor", "factor", "numeric", "ordered", "numeric", "numeric"))
    expect_equal(names(attributes(dtaFrm)), c("names", "row.names", "class"))
    expect_equal(names(attributes(dtaFrm[[3]])),  c("levels", "class", "values"))
    expect_equal(names(attributes(dtaFrm[[5]])),  c("levels", "class"))

    unlink(nmeTmp);
})
