test_that("wide2long_omv works", {
    # generate test data set from several R-datasets (and sort it and revmove duplicates)
    set.seed(1)
    dtaTmp <- cbind(data.frame(Year = 1900:2020), as.data.frame(matrix(runif(121 * 12, 0, 100), nrow = 121, dimnames = list(1:121, paste0("X_", month.abb[1:12])))))
    for (i in 1:12) attr(dtaTmp[[i + 1]], "jmv-desc") <- paste0("Test variable (", month.abb[i], ")")
    nmeInp <- paste0(tempfile(), ".rds")
    nmeOut <- gsub(".rds", "_L.omv", nmeInp)
    saveRDS(dtaTmp, nmeInp)

    wide2long_omv(nmeInp, nmeOut, varLst = setdiff(names(dtaTmp), "Year"), varID = "Year", varTme = "Month", varSep = "_")
    dtaFrm <- read_omv(nmeOut)
    expect_s3_class(dtaFrm, "data.frame")
    expect_equal(dim(dtaFrm), c(1452, 3))
    expect_equal(as.vector(sapply(dtaFrm, typeof)), c("integer", "integer", "double"))
    expect_equal(names(attributes(dtaFrm)), c("names", "row.names", "class", "removedRows", "addedRows", "transforms"))
    expect_equal(names(attributes(dtaFrm[[3]])), c("jmv-desc", "missingValues", "name", "id", "columnType", "dataType", "measureType", "formula",
                                                   "formulaMessage", "parentId", "width", "type", "importName", "description", "transform", "edits"))
    expect_equal(attr(dtaFrm[[3]], "jmv-desc"), "Test variable")
    expect_equal(c(mean(dtaFrm[[3]]), sd(dtaFrm[[3]])), c(49.33121, 28.93480), tolerance = 1e-4)

    wide2long_omv(nmeInp, nmeOut, varLst = setdiff(names(dtaTmp), "Year"), varID = "Year", varTme = "Month", varSrt = c("Year", "Month"))
    dtaFrm <- read_omv(nmeOut)
    expect_s3_class(dtaFrm, "data.frame")
    expect_equal(dim(dtaFrm), c(1452, 3))
    expect_false(is.unsorted(dtaFrm[["Year"]]))
    expect_true(all(dtaFrm[["Month"]] == rep(sort(month.abb), length(unique(dtaFrm[["Year"]])))))

    # test cases for code coverage ============================================================================================================================
    expect_error(capture.output(wide2long_omv(nmeInp, nmeOut, varTme = "Month", varSep = "_", varSrt = c("Year", "Month"))))
    expect_error(capture.output(wide2long_omv(nmeInp, nmeOut, varSep = "x", varID = "Year")))
#   expect_error(suppressWarnings(wide2long_omv(nmeInp, nmeOut, varSep = ".", varID = "Year")))
#   expect_warning(wide2long_omv(nmeInp, nmeOut, varID = "Year", varTme = "Month", varSrt = c("Year", "Month")))

    saveRDS(data.frame(Year = 1900:2020, X_1 = runif(121), X.2 = runif(121)), nmeInp)
    expect_error(capture.output(wide2long_omv(nmeInp, nmeOut, varSep = "_", varID = "Year")))

    saveRDS(data.frame(Year = 1900:2020, X_1 = runif(121), X_2 = runif(121), X_2_A = runif(121)), nmeInp)
    expect_error(capture.output(wide2long_omv(nmeInp, nmeOut, varSep = "_", varID = "Year")))

    dtaTmp <- data.frame(ID = as.character(1:121), A = runif(121), B = runif(121), C = runif(121))
    attributes(dtaTmp[[1]]) <- list(`jmv-id` = TRUE, measureType = "ID")
    saveRDS(dtaTmp, nmeInp)
    expect_output(wide2long_omv(nmeInp, nmeOut, varSep = "", varID = "ID"),
                  "Variable list \\(varLst\\) was generated using all variables in the data frame except those defined in varExc or varID \\(ID\\).")
    dtaFrm <- read_omv(nmeOut)
    expect_s3_class(dtaFrm, "data.frame")
    expect_equal(dim(dtaFrm), c(363, 3))
    expect_equal(names(dtaFrm), c("ID", "cond", "measure"))
    expect_equal(as.vector(sapply(dtaFrm, class)), c("factor", "factor", "numeric"))
    expect_equal(table(dtaFrm$cond), table(c(rep("A", 121), rep("B", 121), rep("C", 121))))

    unlink(nmeInp)
    unlink(nmeOut)
})
