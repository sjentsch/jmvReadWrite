test_that("wide2long_omv works", {
    # generate test data set from several R-datasets (and sort it and revmove duplicates)
    set.seed(1);
    dtaTmp <- cbind(data.frame(Year = 1900:2020), as.data.frame(matrix(runif(121 * 12, 0, 100), nrow = 121, dimnames = list(1:121, paste0("X.", month.abb[1:12])))));
    for (i in 1:12) attr(dtaTmp[[i + 1]], "jmv-desc") <- paste0("Test variable (", month.abb[i], ")");
    nmeInp <- paste0(tempfile(), ".rds");
    nmeOut <- gsub(".rds", "_L.omv", nmeInp);
    saveRDS(dtaTmp, nmeInp);

    wide2long_omv(nmeInp, nmeOut, varLst = setdiff(names(dtaTmp), "Year"), varID = "Year", varTme = "Month");
    dtaFrm <- read_omv(nmeOut);
    expect_s3_class(dtaFrm, "data.frame");
    expect_equal(dim(dtaFrm), c(1452, 3));
    expect_equal(as.vector(sapply(dtaFrm, typeof)), c("integer", "integer", "double"));
    expect_equal(names(attributes(dtaFrm)), c("row.names", "names", "class", "removedRows", "addedRows", "transforms"));
    expect_equal(names(attributes(dtaFrm[[3]])), c("jmv-desc", "missingValues", "name", "id", "columnType", "dataType", "measureType", "formula",
                                                   "formulaMessage", "parentId", "width", "type", "importName", "description", "transform", "edits"));
    expect_equal(attr(dtaFrm[[3]], "jmv-desc"), "Test variable");
    expect_equal(c(mean(dtaFrm[[3]]), sd(dtaFrm[[3]])), c(49.33121, 28.93480), tolerance = 1e-4);

    wide2long_omv(nmeInp, nmeOut, varLst = setdiff(names(dtaTmp), "Year"), varID = "Year", varTme = "Month", varSrt = c("Year", "Month"));
    dtaFrm <- read_omv(nmeOut);
    expect_s3_class(dtaFrm, "data.frame");
    expect_equal(dim(dtaFrm), c(1452, 3));
    expect_false(is.unsorted(dtaFrm[["Year"]]));
    expect_true(all(dtaFrm[["Month"]] == rep(sort(month.abb), length(unique(dtaFrm[["Year"]])))));

    # test cases for code coverage ============================================================================================================================
    expect_warning(wide2long_omv(nmeInp, nmeOut, varID = "Year", varTme = "Month", varSrt = c("Year", "Month")));
    expect_error(wide2long_omv(nmeInp, nmeOut, varTme = "Month", varSrt = c("Year", "Month")));
    expect_error(suppressWarnings(wide2long_omv(nmeInp, nmeOut, varSep = "_", varID = "Year")));

    unlink(nmeInp);
    unlink(nmeOut);
})
