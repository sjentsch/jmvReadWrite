test_that("wide2long_omv works", {
    # generate test data set from several R-datasets (and sort it and revmove duplicates)
    dtaTmp <- rbind(ts2w(AirPassengers), ts2w(ldeaths), ts2w(co2), ts2w(nottem), ts2w(UKDriverDeaths), ts2w(USAccDeaths));
    dtaTmp <- dtaTmp[order(dtaTmp$Year),];
    dtaTmp <- dtaTmp[!duplicated(dtaTmp$Year),];
    nmeInp <- paste0(tempfile(), ".rds");
    nmeOut <- gsub(".rds", "_L.omv", nmeInp);
    saveRDS(dtaTmp, nmeInp);

    wide2long_omv(nmeInp, nmeOut, varLst = setdiff(names(dtaTmp), "Year"), varID = "Year", varTme = "Month");
    dtaFrm <- read_omv(nmeOut);
    expect_s3_class(dtaFrm, "data.frame");
    expect_equal(dim(dtaFrm), c(828, 3));
    expect_equal(as.vector(sapply(dtaFrm, typeof)), c("integer", "integer", "double"));
    expect_equal(names(attributes(dtaFrm)), c("row.names", "names", "class", "removedRows", "addedRows", "transforms"));
    expect_equal(names(attributes(dtaFrm[[3]])), c("missingValues", "name", "id", "columnType", "dataType", "measureType", "formula", "formulaMessage", "parentId", "width",
                                                   "type", "importName", "description", "transform", "edits"));
    
    wide2long_omv(nmeInp, nmeOut, varLst = setdiff(names(dtaTmp), "Year"), varID = "Year", varTme = "Month", varSrt = c("Year", "Month"));
    dtaFrm <- read_omv(nmeOut);
    expect_s3_class(dtaFrm, "data.frame");
    expect_equal(dim(dtaFrm), c(828, 3));
    expect_false(is.unsorted(dtaFrm[["Year"]]));
    expect_true(all(dtaFrm[["Month"]] == rep(1:12, length(unique(dtaFrm[["Year"]])))));
})
