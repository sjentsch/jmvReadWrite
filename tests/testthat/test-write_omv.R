test_that("write_omv works", {
    # check whether writing the data is working (file existence, size, contents [.omv-files are ZIP archives and must contain files that include meta, metadata.json, data.bin])
    nmeOut <- paste0(tempfile(), ".omv");
    dtaDbg <- write_omv(ToothGrowth, fleOut = nmeOut, retDbg = TRUE);
    expect_true(file.exists(nmeOut));
    expect_gt(file.info(nmeOut)$size, 1);
    expect_true(chkFle(nmeOut, isZIP = TRUE));
    expect_true(chkFle(nmeOut, "meta"));
    expect_true(chkFle(nmeOut, "metadata.json"));
    expect_true(chkFle(nmeOut, "data.bin"));
    unlink(nmeOut);

    # check the debugging information: name and type of the three parts that are returned, content of the metadata, whether all entries in xtdDta are labels, and
    # whether dtaFrm as a data frame with the correct sizes and attributes
    expect_equal(names(dtaDbg),                       c("mtaDta", "xtdDta", "dtaFrm"));
    expect_equal(as.character(sapply(dtaDbg, class)), c("list",   "list",   "data.frame"));
    expect_equal(names(dtaDbg$mtaDta), c("rowCount", "columnCount", "removedRows", "addedRows", "fields", "transforms"));
    expect_true(all(grepl("labels", sapply(dtaDbg$xtdDta, attributes))));
    expect_s3_class(dtaDbg$dtaFrm, "data.frame");
    expect_equal(dim(dtaDbg$dtaFrm), c(60, 7));
    expect_equal(names(attributes(dtaDbg$dtaFrm)), c("names", "row.names", "class"));
    expect_equal(names(attributes(dtaDbg$dtaFrm[[3]])), c("levels", "class", "values"));
    expect_equal(names(attributes(dtaDbg$dtaFrm[[7]])), c("jmv-desc"));
    expect_equal(attributes(dtaDbg$dtaFrm[[4]]), NULL);
    expect_equal(sapply(ToothGrowth, class), sapply(dtaDbg$dtaFrm, class));
})
