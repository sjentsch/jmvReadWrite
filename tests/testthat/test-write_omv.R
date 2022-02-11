test_that("write_omv works", {
    # check whether writing the data is working (file existence, size, contents [.omv-files are ZIP archives and must contain files that include meta, metadata.json, data.bin])
    dtaOut <- jmvReadWrite::ToothGrowth;
    colOut <- dim(dtaOut)[1];
    nmeOut <- paste0(tempfile(), ".omv");
    dtaDbg <- write_omv(dtaFrm = dtaOut, fleOut = nmeOut, retDbg = TRUE);
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
    expect_equal(sapply(jmvReadWrite::ToothGrowth, class), sapply(dtaDbg$dtaFrm, class));

    # test cases for code coverage ============================================================================================================================
    attr(dtaDbg$dtaFrm, "label.table") <- c("A", "B", "C");
    expect_error(write_omv(NULL, nmeOut));
    expect_error(write_omv(dtaDbg$dtaFrm, ""));
    expect_error(write_omv(dtaDbg$dtaFrm, nmeOut));
    expect_error(capture.output(add2ZIP(fleZIP = nmeOut, crrHdl = NULL)));

    set.seed(1);
    dtaOut <- cbind(dtaOut, data.frame(Bool = sample(c(TRUE, FALSE), colOut, TRUE),
                                       Date = sample(seq(as.Date("1999/01/01"), as.Date("2000/01/01"), by = "day"), colOut),
                                       Time = sample(as.difftime(tim = seq(0, 3600), units = "secs"), colOut)));
    write_omv(dtaFrm = dtaOut, fleOut = nmeOut);
    dtaInp <- read_omv(fleInp = nmeOut);
    unlink(nmeOut);
    expect_equal(as.integer(table(dtaInp[["Bool"]])), c(29, 31));
    expect_equal(c(mean(dtaInp[["Date"]]), sd(dtaInp[["Date"]])), c(10787.3667, 108.7002), tolerance = 1e-4);
    expect_equal(attr(dtaInp[["Date"]], "jmv-desc"), "Date (date converted to numeric; days since 1970-01-01)");
    expect_equal(c(mean(dtaInp[["Time"]]), sd(dtaInp[["Time"]])), c(1538.367, 1041.579), tolerance = 1e-4);
    expect_equal(attr(dtaInp[["Time"]], "jmv-desc"), "Time (time converted to numeric; sec since 00:00)");
})
