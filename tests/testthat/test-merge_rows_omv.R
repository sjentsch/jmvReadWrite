test_that("merge_rows_omv works", {
    nmeInp <- system.file("data", c("bfi_sample.rda", "bfi_sample2.rda", "bfi_sample.rda"),  package = "jmvReadWrite");

    # check merging rows with writing an output file and afterwards checking it (existence, size, whether it is a ZIP-file and content)
    nmeOut <- paste0(tempfile(), ".omv");
    merge_rows_omv(fleInp = nmeInp, fleOut = nmeOut);
    expect_true(file.exists(nmeOut));
    expect_gt(file.info(nmeOut)$size, 1);
    expect_true(chkFle(nmeOut, isZIP = TRUE));
    expect_true(chkFle(nmeOut, "meta"));
    expect_true(chkFle(nmeOut, "metadata.json"));
    expect_true(chkFle(nmeOut, "data.bin"));
    unlink(nmeOut);

    # checking merging rows with returning the merged dataset as a variable
    # try out different setting for the arguments and their consequence for size, etc.
    dtaFrm <- merge_rows_omv(fleInp = nmeInp);
    expect_s3_class(dtaFrm, "data.frame");
    expect_equal(dim(dtaFrm), c(758, 33));
    expect_equal(names(attributes(dtaFrm)), c("names", "row.names", "class"));
    expect_equal(names(attributes(dtaFrm[[2]])), c("levels", "class"));
    expect_equal(attributes(dtaFrm[[28]]), NULL);

    dtaFrm <- merge_rows_omv(fleInp = nmeInp, typMrg = "common");
    expect_s3_class(dtaFrm, "data.frame");
    expect_equal(dim(dtaFrm), c(758, 28));

    dtaFrm <- merge_rows_omv(fleInp = nmeInp, colInd = TRUE);
    expect_s3_class(dtaFrm, "data.frame");
    expect_equal(dim(dtaFrm), c(758, 34));
    expect_true(all(table(dtaFrm$fleInd) == c(bfi_sample = 508, bfi_sample2 = 250)));

    dtaFrm <- merge_rows_omv(fleInp = nmeInp, rstRwN = TRUE);
    expect_s3_class(dtaFrm, "data.frame");
    expect_equal(dim(dtaFrm), c(758, 33));
    expect_true(all(rownames(dtaFrm) == as.character(1:dim(dtaFrm)[1])));

    dtaFrm <- merge_rows_omv(fleInp = nmeInp, colInd = TRUE, rmvDpl = TRUE);
    expect_s3_class(dtaFrm, "data.frame");
    expect_equal(dim(dtaFrm), c(504, 34));
    expect_true(all(table(dtaFrm$fleInd) == c(bfi_sample = 254, bfi_sample2 = 250)));
})
