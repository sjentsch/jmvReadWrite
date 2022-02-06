test_that("merge_rows_omv works", {
    nmeInp <- system.file("data", c("bfi_sample.rda", "bfi_sample2.rda", "bfi_sample.rda"),  package = "jmvReadWrite");

    nmeOut <- paste0(tempfile(), ".omv");
    merge_rows_omv(fleInp = nmeInp, fleOut = nmeOut);

    expect_true(file.exists(nmeOut));
    expect_gt(file.info(nmeOut)$size, 1);
    expect_true(chkFle(nmeOut, isZIP = TRUE));
    expect_true(chkFle(nmeOut, "meta"));
    expect_true(chkFle(nmeOut, "metadata.json"));
    expect_true(chkFle(nmeOut, "data.bin"));
    unlink(nmeOut);

    
  expect_equal(2 * 2, 4)
})
