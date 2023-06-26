test_that("sort_omv works", {
    nmeInp <- paste0(tempfile(), ".rds")
    nmeOut <- paste0(tempfile(), "_S.omv")
    saveRDS(jmvReadWrite::AlbumSales, nmeInp)

    sort_omv(nmeInp, nmeOut, varSrt = "Image")
    expect_true(file.exists(nmeOut))
    expect_gt(file.info(nmeOut)$size, 1)
    expect_true(chkFle(nmeOut, isZIP = TRUE))
    expect_true(chkFle(nmeOut, fleCnt = "meta"))
    expect_true(chkFle(nmeOut, fleCnt = "metadata.json"))
    expect_true(chkFle(nmeOut, fleCnt = "data.bin"))

    dtaFrm <- read_omv(nmeOut, sveAtt = FALSE)
    expect_s3_class(dtaFrm, "data.frame")
    expect_equal(dim(dtaFrm), c(200, 5))
    expect_equal(as.vector(sapply(dtaFrm, typeof)), c("integer", "double", "integer", "integer", "integer"))
    expect_equal(as.integer(table(dtaFrm[["Image"]])), c(3, 1, 1, 4, 17, 44, 73, 44, 12, 1))
    expect_equal(which(diff(as.integer(dtaFrm[["Image"]])) == 1), c(3, 4, 5, 9, 26, 70, 143, 187, 199))
    expect_false(is.unsorted(dtaFrm[["Image"]]))

    unlink(nmeInp)
    unlink(nmeOut)

    # test cases for code coverage ============================================================================================================================
    expect_error(sort_omv(nmeInp, nmeOut, varSrt = c()))
})
