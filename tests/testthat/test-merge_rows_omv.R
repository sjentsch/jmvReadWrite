test_that("merge_rows_omv works", {
    tmpInp <- list(jmvReadWrite::bfi_sample, jmvReadWrite::bfi_sample2, jmvReadWrite::bfi_sample)
    nmeInp <- paste0(tempfile(), "_", 1:3, ".rds")
    for (i in seq_along(nmeInp)) saveRDS(tmpInp[[i]], nmeInp[i])

    # check merging rows with writing an output file and afterwards checking it (existence, size, whether it is a ZIP-file and content)
    nmeOut <- tempfile(fileext = ".omv")
    expect_null(merge_rows_omv(dtaInp = nmeInp, fleOut = nmeOut))
    expect_true(file.exists(nmeOut))
    expect_gt(file.info(nmeOut)$size, 1)
    expect_true(chkFle(nmeOut, isZIP = TRUE))
    expect_true(chkFle(nmeOut, fleCnt = "meta"))
    expect_true(chkFle(nmeOut, fleCnt = "metadata.json"))
    expect_true(chkFle(nmeOut, fleCnt = "data.bin"))
    unlink(nmeOut)

    # checking merging rows with returning the merged dataset as a variable
    # try out different setting for the arguments and their consequence for size, etc.
    dtaFrm <- merge_rows_omv(dtaInp = nmeInp)
    expect_s3_class(dtaFrm, "data.frame")
    expect_equal(dim(dtaFrm), c(758, 33))
    expect_equal(names(attributes(dtaFrm)), c("names", "row.names", "class"))
    expect_equal(names(attributes(dtaFrm[[2]])), "jmv-desc")
    expect_equal(names(attributes(dtaFrm[[30]])), c("levels", "class"))

    dtaFrm <- merge_rows_omv(dtaInp = nmeInp, typMrg = "common")
    expect_s3_class(dtaFrm, "data.frame")
    expect_equal(dim(dtaFrm), c(758, 29))

    dtaFrm <- merge_rows_omv(dtaInp = nmeInp, colInd = TRUE)
    expect_s3_class(dtaFrm, "data.frame")
    expect_equal(dim(dtaFrm), c(758, 34))
    expect_true(all(unname(table(dtaFrm$fleInd)) == sapply(tmpInp, dim)[1, ]))

    dtaFrm <- merge_rows_omv(dtaInp = nmeInp, rstRwN = TRUE)
    expect_s3_class(dtaFrm, "data.frame")
    expect_equal(dim(dtaFrm), c(758, 33))
    expect_true(all(rownames(dtaFrm) == as.character(seq_len(dim(dtaFrm)[1]))))

    dtaFrm <- merge_rows_omv(dtaInp = nmeInp, colInd = TRUE, rmvDpl = TRUE)
    expect_s3_class(dtaFrm, "data.frame")
    expect_equal(dim(dtaFrm), c(504, 34))
    expect_true(all(table(dtaFrm$fleInd) == c(bfi_sample = 254, bfi_sample2 = 250)))

    tmpDF <- tmpInp[[1]]
    attr(tmpDF, "fleInp") <- nmeInp[2]
    dtaFrm <- merge_rows_omv(dtaInp = tmpDF, colInd = TRUE)
    expect_s3_class(dtaFrm, "data.frame")
    expect_equal(dim(dtaFrm), c(504, 34))
    expect_true(all(table(dtaFrm$fleInd) == setNames(254, "input data frame") | table(dtaFrm$fleInd) == setNames(250, nmeInp[2])))

    tmpDF$age <- as.numeric(tmpDF$age)
    expect_error(merge_rows_omv(dtaInp = tmpDF, colInd = TRUE), regexp = "^Variable age has different types:\\s+input data frame: numeric")
    unlink(nmeInp)

    # test cases for code coverage ============================================================================================================================
    nmeInp <- paste0(tempfile(), "_", 1:4, ".rds")
    for (i in seq_along(nmeInp)) saveRDS(stats::setNames(data.frame(runif(n = 100)), LETTERS[i]), nmeInp[i])
    expect_error(dtaFrm <- merge_rows_omv(fleInp = nmeInp, typMrg = "common"), regexp = "^Please use the argument dtaInp instead of fleInp\\.")
    expect_error(dtaFrm <- merge_rows_omv(dtaInp = nmeInp, typMrg = "common"), regexp = "^The data sets in the files that were given as dtaInp-argument do not contain variables that are overlapping")
    unlink(nmeInp)

    expect_equal(addIdx(jmvReadWrite::bfi_sample, "bfi_sample.omv")[[1]], rep("bfi_sample", 254))
    expect_equal(addIdx(jmvReadWrite::bfi_sample, ""), jmvReadWrite::bfi_sample)
    expect_equal(addIdx(jmvReadWrite::bfi_sample), jmvReadWrite::bfi_sample)
})
