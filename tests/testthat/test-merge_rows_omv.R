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
    expect_identical(dim(dtaFrm), c(758L, 33L))
    expect_named(attributes(dtaFrm), c("names", "row.names", "class"))
    expect_named(attributes(dtaFrm[[2]]), "jmv-desc")
    expect_named(attributes(dtaFrm[[30]]), c("levels", "class"))

    dtaFrm <- merge_rows_omv(dtaInp = nmeInp, typMrg = "common")
    expect_s3_class(dtaFrm, "data.frame")
    expect_identical(dim(dtaFrm), c(758L, 29L))

    dtaFrm <- merge_rows_omv(dtaInp = nmeInp, colInd = TRUE)
    expect_s3_class(dtaFrm, "data.frame")
    expect_identical(dim(dtaFrm), c(758L, 34L))
    expect_identical(as.integer(table(dtaFrm$fleInd)), vapply(tmpInp, dim, integer(2))[1, ])

    dtaFrm <- merge_rows_omv(dtaInp = nmeInp, rstRwN = TRUE)
    expect_s3_class(dtaFrm, "data.frame")
    expect_identical(dim(dtaFrm), c(758L, 33L))
    expect_identical(rownames(dtaFrm), as.character(seq_len(nrow(dtaFrm))))

    dtaFrm <- merge_rows_omv(dtaInp = nmeInp, colInd = TRUE, rmvDpl = TRUE)
    expect_s3_class(dtaFrm, "data.frame")
    expect_identical(dim(dtaFrm), c(504L, 34L))
    expect_identical(as.integer(table(dtaFrm$fleInd)), c(254L, 250L))

    tmpDF <- tmpInp[[1]]
    attr(tmpDF, "fleInp") <- nmeInp[2]
    dtaFrm <- merge_rows_omv(dtaInp = tmpDF, colInd = TRUE)
    expect_s3_class(dtaFrm, "data.frame")
    expect_identical(dim(dtaFrm), c(504L, 34L))
    expect_named(table(dtaFrm$fleInd), c(gsub(".rds$", "", basename(nmeInp[2])), "input data frame"))
    expect_identical(as.vector(table(dtaFrm$fleInd)), c(250L, 254L))

    tmpDF$age <- as.numeric(tmpDF$age)
    expect_error(merge_rows_omv(dtaInp = tmpDF, colInd = TRUE), regexp = "^Variable age has different types:\\s+input data frame: numeric")
    unlink(nmeInp)

    # test cases for code coverage ====================================================================================
    nmeInp <- paste0(tempfile(), "_", 1:4, ".rds")
    for (i in seq_along(nmeInp)) saveRDS(stats::setNames(data.frame(runif(n = 100)), LETTERS[i]), nmeInp[i])
    expect_error(dtaFrm <- merge_rows_omv(fleInp = nmeInp, typMrg = "common"),
                 "^Please use the argument dtaInp instead of fleInp\\.")
    expect_error(dtaFrm <- merge_rows_omv(dtaInp = nmeInp, typMrg = "common"),
                 "^The data sets in the files that were given as dtaInp-argument do not contain variables that are overlapping")
    unlink(nmeInp)

    expect_identical(addIdx(jmvReadWrite::bfi_sample, "bfi_sample.omv")[[1]], rep("bfi_sample", 254))
    expect_identical(addIdx(jmvReadWrite::bfi_sample, ""), jmvReadWrite::bfi_sample)
    expect_identical(addIdx(jmvReadWrite::bfi_sample), jmvReadWrite::bfi_sample)
})
