test_that("combine_cols_omv works", {
    dtaInp <- jmvReadWrite::bfi_sample2
    nmeOut <- tempfile(fileext = ".omv")
    set.seed(1)
    selRow <- rnorm(nrow(dtaInp)) < 0
    dtaInp[selRow, "A1_1"] <- dtaInp[selRow, "A1"]
    dtaInp[selRow, "A1"]   <- NA

    expect_null(combine_cols_omv(dtaInp = dtaInp, fleOut = nmeOut, varPrs = list(c("A1", "A1_1"))))
    expect_true(file.exists(nmeOut))
    expect_gt(file.info(nmeOut)$size, 1)
    expect_true(chkFle(nmeOut, isZIP = TRUE))
    expect_true(chkFle(nmeOut, fleCnt = "meta"))
    expect_true(chkFle(nmeOut, fleCnt = "metadata.json"))
    expect_true(chkFle(nmeOut, fleCnt = "data.bin"))
    unlink(nmeOut)

    dtaFrm <- combine_cols_omv(dtaInp, varPrs = list(list("A1", "A1_1")))
    expect_s3_class(dtaFrm, "data.frame")
    expect_identical(dim(dtaFrm), c(250L, 29L))
    expect_identical(vapply(dtaFrm, typeof, character(1), USE.NAMES = FALSE), c("character", rep("integer", 27), "character"))
    expect_named(dtaFrm, names(jmvReadWrite::bfi_sample2))
    expect_identical(dtaFrm[!selRow, "A1"], dtaInp[!selRow, "A1"])
    expect_identical(dtaFrm[, "A1"], jmvReadWrite::bfi_sample2[, "A1"])
    expect_false(anyNA(dtaFrm[, "A1"]))

    dtaInp <- jmvReadWrite::bfi_sample2
    dtaInp[selRow, "A1_1"] <- dtaInp[selRow,  "A1"] + 1
    class(dtaInp[, "A1_1"]) <- class(dtaInp[, "A1"])
    expect_false(anyNA(dtaInp[selRow, "A1_1"]))
    expect_true(all(is.na(dtaInp[!selRow, "A1_1"])))

    dtaFrm <- combine_cols_omv(dtaInp, varPrs = list(c("A1", "A1_1")), mdeCmb = "first")
    expect_s3_class(dtaFrm, "data.frame")
    expect_identical(dim(dtaFrm), c(250L, 29L))
    expect_identical(vapply(dtaFrm, typeof, character(1), USE.NAMES = FALSE), c("character", rep("integer", 27), "character"))
    expect_named(dtaFrm, names(jmvReadWrite::bfi_sample2))
    expect_identical(dtaFrm[, "A1"], dtaInp[, "A1"])
    expect_identical(dtaFrm[, "A1"], jmvReadWrite::bfi_sample2[, "A1"])
    expect_false(anyNA(dtaFrm[, "A1"]))

    dtaFrm <- combine_cols_omv(dtaInp, varPrs = list(c("A1", "A1_1")), mdeCmb = "second")
    expect_s3_class(dtaFrm, "data.frame")
    expect_identical(dim(dtaFrm), c(250L, 29L))
    expect_identical(vapply(dtaFrm, typeof, character(1), USE.NAMES = FALSE), c("character", rep("integer", 27), "character"))
    expect_named(dtaFrm, names(jmvReadWrite::bfi_sample2))
    expect_identical(dtaFrm[!selRow, "A1"], dtaInp[!selRow, "A1"])
    expect_identical(dtaFrm[selRow,  "A1"], dtaInp[selRow, "A1_1"])
    expect_identical(dtaFrm[!selRow, "A1"], jmvReadWrite::bfi_sample2[!selRow, "A1"])
    expect_identical(dtaFrm[selRow,  "A1"], jmvReadWrite::bfi_sample2[selRow,  "A1"] + 1L)
    expect_false(anyNA(dtaFrm[, "A1"]))

    # test cases for code coverage ============================================================================================================================
    expect_error(combine_cols_omv(fleInp = tempfile(fileext = ".omv"), varPrs = list(c("A1", "A1_1")), mdeCmb = "first"),
      regexp = "Please use the argument dtaInp instead of fleInp\\.")
    expect_error(combine_cols_omv(dtaInp = dtaInp, varPrs = list(c(1, 2))),
      regexp = "The parameter varPrs needs to be a list with at least one \\(valid\\) variable pair to combine\\.")
    expect_error(combine_cols_omv(dtaInp = dtaInp, varPrs = list(c("A1", "A1_2"))),
      regexp = "The parameter varPrs needs to be a list with at least one \\(valid\\) variable pair to combine\\.")
    expect_error(combine_cols_omv(dtaInp = dtaInp, varPrs = list("A1", "A1_2")),
      regexp = "The parameter varPrs needs to be a list with at least one \\(valid\\) variable pair to combine\\.")
    expect_error(combine_cols_omv(dtaInp = dtaInp, varPrs = list(c("A1", "A1_1", "A2"))),
      regexp = "The parameter varPrs needs to be a list with at least one \\(valid\\) variable pair to combine\\.")
    expect_error(combine_cols_omv(dtaInp = dtaInp, varPrs = list(c("A1", "A1_1"))),
      regexp = "Mismatching values in the variable pair A1 - A1_1\\.")
    expect_error(combine_cols_omv(dtaInp = dtaInp, varPrs = list(c("A1", "A1_1")), mdeCmb = "none"),
      regexp = "Mismatching values in the variable pair A1 - A1_1\\.")

})
