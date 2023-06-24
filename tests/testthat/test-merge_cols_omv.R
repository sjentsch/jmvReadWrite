test_that("merge_cols_omv works", {
    nmeOut <- paste0(tempfile(), "_W.omv")
    nmeInp <- vector(mode = "character", length = 3)
    dtaTmp <- rmvAtt(jmvReadWrite::bfi_sample2)
    varTmp <- names(dtaTmp)[-1]
    for (i in seq_along(nmeInp)) {
        nmeInp[i] <- gsub("_W.omv", paste0("_", i, ".rds"), nmeOut)
        names(dtaTmp)[-1] <- paste0(varTmp, "_", i)
        saveRDS(dtaTmp[sample(seq_len(dim(dtaTmp)[1]), size = round(dim(dtaTmp)[1] * (0.97 + 0.01 * i))), ], nmeInp[i])
    }

    merge_cols_omv(nmeInp, nmeOut, typMrg = "outer", varBy = "ID", varSrt = c("gender_3", "age_3"))
    expect_true(file.exists(nmeOut))
    expect_gt(file.info(nmeOut)$size, 1)
    expect_true(chkFle(nmeOut, isZIP = TRUE))
    expect_true(chkFle(nmeOut, fleCnt = "meta"))
    expect_true(chkFle(nmeOut, fleCnt = "metadata.json"))
    expect_true(chkFle(nmeOut, fleCnt = "data.bin"))
    unlink(nmeOut)

    dtaFrm <- merge_cols_omv(nmeInp, typMrg = "outer", varBy = "ID", varSrt = c("gender_3", "age_3"))
    expect_s3_class(dtaFrm, "data.frame")
    expect_equal(dim(dtaFrm), c(250, 85))
    expect_equal(as.vector(sapply(dtaFrm, typeof)), c("character", rep(c(rep("integer", 27), "character"), 3)))
    expect_equal(names(dtaFrm), c("ID", paste0(paste0(varTmp, "_"), sort(rep(1:3, length(varTmp))))))
    expect_equal(unname(colSums(is.na(dtaFrm[, paste0("age_", 1:3)]))), c(5, 2, 0))
    expect_equal(as.integer(table(dtaFrm[["gender_3"]])), c(172, 78))
    expect_equal(which(diff(as.integer(dtaFrm[["gender_3"]])) == 1), 172)
    expect_equal(length(which(diff(as.integer(dtaFrm[["age_3"]])) == 1)), 52)
    expect_equal(max(diff(which(diff(as.integer(dtaFrm[["age_3"]])) == 1))), 12)

    dtaFrm <- merge_cols_omv(nmeInp[-2], typMrg = "inner", varBy = "ID", varSrt = c("gender_3", "age_3"))
    expect_s3_class(dtaFrm, "data.frame")
    expect_equal(dim(dtaFrm), c(245, 57))

    unlink(nmeInp)

    # test cases for code coverage ============================================================================================================================
    dtaFrm <- list(data.frame(ID = runif(10), A = runif(10)), data.frame(ID = runif(10), B = runif(10)), data.frame(ID = runif(10), C = runif(10)), data.frame(ID = runif(10), D = runif(10)))
    expect_equal(chkByV(list(), dtaFrm), rep(list("ID"), 4))
    expect_equal(chkByV(rep(list("ID"), 4), dtaFrm), rep(list("ID"), 4))
    expect_error(chkByV(rep(list("ID2"), 4), dtaFrm))
    expect_equal(chkByV("ID", dtaFrm), rep(list("ID"), 4))
    expect_error(chkByV("ID2", dtaFrm))
    expect_error(chkByV(rep(list("ID"), 3), dtaFrm))
})
