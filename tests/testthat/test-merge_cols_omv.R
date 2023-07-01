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

    nmeInp <- paste0(tempfile(), ".rds")
    saveRDS(data.frame(ID = seq(60), A = rnorm(60), B = rnorm(60)), nmeInp)
    merge_cols_omv(c("../ToothGrowth.omv", nmeInp), fleOut = nmeOut, typMrg = "outer", varBy = "ID", psvAnl = TRUE)
    expect_true(chkFle(nmeOut))
    expect_gt(file.info(nmeOut)$size, 1)
    expect_true(chkFle(nmeOut, isZIP = TRUE))
    expect_true(chkFle(nmeOut, fleCnt = "meta"))
    expect_true(chkFle(nmeOut, fleCnt = "metadata.json"))
    expect_true(chkFle(nmeOut, fleCnt = "data.bin"))
    df4Chk <- read_omv(nmeOut, sveAtt = FALSE, getSyn = TRUE)
    expect_s3_class(df4Chk, "data.frame")
    expect_equal(dim(df4Chk), c(60, 15))
    expect_equal(names(df4Chk), c("ID", "Filter 1", "logLen", "supp - Transform 1", "len", "supp", "dose", "dose2", "Trial", "Residuals", "J", "K", "L", "A", "B"))
    expect_equal(as.vector(sapply(df4Chk, typeof)),
      c("character", "logical", "double", "integer", "double", "integer", "double", "integer", "integer", "double", "double", "double", "integer", "double", "double"))
    expect_equal(zip::zip_list(nmeOut)$filename,
      c("data.bin", "strings.bin", "meta", "metadata.json", "xdata.json", "index.html", "01 empty/analysis", "02 anova/analysis", "03 empty/analysis",
        "04 ancova/analysis", "05 empty/analysis", "02 anova/resources/3b518ea3d44f095f.png", "02 anova/resources/07288f96c58ae68b.png"))
    expect_equal(attr(df4Chk, "syntax"),
      list(paste("jmv::ANOVA(formula = len ~ supp + dose2 + supp:dose2, data = data, effectSize = \"partEta\", modelTest = TRUE, qq = TRUE,",
                 "contrasts = list(list(var=\"supp\", type=\"none\"), list(var=\"dose2\", type=\"polynomial\")), postHoc = ~ supp + dose2, emMeans = ~ dose2:supp)"),
           "jmv::ancova(formula = len ~ supp + dose, data = data, effectSize = \"partEta\", modelTest = TRUE)"))
})
