test_that("merge_cols_omv works", {
    nmeOut <- tempfile(fileext = "_W.omv")
    nmeInp <- vector(mode = "character", length = 3)
    dtaTmp <- rmvAtt(jmvReadWrite::bfi_sample2)
    varTmp <- names(dtaTmp)[-1]
    for (i in seq_along(nmeInp)) {
        nmeInp[i] <- tempfile(fileext = ".rds")
        names(dtaTmp)[-1] <- paste0(varTmp, "_", i)
        saveRDS(dtaTmp[sample(seq_len(dim(dtaTmp)[1]), size = round(dim(dtaTmp)[1] * (0.97 + 0.01 * i))), ], nmeInp[i])
    }

    expect_null(merge_cols_omv(dtaInp = nmeInp, fleOut = nmeOut, typMrg = "outer", varBy = "ID", varSrt = c("gender_3", "age_3")))
    expect_true(file.exists(nmeOut))
    expect_gt(file.info(nmeOut)$size, 1)
    expect_true(chkFle(nmeOut, isZIP = TRUE))
    expect_true(chkFle(nmeOut, fleCnt = "meta"))
    expect_true(chkFle(nmeOut, fleCnt = "metadata.json"))
    expect_true(chkFle(nmeOut, fleCnt = "data.bin"))
    unlink(nmeOut)

    dtaFrm <- merge_cols_omv(dtaInp = nmeInp, typMrg = "outer", varBy = "ID", varSrt = c("gender_3", "age_3"))
    expect_s3_class(dtaFrm, "data.frame")
    expect_equal(dim(dtaFrm), c(250, 85))
    expect_equal(vapply(dtaFrm, typeof, character(1), USE.NAMES = FALSE), c("character", rep(c(rep("integer", 27), "character"), 3)))
    expect_equal(names(dtaFrm), c("ID", paste0(paste0(varTmp, "_"), sort(rep(1:3, length(varTmp))))))
    expect_equal(unname(colSums(is.na(dtaFrm[, paste0("age_", 1:3)]))), c(5, 2, 0))
    expect_equal(as.integer(table(dtaFrm[["gender_3"]])), c(172, 78))
    expect_equal(which(diff(as.integer(dtaFrm[["gender_3"]])) == 1), 172)
    expect_equal(length(which(diff(as.integer(dtaFrm[["age_3"]])) == 1)), 52)
    expect_equal(max(diff(which(diff(as.integer(dtaFrm[["age_3"]])) == 1))), 12)

    dtaFrm <- merge_cols_omv(dtaInp = nmeInp[-2], typMrg = "inner", varBy = "ID", varSrt = c("gender_3", "age_3"))
    expect_s3_class(dtaFrm, "data.frame")
    expect_equal(dim(dtaFrm), c(245, 57))
    unlink(nmeInp)

    nmeInp <- vector(mode = "character", length = 3)
    dtaTmp <- jmvReadWrite::bfi_sample2
    for (i in seq_along(nmeInp)) {
        nmeInp[i] <- tempfile(fileext = ".rds")
        saveRDS(dtaTmp, nmeInp[i])
    }
    dtaFrm <- merge_cols_omv(dtaInp = nmeInp, typMrg = "outer", varBy = "ID", varSrt = c("ID"))
    expect_s3_class(dtaFrm, "data.frame")
    expect_equal(dim(dtaFrm), c(250, 29))
    expect_true(all(dtaFrm == dtaTmp[order(dtaTmp[, "ID"]), ]))
    unlink(nmeInp)

    nmeInp <- vector(mode = "character", length = 5)
    dtaTmp <- jmvReadWrite::bfi_sample2
    for (i in seq_along(nmeInp)) {
        nmeInp[i] <- tempfile(fileext = ".rds")
        strAtt <- attributes(dtaTmp[, "age"])
        dtaTmp[, "age"] <- dtaTmp[sample(nrow(dtaTmp)), "age"]
        attributes(dtaTmp[, "age"]) <- strAtt
        saveRDS(dtaTmp, nmeInp[i])
    }
    dtaFrm <- merge_cols_omv(dtaInp = nmeInp, typMrg = "outer", varBy = "ID", varSrt = c("ID"))
    dplClm <- gsub("age_1", "age", paste0("age_", seq(5)))
    expect_s3_class(dtaFrm, "data.frame")
    expect_equal(dim(dtaFrm), c(250, 33))
    expect_true(all(dplClm %in% names(dtaFrm)))
    expect_true(all(apply(vapply(dtaFrm[, dplClm], sort, numeric(dim(dtaFrm)[1])), 1, diff) == 0))
    expect_true(all(diff(colMeans(dtaFrm[, dplClm])) == 0))
    expect_true(all(unlist(lapply(dtaFrm[, dplClm], attributes)) == "Age of the respondent (years)"))

    # test cases for code coverage ============================================================================================================================
    expect_error(merge_cols_omv(fleInp = nmeInp, typMrg = "outer", varBy = "ID"), regexp = "Please use the argument dtaInp instead of fleInp\\.")
    expect_warning(merge_cols_omv(dtaInp = nmeInp, typMrg = "outer", varBy = "ID", psvAnl = TRUE),
      regexp = "psvAnl is only possible if fleOut is a file name \\(analyses are not stored in data frames, only in the jamovi files\\)\\.")
    attr(dtaTmp, "fleInp") <- nmeInp
    expect_warning(merge_cols_omv(dtaInp = dtaTmp, fleOut = nmeOut, typMrg = "outer", varBy = "ID", psvAnl = TRUE),
      regexp = "psvAnl is only possible if dtaInp is a file name \\(analyses are not stored in data frames, only in the jamovi files\\)\\.")
    unlink(nmeOut)
    unlink(nmeInp)

    dtaFrm <- list(data.frame(ID = sample(10), A = runif(10)), data.frame(ID = sample(10), B = runif(10)), data.frame(ID = sample(10), C = runif(10)), data.frame(ID = sample(10), D = runif(10)))
    expect_equal(chkByV(list(), dtaFrm), rep(list("ID"), 4))
    expect_equal(chkByV(rep(list("ID"), 4), dtaFrm), rep(list("ID"), 4))
    expect_error(chkByV(rep(list("ID2"), 4), dtaFrm),
      regexp = "^Not all data sets given in dtaInp contain the variable\\(s\\) / column\\(s\\) that shall be used for matching\\.")
    expect_equal(chkByV("ID", dtaFrm), rep(list("ID"), 4))
    expect_error(chkByV("ID2", dtaFrm), regexp = "^Not all data sets given in dtaInp contain the variable\\(s\\) / column\\(s\\) that shall be used for matching\\.")
    expect_error(chkByV(rep(list("ID"), 3), dtaFrm),
      regexp = "^varBy must be either a list \\(with the same length as dtaInp\\), a character vector, or a string\\.")
    expect_error(chkByV(rep(list("ID"), 5), c(dtaFrm, list(data.frame(ID = c(sample(9), NA), E = runif(10))))),
      regexp = "^Values in the ID variable can't be empty \\(empty values found in data set 4 to be merged\\)\\.")
    expect_error(chkByV(rep(list("ID"), 5), c(list(data.frame(ID = c(sample(9), NA), Z = runif(10))), dtaFrm)),
      regexp = "^Values in the ID variable can't be empty \\(empty values found in the original data set\\)\\.")


    nmeInp <- tempfile(fileext = ".rds")
    saveRDS(data.frame(ID = seq(60), A = rnorm(60), B = rnorm(60)), nmeInp)
    expect_null(merge_cols_omv(c(file.path("..", "ToothGrowth.omv"), nmeInp), fleOut = nmeOut, typMrg = "outer", varBy = "ID", psvAnl = TRUE))
    expect_true(chkFle(nmeOut))
    expect_gt(file.info(nmeOut)$size, 1)
    expect_true(chkFle(nmeOut, isZIP = TRUE))
    expect_true(chkFle(nmeOut, fleCnt = "meta"))
    expect_true(chkFle(nmeOut, fleCnt = "metadata.json"))
    expect_true(chkFle(nmeOut, fleCnt = "data.bin"))
    df4Chk <- read_omv(nmeOut, sveAtt = FALSE, getSyn = TRUE)
    expect_s3_class(df4Chk, "data.frame")
    expect_equal(dim(df4Chk), c(60, 19))
    expect_equal(names(df4Chk),
      c("ID", "Filter 1", "logLen", "supp - Transform 1", "len", "supp", "dose", "dose2", "dose3", "Trial", "Residuals", "J", "K", "L", "M", "O", "weights", "A", "B"))
    expect_equal(vapply(df4Chk, typeof, character(1), USE.NAMES = FALSE),
      c("integer", "logical", "double", "integer", "double", "integer", "double", "integer", "integer", "integer",
        "double", "double", "double", "integer", "logical", "logical", "integer", "double", "double"))
    expect_equal(sort(zip::zip_list(nmeOut)$filename),
      c("01 empty/analysis", "02 anova/analysis", "02 anova/resources/65167cb3bdaf8761.png", "02 anova/resources/99f9b5d34a92049b.png", "03 empty/analysis",
        "04 ancova/analysis", "05 empty/analysis", "data.bin", "index.html", "meta", "metadata.json", "xdata.json"))
    expect_equal(attr(df4Chk, "syntax"),
      c(paste("jmv::ANOVA(formula = len ~ supp + dose2 + supp:dose2, data = data, effectSize = \"partEta\", modelTest = TRUE, qq = TRUE,",
              "contrasts = list(list(var=\"supp\", type=\"none\"), list(var=\"dose2\", type=\"polynomial\")), postHoc = ~ supp + dose2, emMeans = ~ dose2:supp)"),
           "jmv::ancova(formula = len ~ supp + dose, data = data, effectSize = \"partEta\", modelTest = TRUE)"))
    unlink(nmeOut)
    unlink(nmeInp)
})
