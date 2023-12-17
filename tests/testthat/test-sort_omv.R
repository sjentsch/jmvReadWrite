test_that("sort_omv works", {
    nmeInp <- tempfile(fileext = ".rds")
    nmeOut <- tempfile(fileext = "_S.omv")
    saveRDS(jmvReadWrite::AlbumSales, nmeInp)

    expect_null(sort_omv(dtaInp = nmeInp, fleOut = nmeOut, varSrt = "Image"))
    expect_true(file.exists(nmeOut))
    expect_gt(file.info(nmeOut)$size, 1)
    expect_true(chkFle(nmeOut, isZIP = TRUE))
    expect_true(chkFle(nmeOut, fleCnt = "meta"))
    expect_true(chkFle(nmeOut, fleCnt = "metadata.json"))
    expect_true(chkFle(nmeOut, fleCnt = "data.bin"))
    df4Chk <- read_omv(nmeOut, sveAtt = FALSE)
    expect_s3_class(df4Chk, "data.frame")
    expect_equal(dim(df4Chk), c(200, 5))
    expect_equal(vapply(df4Chk, typeof, character(1), USE.NAMES = FALSE), c("integer", "double", "integer", "integer", "integer"))
    expect_equal(as.integer(table(df4Chk[["Image"]])), c(3, 1, 1, 4, 17, 44, 73, 44, 12, 1))
    expect_equal(which(diff(as.integer(df4Chk[["Image"]])) == 1), c(3, 4, 5, 9, 26, 70, 143, 187, 199))
    expect_false(is.unsorted(df4Chk[["Image"]]))
    unlink(nmeInp)
    unlink(nmeOut)

    expect_null(sort_omv(dtaInp = jmvReadWrite::AlbumSales, fleOut = nmeOut, varSrt = "Image"))
    expect_true(file.exists(nmeOut))
    expect_gt(file.info(nmeOut)$size, 1)
    expect_true(chkFle(nmeOut, isZIP = TRUE))
    expect_true(chkFle(nmeOut, fleCnt = "meta"))
    expect_true(chkFle(nmeOut, fleCnt = "metadata.json"))
    expect_true(chkFle(nmeOut, fleCnt = "data.bin"))
    df4Chk <- read_omv(nmeOut, sveAtt = FALSE)
    expect_s3_class(df4Chk, "data.frame")
    expect_equal(dim(df4Chk), c(200, 5))
    expect_equal(vapply(df4Chk, typeof, character(1), USE.NAMES = FALSE), c("integer", "double", "integer", "integer", "integer"))
    expect_false(is.unsorted(df4Chk[["Image"]]))
    unlink(nmeOut)

    df4Chk <- sort_omv(dtaInp = jmvReadWrite::AlbumSales, varSrt = "Image")
    expect_s3_class(df4Chk, "data.frame")
    expect_equal(dim(df4Chk), c(200, 5))
    expect_equal(vapply(df4Chk, typeof, character(1), USE.NAMES = FALSE), c("integer", "double", "integer", "integer", "integer"))
    expect_false(is.unsorted(df4Chk[["Image"]]))

    # test cases for code coverage and for the transfer of analyses ===========================================================================================
    expect_error(sort_omv(fleInp = jmvReadWrite::AlbumSales, varSrt = "Image"), regexp = "Please use the argument dtaInp instead of fleInp\\.")

    expect_error(sort_omv(dtaInp = nmeInp, fleOut = nmeOut, varSrt = c()), regexp = "^Calling sort_omv requires giving at least one variable to sort after\\.")
    sort_omv(dtaInp = file.path("..", "ToothGrowth.omv"), fleOut = nmeOut, varSrt = "len", psvAnl = TRUE)
    expect_true(chkFle(nmeOut))
    expect_gt(file.info(nmeOut)$size, 1)
    expect_true(chkFle(nmeOut, isZIP = TRUE))
    expect_true(chkFle(nmeOut, fleCnt = "meta"))
    expect_true(chkFle(nmeOut, fleCnt = "metadata.json"))
    expect_true(chkFle(nmeOut, fleCnt = "data.bin"))
    df4Chk <- read_omv(nmeOut, sveAtt = FALSE, getSyn = TRUE)
    expect_s3_class(df4Chk, "data.frame")
    expect_equal(dim(df4Chk), c(60, 17))
    expect_equal(names(df4Chk),
      c("Filter 1", "ID", "logLen", "supp - Transform 1", "len", "supp", "dose", "dose2", "dose3", "Trial", "Residuals", "J", "K", "L", "M", "O", "weights"))
    expect_equal(vapply(df4Chk, typeof, character(1), USE.NAMES = FALSE),
      c("logical", "integer", "double", "integer", "double", "integer", "double", "integer", "integer", "integer", "double", "double", "double", "integer", "logical", "logical", "integer"))
    expect_equal(sort(zip::zip_list(nmeOut)$filename),
      c("01 empty/analysis", "02 anova/analysis", "02 anova/resources/65167cb3bdaf8761.png", "02 anova/resources/99f9b5d34a92049b.png", "03 empty/analysis",
        "04 ancova/analysis", "05 empty/analysis", "data.bin", "index.html", "meta", "metadata.json", "xdata.json"))
    expect_equal(attr(df4Chk, "syntax"),
      c(paste("jmv::ANOVA(formula = len ~ supp + dose2 + supp:dose2, data = data, effectSize = \"partEta\", modelTest = TRUE, qq = TRUE,",
              "contrasts = list(list(var=\"supp\", type=\"none\"), list(var=\"dose2\", type=\"polynomial\")), postHoc = ~ supp + dose2, emMeans = ~ dose2:supp)"),
           "jmv::ancova(formula = len ~ supp + dose, data = data, effectSize = \"partEta\", modelTest = TRUE)"))
    unlink(nmeOut)

    expect_warning(sort_omv(dtaInp = jmvReadWrite::AlbumSales, fleOut = nmeOut, varSrt = "Sales", psvAnl = TRUE),
      regexp = "^psvAnl is only possible if dtaInp is a file name \\(analyses are not stored in data frames, only in the jamovi files\\)\\.")
    unlink(nmeOut)
    expect_warning(sort_omv(dtaInp = jmvReadWrite::AlbumSales, varSrt = "Sales", psvAnl = TRUE),
      regexp = "^psvAnl is only possible if fleOut is a file name \\(analyses are not stored in data frames, only in the jamovi files\\)\\.")
    unlink(nmeOut)
})
