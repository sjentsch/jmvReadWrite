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
    expect_identical(dim(df4Chk), c(200L, 5L))
    expect_identical(vapply(df4Chk, typeof, character(1), USE.NAMES = FALSE),
                     c("integer", "double", "integer", "integer", "integer"))
    expect_identical(as.integer(table(df4Chk[["Image"]])), c(3L, 1L, 1L, 4L, 17L, 44L, 73L, 44L, 12L, 1L))
    expect_identical(which(diff(as.integer(df4Chk[["Image"]])) == 1), c(3L, 4L, 5L, 9L, 26L, 70L, 143L, 187L, 199L))
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
    expect_identical(dim(df4Chk), c(200L, 5L))
    expect_identical(vapply(df4Chk, typeof, character(1), USE.NAMES = FALSE),
                     c("integer", "double", "integer", "integer", "integer"))
    expect_false(is.unsorted(df4Chk[["Image"]]))
    unlink(nmeOut)

    df4Chk <- sort_omv(dtaInp = jmvReadWrite::AlbumSales, varSrt = "Image")
    expect_s3_class(df4Chk, "data.frame")
    expect_identical(dim(df4Chk), c(200L, 5L))
    expect_identical(vapply(df4Chk, typeof, character(1), USE.NAMES = FALSE),
                     c("integer", "double", "integer", "integer", "integer"))
    expect_false(is.unsorted(df4Chk[["Image"]]))

    # test cases for code coverage and for the transfer of analyses ===================================================
    expect_error(sort_omv(fleInp = jmvReadWrite::AlbumSales, varSrt = "Image"),
                 "Please use the argument dtaInp instead of fleInp\\.")
    expect_error(sort_omv(dtaInp = nmeInp, fleOut = nmeOut, varSrt = NULL),
                 "^Calling sort_omv requires giving at least one variable to sort after\\.")
    sort_omv(dtaInp = file.path("..", "ToothGrowth.omv"), fleOut = nmeOut, varSrt = "len", psvAnl = TRUE)
    expect_true(chkFle(nmeOut))
    expect_gt(file.info(nmeOut)$size, 1)
    expect_true(chkFle(nmeOut, isZIP = TRUE))
    expect_true(chkFle(nmeOut, fleCnt = "meta"))
    expect_true(chkFle(nmeOut, fleCnt = "metadata.json"))
    expect_true(chkFle(nmeOut, fleCnt = "data.bin"))
    df4Chk <- read_omv(nmeOut, sveAtt = FALSE, getSyn = TRUE)
    expect_s3_class(df4Chk, "data.frame")
    expect_identical(dim(df4Chk), c(60L, 16L))
    expect_named(df4Chk,
                 c("Filter 1", "ID", "logLen", "supp - Transform 1", "len", "supp", "dose", "dose2", "dose3", "Trial",
                   "Residuals", "J", "K", "L", "M", "weights"))
    expect_identical(vapply(df4Chk, typeof, character(1), USE.NAMES = FALSE),
                     c("logical", "character", "double", "integer", "double", "integer", "double", "integer", "integer",
                       "integer", "double", "double", "double", "integer", "logical", "integer"))
    expect_identical(sort(zip::zip_list(nmeOut)$filename),
                     c("01 empty/analysis", "02 anova/analysis", "02 anova/resources/65167cb3bdaf8761.png",
                       "02 anova/resources/99f9b5d34a92049b.png", "03 empty/analysis", "04 ancova/analysis",
                       "05 empty/analysis", "data.bin", "index.html", "meta", "metadata.json", "strings.bin", "xdata.json"))
    expect_identical(attr(df4Chk, "syntax"),
                     c(paste("jmv::ANOVA(formula = len ~ supp + dose2 + supp:dose2, data = data, effectSize =",
                             "\"partEta\", modelTest = TRUE, qq = TRUE, contrasts = list(list(var=\"supp\",",
                             "type=\"none\"), list(var=\"dose2\", type=\"polynomial\")), postHoc = ~ supp + dose2,",
                             "emMeans = ~ dose2:supp)"),
                       "jmv::ancova(formula = len ~ supp + dose, data = data, effectSize = \"partEta\", modelTest = TRUE)"))
    unlink(nmeOut)

    expect_warning(sort_omv(dtaInp = jmvReadWrite::AlbumSales, fleOut = nmeOut, varSrt = "Sales", psvAnl = TRUE),
                   "^psvAnl is only possible if dtaInp is a file name \\(analyses are not stored in data frames, only in the jamovi files\\)\\.")
    unlink(nmeOut)
    expect_warning(sort_omv(dtaInp = jmvReadWrite::AlbumSales, varSrt = "Sales", psvAnl = TRUE),
                   "^psvAnl is only possible if fleOut is a file name \\(analyses are not stored in data frames, only in the jamovi files\\)\\.")
    unlink(nmeOut)
})
