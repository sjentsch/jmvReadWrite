test_that("arrange_cols_omv works", {
    nmeInp <- paste0(tempfile(), ".rds")
    nmeOut <- paste0(tempfile(), "_A.omv")
    saveRDS(jmvReadWrite::AlbumSales, nmeInp)

    expect_null(arrange_cols_omv(dtaInp = nmeInp, fleOut = nmeOut, varOrd = c("selSbj", "Sales", "Adverts", "Airplay", "Image")))
    expect_true(chkFle(nmeOut))
    expect_gt(file.info(nmeOut)$size, 1)
    expect_true(chkFle(nmeOut, isZIP = TRUE))
    expect_true(chkFle(nmeOut, fleCnt = "meta"))
    expect_true(chkFle(nmeOut, fleCnt = "metadata.json"))
    expect_true(chkFle(nmeOut, fleCnt = "data.bin"))
    df4Chk <- read_omv(nmeOut, sveAtt = FALSE)
    expect_s3_class(df4Chk, "data.frame")
    expect_equal(dim(df4Chk), c(200, 5))
    expect_equal(names(df4Chk), c("selSbj", "Sales", "Adverts", "Airplay", "Image"))
    expect_equal(as.vector(sapply(df4Chk, typeof)), c("integer", "integer", "double", "integer", "integer"))
    unlink(nmeOut)

    expect_null(arrange_cols_omv(dtaInp = nmeInp, fleOut = nmeOut, varMve = list(Sales = -3, Adverts = 2)))
    expect_true(chkFle(nmeOut))
    expect_gt(file.info(nmeOut)$size, 1)
    expect_true(chkFle(nmeOut, isZIP = TRUE))
    expect_true(chkFle(nmeOut, fleCnt = "meta"))
    expect_true(chkFle(nmeOut, fleCnt = "metadata.json"))
    expect_true(chkFle(nmeOut, fleCnt = "data.bin"))
    df4Chk <- read_omv(nmeOut, sveAtt = FALSE)
    expect_s3_class(df4Chk, "data.frame")
    expect_equal(dim(df4Chk), c(200, 5))
    expect_equal(names(df4Chk), c("selSbj", "Sales", "Airplay", "Image", "Adverts"))
    expect_equal(as.vector(sapply(df4Chk, typeof)), c("integer", "integer", "integer", "integer", "double"))
    unlink(nmeOut)

    df4Chk <- arrange_cols_omv(dtaInp = nmeInp, varMve = list(Sales = -3, Adverts = 2))
    expect_s3_class(df4Chk, "data.frame")
    expect_equal(dim(df4Chk), c(200, 5))
    expect_equal(names(df4Chk), c("selSbj", "Sales", "Airplay", "Image", "Adverts"))
    expect_equal(as.vector(sapply(df4Chk, typeof)), c("integer", "integer", "integer", "integer", "double"))

    # test cases for code coverage ============================================================================================================================
    expect_error(arrange_cols_omv(fleInp = nmeInp, varMve = list(len = -2, supp = -1)), regexp = "Please use the argument dtaInp instead of fleInp\\.")
    expect_error(arrange_cols_omv(dtaInp = nmeInp, fleOut = nmeOut),
      regexp = "^Calling arrange_cols_omv requires either the parameter varOrd \\(a character vector\\) or the parameter varMve \\(a named list\\), using the correct format")
    expect_false(file.exists(nmeOut))
    expect_error(arrange_cols_omv(dtaInp = nmeInp, fleOut = nmeOut, varOrd = list()),
      regexp = "^Calling arrange_cols_omv requires either the parameter varOrd \\(a character vector\\) or the parameter varMve \\(a named list\\), using the correct format")
    expect_false(file.exists(nmeOut))
    expect_error(arrange_cols_omv(dtaInp = nmeInp, fleOut = nmeOut, varOrd = c()),
      regexp = "^Calling arrange_cols_omv requires either the parameter varOrd \\(a character vector\\) or the parameter varMve \\(a named list\\), using the correct format")
    expect_false(file.exists(nmeOut))
    expect_error(arrange_cols_omv(dtaInp = nmeInp, fleOut = nmeOut, varOrd = c(1, 2)),
      regexp = "^Calling arrange_cols_omv requires either the parameter varOrd \\(a character vector\\) or the parameter varMve \\(a named list\\), using the correct format")
    expect_false(file.exists(nmeOut))
    expect_error(arrange_cols_omv(dtaInp = nmeInp, fleOut = nmeOut, varOrd = c("", "")),
      regexp = "^Calling arrange_cols_omv requires either the parameter varOrd \\(a character vector\\) or the parameter varMve \\(a named list\\), using the correct format")
    expect_false(file.exists(nmeOut))
    expect_error(arrange_cols_omv(dtaInp = nmeInp, fleOut = nmeOut, varMve = c()),
      regexp = "^Calling arrange_cols_omv requires either the parameter varOrd \\(a character vector\\) or the parameter varMve \\(a named list\\), using the correct format")
    expect_false(file.exists(nmeOut))
    expect_error(arrange_cols_omv(dtaInp = nmeInp, fleOut = nmeOut, varMve = list()),
      regexp = "^Calling arrange_cols_omv requires either the parameter varOrd \\(a character vector\\) or the parameter varMve \\(a named list\\), using the correct format")
    expect_false(file.exists(nmeOut))
    expect_error(arrange_cols_omv(dtaInp = nmeInp, fleOut = nmeOut, varMve = list(1, 1)),
      regexp = "^Calling arrange_cols_omv requires either the parameter varOrd \\(a character vector\\) or the parameter varMve \\(a named list\\), using the correct format")
    expect_false(file.exists(nmeOut))
    expect_error(arrange_cols_omv(dtaInp = nmeInp, fleOut = nmeOut, varMve = list(A = "1", B = "1")),
      regexp = "^Calling arrange_cols_omv requires either the parameter varOrd \\(a character vector\\) or the parameter varMve \\(a named list\\), using the correct format")
    expect_false(file.exists(nmeOut))
    expect_error(arrange_cols_omv(dtaInp = nmeInp, fleOut = nmeOut, varMve = list(A = 1, B = 1.1)),
      regexp = "^Calling arrange_cols_omv requires either the parameter varOrd \\(a character vector\\) or the parameter varMve \\(a named list\\), using the correct format")
    expect_false(file.exists(nmeOut))
    expect_error(arrange_cols_omv(dtaInp = nmeInp, fleOut = nmeOut, varMve = list(A = 1, B = 0)),
      regexp = "^Calling arrange_cols_omv requires either the parameter varOrd \\(a character vector\\) or the parameter varMve \\(a named list\\), using the correct format")
    expect_false(file.exists(nmeOut))
    expect_error(arrange_cols_omv(dtaInp = nmeInp, fleOut = nmeOut, varOrd = c("Sale", "Adverts", "Airplay", "Image")),
      regexp = "^The variable\\(s\\) .* are not contained in the current data set\\.")
    expect_false(file.exists(nmeOut))
    expect_error(arrange_cols_omv(dtaInp = nmeInp, fleOut = nmeOut, varMve = list(Sale = -3)),
      regexp = "^The variable\\(s\\) .* are not contained in the current data set\\.")
    expect_false(file.exists(nmeOut))
    expect_error(arrange_cols_omv(dtaInp = nmeInp, fleOut = nmeOut, varMve = list(Sales = -5)),
      regexp = "^The value given in varMve must be chosen so that the element isn't moved before the first or after the last column.")
    expect_false(file.exists(nmeOut))
    expect_error(arrange_cols_omv(dtaInp = nmeInp, fleOut = nmeOut, varMve = list(Sales = -3, Image = 2)),
      regexp = "^The value given in varMve must be chosen so that the element isn't moved before the first or after the last column.")
    expect_false(file.exists(nmeOut))

    expect_warning(arrange_cols_omv(dtaInp = nmeInp, fleOut = nmeOut, varOrd = c("Sales", "Adverts", "Airplay", "Image")),
      regexp = "^The following variable\\(s\\) from the original data set are not contained in varOrd: .*")
    expect_true(file.exists(nmeOut))
    unlink(nmeOut)
    expect_warning(arrange_cols_omv(dtaInp = nmeInp, fleOut = nmeOut, varOrd = c("selSbj", "Sales", "Adverts", "Airplay", "Image"), varMve = list(Sales = -3)),
      regexp = "^Both, varOrd and varMve given as input parameters. varOrd takes precedence\\.")
    expect_true(file.exists(nmeOut))
    unlink(nmeOut)
    unlink(nmeInp)

    # test cases for the transfer of analyses =================================================================================================================
    nmeInp <- file.path("..", "ToothGrowth.omv")
    arrange_cols_omv(dtaInp = nmeInp, fleOut = nmeOut, varMve = list(len = -2, supp = -1), psvAnl = TRUE)
    expect_true(chkFle(nmeOut))
    expect_gt(file.info(nmeOut)$size, 1)
    expect_true(chkFle(nmeOut, isZIP = TRUE))
    expect_true(chkFle(nmeOut, fleCnt = "meta"))
    expect_true(chkFle(nmeOut, fleCnt = "metadata.json"))
    expect_true(chkFle(nmeOut, fleCnt = "data.bin"))
    df4Chk <- read_omv(nmeOut, getSyn = TRUE)
    expect_s3_class(df4Chk, "data.frame")
    expect_equal(dim(df4Chk), c(60, 14))
    expect_equal(names(df4Chk), c("Filter 1", "ID", "len", "logLen", "supp", "supp - Transform 1", "dose", "dose2", "Trial", "Residuals", "J", "K", "L", "weights"))
    expect_equal(as.vector(sapply(df4Chk, typeof)),
      c("logical", "integer", "double", "double", "integer", "integer", "double", "integer", "integer", "double", "double", "double", "integer", "integer"))
    expect_equal(sort(zip::zip_list(nmeOut)$filename),
      c("01 empty/analysis", "02 anova/analysis", "02 anova/resources/61c33c657d5e31f1.png", "02 anova/resources/dd0ce025a00dad1b.png", "03 empty/analysis",
        "04 ancova/analysis", "05 empty/analysis", "data.bin", "index.html", "meta", "metadata.json", "xdata.json"))
    expect_equal(attr(df4Chk, "syntax"),
      list(paste("jmv::ANOVA(formula = len ~ supp + dose2 + supp:dose2, data = data, effectSize = \"partEta\", modelTest = TRUE, qq = TRUE,",
                 "contrasts = list(list(var=\"supp\", type=\"none\"), list(var=\"dose2\", type=\"polynomial\")), postHoc = ~ supp + dose2, emMeans = ~ dose2:supp)"),
           "jmv::ancova(formula = len ~ supp + dose, data = data, effectSize = \"partEta\", modelTest = TRUE)"))
    expect_warning(arrange_cols_omv(dtaInp = jmvReadWrite::AlbumSales, fleOut = nmeOut, varOrd = c("selSbj", "Sales", "Adverts", "Airplay", "Image"), psvAnl = TRUE),
      regexp = "^psvAnl is only possible if dtaInp is a file name \\(analyses are not stored in data frames, only in the jamovi files\\)\\.")
    expect_warning(arrange_cols_omv(dtaInp = jmvReadWrite::AlbumSales, varOrd = c("selSbj", "Sales", "Adverts", "Airplay", "Image"), psvAnl = TRUE),
      regexp = "^psvAnl is only possible if fleOut is a file name \\(analyses are not stored in data frames, only in the jamovi files\\)\\.")
    unlink(nmeOut)
    # do not unlink nmeInp, this isn't a generated file, but a link
})
