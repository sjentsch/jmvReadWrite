test_that("label_vars_omv works", {

    lblFle <- file.path("..", "label_example.csv")
    lblDtF <- read.csv(lblFle, header = FALSE)
    nmeInp <- system.file("extdata", "bfi_sample.omv", package = "jmvReadWrite")
    dtaInp <- jmvReadWrite::read_omv(fleInp = nmeInp)[, lblDtF[, 1]]
    nmeOut <- tempfile(fileext = "_L.omv")

    # check that the labels in the original file are not set (NULL)
    expect_true(all(vapply(dtaInp[, lblDtF[, 1]], function(c) is.null(attr(c, "jmv-desc")), logical(1))))

    # varLbl: (1) use file name as parameter
    expect_null(label_vars_omv(dtaInp = nmeInp, fleOut = nmeOut, varLbl = lblFle))
    expect_true(chkFle(nmeOut))
    expect_gt(file.info(nmeOut)$size, 1)
    expect_true(chkFle(nmeOut, isZIP = TRUE))
    expect_true(chkFle(nmeOut, fleCnt = "meta"))
    expect_true(chkFle(nmeOut, fleCnt = "metadata.json"))
    expect_true(chkFle(nmeOut, fleCnt = "data.bin"))
    df4Chk <- read_omv(nmeOut, sveAtt = FALSE)
    expect_s3_class(df4Chk, "data.frame")
    expect_equal(dim(df4Chk), c(250, 33))
    expect_equal(as.character(lapply(df4Chk[, lblDtF[, 1]], attr, "jmv-desc")), lblDtF[, 2])
    unlink(nmeOut)

    # varLbl: (2) use data frame as parameter
    expect_null(label_vars_omv(dtaInp = nmeInp, fleOut = nmeOut, varLbl = lblDtF))
    expect_true(chkFle(nmeOut))
    expect_gt(file.info(nmeOut)$size, 1)
    expect_true(chkFle(nmeOut, isZIP = TRUE))
    expect_true(chkFle(nmeOut, fleCnt = "meta"))
    expect_true(chkFle(nmeOut, fleCnt = "metadata.json"))
    expect_true(chkFle(nmeOut, fleCnt = "data.bin"))
    df4Chk <- read_omv(nmeOut, sveAtt = FALSE)
    expect_s3_class(df4Chk, "data.frame")
    expect_equal(dim(df4Chk), c(250, 33))
    expect_equal(as.character(lapply(df4Chk[, lblDtF[, 1]], attr, "jmv-desc")), lblDtF[, 2])
    unlink(nmeOut)

    # varLbl: (3) use character vector (with labels) as parameter, here we need to
    # give a data frame with the same number of columns as input
    expect_null(label_vars_omv(dtaInp = dtaInp[, lblDtF[, 1]], fleOut = nmeOut, varLbl = lblDtF))
    expect_true(chkFle(nmeOut))
    expect_gt(file.info(nmeOut)$size, 1)
    expect_true(chkFle(nmeOut, isZIP = TRUE))
    expect_true(chkFle(nmeOut, fleCnt = "meta"))
    expect_true(chkFle(nmeOut, fleCnt = "metadata.json"))
    expect_true(chkFle(nmeOut, fleCnt = "data.bin"))
    df4Chk <- read_omv(nmeOut, sveAtt = FALSE)
    expect_s3_class(df4Chk, "data.frame")
    expect_equal(dim(df4Chk), c(250, 28))
    expect_equal(as.character(lapply(df4Chk, attr, "jmv-desc")), lblDtF[, 2])
    unlink(nmeOut)

    # test cases for code coverage ============================================================================================================================
    expect_error(label_vars_omv(fleInp = nmeInp, varLbl = lblDtF), regexp = "Please use the argument dtaInp instead of fleInp\\.")
    expect_error(label_vars_omv(dtaInp = nmeInp, fleOut = nmeOut),
      regexp = "^Calling label_vars_omv requires the parameter varLbl, using the correct format \\(see Details in help\\)\\.")
    expect_false(file.exists(nmeOut))
    expect_error(label_vars_omv(dtaInp = nmeInp, fleOut = nmeOut, varLbl = list()),
      regexp = "^Calling label_vars_omv requires the parameter varLbl, using the correct format \\(see Details in help\\)\\.")
    expect_false(file.exists(nmeOut))
    expect_error(label_vars_omv(dtaInp = nmeInp, fleOut = nmeOut, varLbl = lblDtF[, 2]),
      regexp = paste0("^If the parameter varLbl is a character, it eiter needs to be the name of an \\(exisiting\\) file or a vector ",
                      "with labels thathas the same length as the number of variables in the input data set \\(\\d+\\).$"))
    expect_false(file.exists(nmeOut))
    expect_error(label_vars_omv(dtaInp = nmeInp, fleOut = nmeOut, varLbl = data.frame(varNme = c("Dummy", lblDtF[-1, 1]), varLbl = lblDtF[2])),
      regexp = "^There must be exactly one column with the variable names \\(currently: 0\\)\\.\\nAll variable names in the label definition must be contained in the input data set\\.")
    expect_false(file.exists(nmeOut))
    expect_error(label_vars_omv(dtaInp = nmeInp, fleOut = nmeOut, varLbl = numeric(1)),
      regexp = "^varLbl was either not a data frame or could not be converted into one.")
    expect_false(file.exists(nmeOut))
    unlink(nmeOut)

    # test cases for the transfer of analyses =================================================================================================================
    nmeInp <- file.path("..", "ToothGrowth.omv")
    lblDtF <- data.frame(varNme = c("ID", "len", "supp", "dose", "dose2", "dose3"), varLbl = c("Participant", "Tooth length", "Type of Nutrition Supplement",
                "Dosage of the Nutrition Supplement (num.)", "Dosage of the Nutrition Supplement (ordered)", "Dosage of the Nutrition Supplement (factor)"))
    # check that the labels in the original file are not set (NULL)
    expect_true(all(vapply(read_omv(nmeInp)[, lblDtF[, 1]], function(c) is.null(attr(c, "jmv-desc")), logical(1))))

    label_vars_omv(dtaInp = nmeInp, fleOut = nmeOut, varLbl = lblDtF, psvAnl = TRUE)
    expect_true(chkFle(nmeOut))
    expect_gt(file.info(nmeOut)$size, 1)
    expect_true(chkFle(nmeOut, isZIP = TRUE))
    expect_true(chkFle(nmeOut, fleCnt = "meta"))
    expect_true(chkFle(nmeOut, fleCnt = "metadata.json"))
    expect_true(chkFle(nmeOut, fleCnt = "data.bin"))
    df4Chk <- read_omv(nmeOut, getSyn = TRUE)
    expect_s3_class(df4Chk, "data.frame")
    expect_equal(dim(df4Chk), c(60, 17))
    expect_equal(names(df4Chk), c("Filter 1", "ID", "logLen", "supp - Transform 1", "len", "supp", "dose", "dose2", "dose3", "Trial", "Residuals", "J", "K", "L", "M", "O", "weights"))
    expect_equal(as.vector(vapply(df4Chk, typeof, character(1))),
      c("logical", "integer", "double", "integer", "double", "integer", "double", "integer", "integer", "integer", "double", "double", "double", "integer", "logical", "logical", "integer"))
    expect_equal(as.character(lapply(df4Chk[, lblDtF[, 1]], attr, "jmv-desc")), lblDtF[, 2])
    expect_equal(sort(zip::zip_list(nmeOut)$filename),
      c("01 empty/analysis", "02 anova/analysis", "02 anova/resources/65167cb3bdaf8761.png", "02 anova/resources/99f9b5d34a92049b.png", "03 empty/analysis",
        "04 ancova/analysis", "05 empty/analysis", "data.bin", "index.html", "meta", "metadata.json", "xdata.json"))
    expect_equal(attr(df4Chk, "syntax"),
      c(paste("jmv::ANOVA(formula = len ~ supp + dose2 + supp:dose2, data = data, effectSize = \"partEta\", modelTest = TRUE, qq = TRUE,",
              "contrasts = list(list(var=\"supp\", type=\"none\"), list(var=\"dose2\", type=\"polynomial\")), postHoc = ~ supp + dose2, emMeans = ~ dose2:supp)"),
           "jmv::ancova(formula = len ~ supp + dose, data = data, effectSize = \"partEta\", modelTest = TRUE)"))
    unlink(nmeOut)
    expect_warning(label_vars_omv(dtaInp = jmvReadWrite::AlbumSales, fleOut = nmeOut, varLbl = c("Include participant?", "Sales", "Adverts", "Airplay", "Image"), psvAnl = TRUE),
      regexp = "^psvAnl is only possible if dtaInp is a file name \\(analyses are not stored in data frames, only in the jamovi files\\)\\.")
    expect_warning(label_vars_omv(dtaInp = jmvReadWrite::AlbumSales,                  varLbl = c("Include participant?", "Sales", "Adverts", "Airplay", "Image"), psvAnl = TRUE),
      regexp = "^psvAnl is only possible if fleOut is a file name \\(analyses are not stored in data frames, only in the jamovi files\\)\\.")
    unlink(nmeOut)
    # do not unlink nmeInp, this isn't a generated file, but a link
})
