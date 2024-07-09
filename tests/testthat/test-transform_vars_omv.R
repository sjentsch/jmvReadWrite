test_that("transform_vars_omv works", {

    # generate skewed variables
    set.seed(335)
    dtaInp <- data.frame(MP = rnorm(1000) * 1e-1 + rexp(1000, 2) * (1 - 1e-1), MN = rnorm(1000) * 1e-1 - rexp(1000, 2) * (1 - 1e-1),
                         SP = rnorm(1000) * 1e-2 + rexp(1000, 2) * (1 - 1e-2), SN = rnorm(1000) * 1e-2 - rexp(1000, 2) * (1 - 1e-2),
                         EP = rnorm(1000) * 1e-4 + rexp(1000, 2) * (1 - 1e-4), EN = rnorm(1000) * 1e-4 - rexp(1000, 2) * (1 - 1e-4))
    nmeOut <- tempfile(fileext = ".omv")

    # varXfm: (1) use transformations as list names, write data frame
    varXfm <- list(posSqr = c("MP"), negSqr = c("MN"), posLog = c("SP"), negLog = c("SN"), posInv = c("EP"), negInv = c("EN"))
    expect_null(transform_vars_omv(dtaInp = dtaInp, fleOut = nmeOut, varXfm = varXfm))
    expect_true(chkFle(nmeOut))
    expect_gt(file.info(nmeOut)$size, 1)
    expect_true(chkFle(nmeOut, isZIP = TRUE))
    expect_true(chkFle(nmeOut, fleCnt = "meta"))
    expect_true(chkFle(nmeOut, fleCnt = "metadata.json"))
    expect_true(chkFle(nmeOut, fleCnt = "data.bin"))
    df4Chk <- read_omv(nmeOut, sveAtt = TRUE)
    expect_s3_class(df4Chk, "data.frame")
    expect_equal(dim(df4Chk), c(1000, 12))
    expect_equal(names(df4Chk), c(names(dtaInp), paste0(names(dtaInp), "_", c(rep("SQR", 2), rep("LOG", 2), rep("INV", 2)))))
    expect_equal(vapply(df4Chk, mean, numeric(1), USE.NAMES = FALSE),
      c(0.44801838, -0.4646020, 0.47815369, -0.49234557, 0.50478086, -0.49908730, 0.78869665, 0.76679464, 0.15589357, 0.15940117, 0.72353452, 0.72355557))
    expect_equal(vapply(df4Chk, sd, numeric(1), USE.NAMES = FALSE),
      c(0.43040192, 0.47964321, 0.47895029, 0.48250463, 0.51346048, 0.50804630, 0.24512659, 0.27358674, 0.12018917, 0.11979942, 0.18327280, 0.17927202))
    expect_equal(vapply(df4Chk, min, numeric(1), USE.NAMES = FALSE),
      c(-0.234050985891947, -2.63444386411986, -0.0148647228014522, -3.47455993472914, 0.00129343898380293, -4.06637548446082, 0, 0, 0, 0, 0.20527105709926, 0.197381201985878))
    expect_equal(vapply(df4Chk, max, numeric(1), USE.NAMES = FALSE),
      c(3.06893926825301, 0.19814687853258, 3.25069026307788, 0.0123121447940763, 3.87290083522986, -0.0000368973062622809, 1.81741306646149, 1.68303022630555,
        0.6299755446769, 0.651943687997723, 1, 1))
    expect_equal(lapply(df4Chk, attr, "formula"),
      list(MP = "", MN = "", SP = "", SN = "", EP = "", EN = "", MP_SQR = "SQRT(MP - VMIN(MP))", MN_SQR = "SQRT(VMAX(MN) - MN)",
           SP_LOG = "LOG10(SP - VMIN(SP) + 1)", SN_LOG = "LOG10(VMAX(SN) + 1 - SN)", EP_INV = "1 / (EP - VMIN(EP) + 1)", EN_INV = "1 / (VMAX(EN) + 1 - EN)"))
    unlink(nmeOut)

    # varXfm: (2) use transformations as list names, return data frame
    varXfm <- list(posSqr = c("MP"), negSqr = c("MN"), posLog = c("SP"), negLog = c("SN"), posInv = c("EP"), negInv = c("EN"))
    df4Chk <- jmvReadWrite::transform_vars_omv(dtaInp = dtaInp, varXfm = varXfm)
    expect_s3_class(df4Chk, "data.frame")
    expect_equal(dim(df4Chk), c(1000, 12))
    expect_equal(names(df4Chk), c(names(dtaInp), paste0(names(dtaInp), "_", c(rep("SQR", 2), rep("LOG", 2), rep("INV", 2)))))
    expect_equal(vapply(df4Chk, mean, numeric(1), USE.NAMES = FALSE),
      c(0.44801838, -0.4646020, 0.47815369, -0.49234557, 0.50478086, -0.49908730, 0.78869665, 0.76679464, 0.15589357, 0.15940117, 0.72353452, 0.72355557))
    expect_equal(vapply(df4Chk, sd, numeric(1), USE.NAMES = FALSE),
      c(0.43040192, 0.47964321, 0.47895029, 0.48250463, 0.51346048, 0.50804630, 0.24512659, 0.27358674, 0.12018917, 0.11979942, 0.18327280, 0.17927202))
    expect_equal(vapply(df4Chk, min, numeric(1), USE.NAMES = FALSE),
      c(-0.234050985891947, -2.63444386411986, -0.0148647228014522, -3.47455993472914, 0.00129343898380293, -4.06637548446082, 0, 0, 0, 0, 0.20527105709926, 0.197381201985878))
    expect_equal(vapply(df4Chk, max, numeric(1), USE.NAMES = FALSE),
      c(3.06893926825301, 0.19814687853258, 3.25069026307788, 0.0123121447940763, 3.87290083522986, -0.0000368973062622809, 1.81741306646149, 1.68303022630555,
        0.6299755446769, 0.651943687997723, 1, 1))
    expect_equal(lapply(df4Chk, attr, "formula"),
      list(MP = NULL, MN = NULL, SP = NULL, SN = NULL, EP = NULL, EN = NULL, MP_SQR = "SQRT(MP - VMIN(MP))", MN_SQR = "SQRT(VMAX(MN) - MN)",
           SP_LOG = "LOG10(SP - VMIN(SP) + 1)", SN_LOG = "LOG10(VMAX(SN) + 1 - SN)", EP_INV = "1 / (EP - VMIN(EP) + 1)", EN_INV = "1 / (VMAX(EN) + 1 - EN)"))

    # varXfm: (3) use skewness as list names, return data frame
    varXfm <- list(mdrPos = c("MP"), mdrNeg = c("MN"), strPos = c("SP"), strNeg = c("SN"), svrPos = c("EP"), svrNeg = c("EN"))
    df4Chk <- jmvReadWrite::transform_vars_omv(dtaInp = dtaInp, varXfm = varXfm)
    expect_s3_class(df4Chk, "data.frame")
    expect_equal(dim(df4Chk), c(1000, 12))
    expect_equal(names(df4Chk), c(names(dtaInp), paste0(names(dtaInp), "_", c(rep("SQR", 2), rep("LOG", 2), rep("INV", 2)))))
    expect_equal(vapply(df4Chk, mean, numeric(1), USE.NAMES = FALSE),
      c(0.44801838, -0.4646020, 0.47815369, -0.49234557, 0.50478086, -0.49908730, 0.78869665, 0.76679464, 0.15589357, 0.15940117, 0.72353452, 0.72355557))
    expect_equal(vapply(df4Chk, sd, numeric(1), USE.NAMES = FALSE),
      c(0.43040192, 0.47964321, 0.47895029, 0.48250463, 0.51346048, 0.50804630, 0.24512659, 0.27358674, 0.12018917, 0.11979942, 0.18327280, 0.17927202))
    expect_equal(vapply(df4Chk, min, numeric(1), USE.NAMES = FALSE),
      c(-0.234050985891947, -2.63444386411986, -0.0148647228014522, -3.47455993472914, 0.00129343898380293, -4.06637548446082, 0, 0, 0, 0, 0.20527105709926, 0.197381201985878))
    expect_equal(vapply(df4Chk, max, numeric(1), USE.NAMES = FALSE),
      c(3.06893926825301, 0.19814687853258, 3.25069026307788, 0.0123121447940763, 3.87290083522986, -0.0000368973062622809, 1.81741306646149, 1.68303022630555,
        0.6299755446769, 0.651943687997723, 1, 1))
    expect_equal(lapply(df4Chk, attr, "formula"),
      list(MP = NULL, MN = NULL, SP = NULL, SN = NULL, EP = NULL, EN = NULL, MP_SQR = "SQRT(MP - VMIN(MP))", MN_SQR = "SQRT(VMAX(MN) - MN)",
           SP_LOG = "LOG10(SP - VMIN(SP) + 1)", SN_LOG = "LOG10(VMAX(SN) + 1 - SN)", EP_INV = "1 / (EP - VMIN(EP) + 1)", EN_INV = "1 / (VMAX(EN) + 1 - EN)"))

    # varXfm: (4) use transformations as list names, return data frame, multiple variables per list entry
    varXfm <- list(mdrPos = c("MP"), mdrNeg = c("MN"), strPos = c("MP", "SP"), strNeg = c("MN", "SN"), svrPos = c("MP", "SP", "EP"), svrNeg = c("MN", "SN", "EN"))
    df4Chk <- jmvReadWrite::transform_vars_omv(dtaInp = dtaInp, varXfm = varXfm)
    expect_s3_class(df4Chk, "data.frame")
    expect_equal(dim(df4Chk), c(1000, 18))
    expect_equal(names(df4Chk), c(names(dtaInp),
      paste0(c("MP", "MN", "MP", "SP", "MN", "SN", "MP", "SP", "EP", "MN", "SN", "EN"), "_", c(rep("SQR", 2), rep("LOG", 4), rep("INV", 6)))))
    expect_equal(vapply(df4Chk, mean, numeric(1), USE.NAMES = FALSE),
      c(0.4480184, -0.4646020, 0.4781537, -0.4923456, 0.5047809, -0.4990873, 0.7886966, 0.7667946, 0.2135253,
        0.1558936,  0.2055426, 0.1594012,  0.6272808, 0.7231910,  0.7235345, 0.6422519, 0.7173079, 0.7235556),
      tolerance = 1e-6)
    expect_equal(vapply(df4Chk, sd, numeric(1), USE.NAMES = FALSE),
      c(0.4304019, 0.4796432, 0.4789503, 0.4825046, 0.5134605, 0.5080463, 0.2451266, 0.2735867, 0.1005615,
        0.1201892, 0.1112784, 0.1197994, 0.1348379, 0.1761416, 0.1832728, 0.1493385, 0.1754724, 0.1792720),
      tolerance = 1e-6)
    expect_equal(vapply(df4Chk, min, numeric(1), USE.NAMES = FALSE),
      c(-0.234050985891947, -2.63444386411986, -0.0148647228014522, -3.47455993472914, 0.00129343898380293, -4.06637548446082, 0,  0,  0,  0, 0, 0,
        0.232396529143129, 0.234436082364521, 0.20527105709926, 0.260920110480266, 0.222872411398513, 0.197381201985878))
    expect_equal(vapply(df4Chk, max, numeric(1), USE.NAMES = FALSE),
      c(3.06893926825301, 0.198146878538258, 3.25069026307788, 0.0123121447940763, 3.87290083522986, -0.0000368973062622809, 1.81741306646149,
        1.68303022630555, 0.633770362448845, 0.6299755446769, 0.58349244625809, 0.651943687997723, 1, 1, 1, 1, 1, 1))
    expect_equal(unname(unlist(lapply(df4Chk, attr, "formula"))),
      c("SQRT(MP - VMIN(MP))", "SQRT(VMAX(MN) - MN)", "LOG10(MP - VMIN(MP) + 1)", "LOG10(SP - VMIN(SP) + 1)", "LOG10(VMAX(MN) + 1 - MN)", "LOG10(VMAX(SN) + 1 - SN)",
        "1 / (MP - VMIN(MP) + 1)", "1 / (SP - VMIN(SP) + 1)", "1 / (EP - VMIN(EP) + 1)", "1 / (VMAX(MN) + 1 - MN)", "1 / (VMAX(SN) + 1 - SN)", "1 / (VMAX(EN) + 1 - EN)"))

    # transform the input dataframe so that all columns have a minumum value of 0, varXfm is the same as for varXfm (2) above
    varXfm <- list(posSqr = c("MP"), negSqr = c("MN"), posLog = c("SP"), negLog = c("SN"), posInv = c("EP"), negInv = c("EN"))
    df4Chk <- jmvReadWrite::transform_vars_omv(dtaInp = sweep(dtaInp, 2, vapply(dtaInp, min, numeric(1), USE.NAMES = FALSE)), varXfm = varXfm)
    expect_s3_class(df4Chk, "data.frame")
    expect_equal(dim(df4Chk), c(1000, 12))
    expect_equal(names(df4Chk), c(names(dtaInp), paste0(names(dtaInp), "_", c(rep("SQR", 2), rep("LOG", 2), rep("INV", 2)))))
    expect_equal(vapply(df4Chk, mean, numeric(1), USE.NAMES = FALSE),
      c(0.68206936, 2.16984187, 0.49301841, 2.98221436, 0.50348742, 3.56728819, 0.78869665, 0.76679464, 0.15589357, 0.15940117, 0.7235345, 0.7235556))
    expect_equal(vapply(df4Chk, sd, numeric(1), USE.NAMES = FALSE),
      c(0.43040192, 0.47964321, 0.47895029, 0.48250463, 0.51346048, 0.50804630, 0.24512659, 0.27358674, 0.12018917, 0.11979942, 0.18327280, 0.17927202))
    expect_equal(vapply(df4Chk, min, numeric(1), USE.NAMES = FALSE), c(rep(0, 10), 0.20527105709926, 0.197381201985878))
    expect_equal(vapply(df4Chk, max, numeric(1), USE.NAMES = FALSE),
      c(3.30299025, 2.83259074, 3.26555499, 3.48687208, 3.87160740, 4.06633859, 1.81741306646149, 1.68303022630555, 0.6299755446769, 0.651943687997723, 1, 1))
    expect_equal(lapply(df4Chk, attr, "formula"),
      list(MP = NULL, MN = NULL, SP = NULL, SN = NULL, EP = NULL, EN = NULL, MP_SQR = "SQRT(MP)", MN_SQR = "SQRT(VMAX(MN) - MN)",
           SP_LOG = "LOG10(SP - VMIN(SP) + 1)", SN_LOG = "LOG10(VMAX(SN) + 1 - SN)", EP_INV = "1 / (EP - VMIN(EP) + 1)", EN_INV = "1 / (VMAX(EN) + 1 - EN)"))

    # transform the input dataframe so that all columns have a minumum value of 1, varXfm is the same as for varXfm (2) above
    varXfm <- list(posSqr = c("MP"), negSqr = c("MN"), posLog = c("SP"), negLog = c("SN"), posInv = c("EP"), negInv = c("EN"))
    df4Chk <- jmvReadWrite::transform_vars_omv(dtaInp = sweep(dtaInp, 2, vapply(dtaInp, min, numeric(1), USE.NAMES = FALSE) - 1), varXfm = varXfm)
    expect_s3_class(df4Chk, "data.frame")
    expect_equal(dim(df4Chk), c(1000, 12))
    expect_equal(names(df4Chk), c(names(dtaInp), paste0(names(dtaInp), "_", c(rep("SQR", 2), rep("LOG", 2), rep("INV", 2)))))
    expect_equal(vapply(df4Chk, mean, numeric(1), USE.NAMES = FALSE),
      c(1.68206936, 3.16984187, 1.49301841, 3.98221436, 1.50348742, 4.56728819, 1.28752337, 0.76679464, 0.15589357, 0.15940117, 0.7235345, 0.7235556))
    expect_equal(vapply(df4Chk, sd, numeric(1), USE.NAMES = FALSE),
      c(0.43040192, 0.47964321, 0.47895029, 0.48250463, 0.51346048, 0.50804630, 0.15613233, 0.27358674, 0.12018917, 0.11979942, 0.18327280, 0.17927202))
    expect_equal(vapply(df4Chk, min, numeric(1), USE.NAMES = FALSE), c(rep(1, 7), 0, 0, 0, 0.20527106, 0.1973812))
    expect_equal(vapply(df4Chk, max, numeric(1), USE.NAMES = FALSE),
      c(4.30299025, 3.83259074, 4.26555499, 4.48687208, 4.87160740, 5.06633859, 2.07436502, 1.68303022630555, 0.6299755446769, 0.651943687997723, 1, 1))
    expect_equal(lapply(df4Chk, attr, "formula"),
      list(MP = NULL, MN = NULL, SP = NULL, SN = NULL, EP = NULL, EN = NULL, MP_SQR = "SQRT(MP)", MN_SQR = "SQRT(VMAX(MN) - MN)",
           SP_LOG = "LOG10(SP)", SN_LOG = "LOG10(VMAX(SN) + 1 - SN)", EP_INV = "1 / (EP)", EN_INV = "1 / (VMAX(EN) + 1 - EN)"))

    # test cases for code coverage ============================================================================================================================
    varXfm <- list(posSqr = c("MP"), negSqr = c("MN"), posLog = c("SP"), negLog = c("SN"), posInv = c("EP"), negInv = c("EN"))
    expect_error(transform_vars_omv(fleInp = "Trial.omv", varXfm = varXfm), regexp = "Please use the argument dtaInp instead of fleInp\\.")
    expect_error(transform_vars_omv(dtaInp = dtaInp, fleOut = nmeOut),
      regexp = "^Calling transform_vars_omv requires the parameter varXfm, using the correct format \\(see Details in help\\)\\.")
    expect_false(file.exists(nmeOut))
    expect_error(transform_vars_omv(dtaInp = dtaInp, fleOut = nmeOut, varXfm = list()),
      regexp = "^Calling transform_vars_omv requires the parameter varXfm, using the correct format \\(see Details in help\\)\\.")
    expect_false(file.exists(nmeOut))
    expect_error(transform_vars_omv(dtaInp = dtaInp, fleOut = nmeOut, varXfm = c("MP", "MN")),
      regexp = "^Calling transform_vars_omv requires the parameter varXfm, using the correct format \\(see Details in help\\)\\.")
    expect_false(file.exists(nmeOut))
    expect_error(transform_vars_omv(dtaInp = dtaInp, fleOut = nmeOut, varXfm = list(wrgNme = "MP")),
      regexp = "^The parameter varXfm has an invalid entry \\(wrong name\\), please use the correct format \\(see Details in help\\)\\.")
    expect_false(file.exists(nmeOut))
    expect_error(transform_vars_omv(dtaInp = dtaInp, fleOut = nmeOut, varXfm = list(posSqr = c("MP", "V1", "V2"))),
      regexp = "^All columns / variables given in the parameter varXfm need to be contained in the input data frame \\(dtaInp\\), but variable\\(s\\)")
    expect_false(file.exists(nmeOut))

})
