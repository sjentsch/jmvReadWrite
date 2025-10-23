test_that("aggregate_omv works", {
    nmeInp <- tempfile(fileext = ".rds")
    nmeOut <- tempfile(fileext = "_A.omv")
    clcStr <- c("N",  "Mss",   "Mn",   "Mdn",    "Mde",  "Sum",  "SD",  "Var",      "Rng",   "Min",  "Max",  "IQR")
    clcDsc <- c("N",  "Miss.", "Mean", "Median", "Mode", "Sum",  "SD",  "Variance", "Range", "Min.", "Max.", "IQR")

    # simple data set with a factorial and a continuous variable ======================================================
    set.seed(1234)
    dtaTmp <- data.frame(ID = rep(as.character(seq(1, 100)), each = 10), Measure = rep(seq(10), times = 100),
                         V1 = runif(n = 100 * 10, 0, 100), V2 = as.factor(round(rnorm(n = 100 * 10, 3, 2 / 3))))
    attr(dtaTmp[, "V1"], "jmv-desc") <- "Variable V1"
    attr(dtaTmp[, "V2"], "jmv-desc") <- "Variable V2"
    saveRDS(dtaTmp, nmeInp)

    # using a grouping variable (ID)
    expect_null(aggregate_omv(dtaInp = nmeInp, fleOut = nmeOut, varAgg = c("V1", "V2"), grpAgg = "ID",
                              clcN = TRUE, clcMss = TRUE, clcMn = TRUE, clcMdn = TRUE, clcMde = TRUE, clcSum = TRUE,
                              clcSD = TRUE, clcVar = TRUE, clcRng = TRUE, clcMin = TRUE, clcMax = TRUE, clcIQR = TRUE))
    df4Chk <- read_omv(nmeOut)
    expect_s3_class(df4Chk, "data.frame")
    expect_equal(dim(df4Chk), c(100, 25))
    expect_equal(vapply(df4Chk, typeof, character(1), USE.NAMES = FALSE),
                 c("character", rep("integer", 2), rep("double", 10), rep("integer", 2), rep("double", 2), rep("integer", 2), rep("double", 2),
                   rep("integer", 3), "double"))
    expect_equal(names(df4Chk), c("ID", paste0(rep(c("V1_", "V2_"), each = 12), rep(clcStr, 2))))
    expect_equal(unname(colMeans(df4Chk[-1])),
                 c(10, 0, 50.7273, 50.3691, 8.9539, 507.2735, 28.7392, 848.0571, 82.6450, 8.9539, 91.5989, 41.2718,
                   10, 0,  2.9940,  2.9800, 2.9700,  29.9400,  0.6790,   0.4862,  2.0500, 1.9700,  4.0200,  0.5900),
                 tolerance = 1e-4)
    expect_equal(vapply(df4Chk[-1], attr, character(1), "jmv-desc", USE.NAMES = FALSE),
                 sprintf("Variable %s (%s)", rep(c("V1", "V2"), each = 12), rep(clcDsc, 2)))
    unlink(nmeOut)

    # not using a grouping variable
    expect_null(aggregate_omv(dtaInp = nmeInp, fleOut = nmeOut, varAgg = c("V1", "V2"),
                              clcN = TRUE, clcMss = TRUE, clcMn = TRUE, clcMdn = TRUE, clcMde = TRUE, clcSum = TRUE,
                              clcSD = TRUE, clcVar = TRUE, clcRng = TRUE, clcMin = TRUE, clcMax = TRUE, clcIQR = TRUE))
    df4Chk <- read_omv(nmeOut)
    expect_s3_class(df4Chk, "data.frame")
    expect_equal(dim(df4Chk), c(1, 24))
    expect_equal(vapply(df4Chk, typeof, character(1), USE.NAMES = FALSE),
                 c(rep("integer", 2), rep("double", 10), rep("integer", 2), "double", rep("integer", 3), rep("double", 2),
                   rep("integer", 4)))
    expect_equal(names(df4Chk), paste0(rep(c("V1_", "V2_"), each = 12), rep(clcStr, 2)))
    expect_equal(as.numeric(unname(df4Chk)),
                 c(1000, 0, 50.7273, 51.0192, 0.0342, 50727.3463, 29.1208, 848.0223, 99.8961, 0.0342, 99.9303, 50.0280,
                   1000, 0,  2.9940,  3.0000, 3.0000,  2994.0000,  0.6946,   0.4824,  4.0000, 1.0000,  4.0200,  0.5900),
                 tolerance = 1e-4)
    expect_equal(vapply(df4Chk, attr, character(1), "jmv-desc", USE.NAMES = FALSE),
                 sprintf("Variable %s (%s)", rep(c("V1", "V2"), each = 12), rep(clcDsc, 2)))
    unlink(nmeOut)


    # simple data set with two continuous variables ===================================================================
    set.seed(1)
    dtaTmp <- data.frame(ID = rep(as.character(seq(1, 100)), each = 10), Measure = rep(seq(10), times = 100),
                         V1 = runif(n = 100 * 10, 0, 100), V2 = round(rnorm(n = 100 * 10, 100, 15)))
    attr(dtaTmp[, "V1"], "jmv-desc") <- "Variable V1"
    attr(dtaTmp[, "V2"], "jmv-desc") <- "Variable V2"
    saveRDS(dtaTmp, nmeInp)

    expect_null(aggregate_omv(dtaInp = nmeInp, fleOut = nmeOut, varAgg = c("V1", "V2"), grpAgg = "ID",
                              clcN = TRUE, clcMss = TRUE, clcMn = TRUE, clcMdn = TRUE, clcMde = TRUE, clcSum = TRUE,
                              clcSD = TRUE, clcVar = TRUE, clcRng = TRUE, clcMin = TRUE, clcMax = TRUE, clcIQR = TRUE))
    df4Chk <- read_omv(nmeOut)
    expect_s3_class(df4Chk, "data.frame")
    expect_equal(dim(df4Chk), c(100, 25))
    expect_equal(vapply(df4Chk, typeof, character(1), USE.NAMES = FALSE),
                 c("character", rep("integer", 2), rep("double", 10), rep("integer", 2), rep("double", 2), rep("integer", 2), rep("double", 2),
                   rep("integer", 3), "double"))
    expect_equal(names(df4Chk), c("ID", paste0(rep(c("V1_", "V2_"), each = 12), rep(clcStr, 2))))
    expect_equal(unname(colMeans(df4Chk[-1])),
                 c(10, 0, 49.9692, 49.8635,  9.3976, 499.6917, 28.2575, 818.0221, 81.4983,  9.3976,  90.8959, 39.0899,
                   10, 0, 99.6310, 99.3150, 88.6200, 996.3100, 15.1117, 239.1143, 47.6100, 76.1400, 123.7500, 18.2550),
                 tolerance = 1e-4)
    expect_equal(vapply(df4Chk[-1], attr, character(1), "jmv-desc", USE.NAMES = FALSE),
                 sprintf("Variable %s (%s)", rep(c("V1", "V2"), each = 12), rep(clcDsc, 2)))
    unlink(nmeOut)

    expect_null(aggregate_omv(dtaInp = nmeInp, fleOut = nmeOut, varAgg = c("V1", "V2"),
                              clcN = TRUE, clcMss = TRUE, clcMn = TRUE, clcMdn = TRUE, clcMde = TRUE, clcSum = TRUE,
                              clcSD = TRUE, clcVar = TRUE, clcRng = TRUE, clcMin = TRUE, clcMax = TRUE, clcIQR = TRUE))
    df4Chk <- read_omv(nmeOut)
    expect_s3_class(df4Chk, "data.frame")
    expect_equal(dim(df4Chk), c(1, 24))
    expect_equal(vapply(df4Chk, typeof, character(1), USE.NAMES = FALSE),
                 c(rep("integer", 2), rep("double", 10), rep("integer", 2), "double", rep("integer", 3), rep("double", 2),
                   rep("integer", 4)))
    expect_equal(names(df4Chk), paste0(rep(c("V1_", "V2_"), each = 12), rep(clcStr, 2)))
    expect_equal(as.numeric(unname(df4Chk)),
                 c(1000, 0, 49.9692, 48.3260,  0.1315, 49969.1673, 28.8387, 831.6708,  99.8616,  0.1315,  99.9931, 48.8804,
                   1000, 0, 99.6310, 99.0000, 96.0000, 99631.0000, 15.5205, 240.8857, 104.0000, 51.0000, 155.0000, 22.0000),
                 tolerance = 1e-4)
    expect_equal(vapply(df4Chk, attr, character(1), "jmv-desc", USE.NAMES = FALSE),
                 sprintf("Variable %s (%s)", rep(c("V1", "V2"), each = 12), rep(clcDsc, 2)))
    unlink(nmeOut)

    # simple data set with NAs ========================================================================================
    set.seed(1)
    dtaTmp[sample(nrow(dtaTmp), 10), "V1"] <- NA
    dtaTmp[sample(nrow(dtaTmp), 10), "V2"] <- NA

    expect_null(aggregate_omv(dtaInp = dtaTmp, fleOut = nmeOut, varAgg = c("V1", "V2"), grpAgg = "ID",
                              clcN = TRUE, clcMss = TRUE, clcMn = TRUE, clcMdn = TRUE, clcMde = TRUE, clcSum = TRUE,
                              clcSD = TRUE, clcVar = TRUE, clcRng = TRUE, clcMin = TRUE, clcMax = TRUE, clcIQR = TRUE))
    df4Chk <- read_omv(nmeOut)
    expect_s3_class(df4Chk, "data.frame")
    expect_equal(dim(df4Chk), c(100, 25))
    expect_equal(sort(dtaTmp[is.na(dtaTmp$V1), "ID"]), df4Chk[df4Chk$V1_N < 10, "ID"])
    expect_equal(sort(dtaTmp[is.na(dtaTmp$V2), "ID"]), df4Chk[df4Chk$V2_N < 10, "ID"])
    expect_equal(unname(colMeans(df4Chk[-1])),
                 c(9.9, 0.1, 50.0834, 50.1054,  9.4463, 495.3516, 28.3369, 822.7721, 81.4495,  9.4463,  90.8959, 39.0855,
                   9.9, 0.1, 99.5748, 99.2950, 88.7600, 985.7900, 15.0837, 238.6681, 47.3300, 76.2800, 123.6100, 18.2550),
                 tolerance = 1e-4)
    unlink(nmeOut)

    expect_null(aggregate_omv(dtaInp = dtaTmp, fleOut = nmeOut, varAgg = c("V1", "V2"), grpAgg = "ID", drpNA = FALSE,
                              clcN = TRUE, clcMss = TRUE, clcMn = TRUE, clcMdn = TRUE, clcMde = TRUE, clcSum = TRUE,
                              clcSD = TRUE, clcVar = TRUE, clcRng = TRUE, clcMin = TRUE, clcMax = TRUE, clcIQR = TRUE))
    df4Chk <- read_omv(nmeOut)
    expect_s3_class(df4Chk, "data.frame")
    expect_equal(dim(df4Chk), c(100, 25))
    expect_true(all(is.na(df4Chk[df4Chk$V1_N < 10, sprintf("V1_%s", setdiff(clcStr, c("N", "Mss")))])))
    expect_true(all(is.na(df4Chk[df4Chk$V2_N < 10, sprintf("V2_%s", setdiff(clcStr, c("N", "Mss")))])))
    expect_equal(unname(colMeans(df4Chk[-1])), c(9.9, 0.1, rep(NA, 10), 9.9, 0.1, rep(NA, 10)))
    unlink(nmeOut)

    # simple data set with NAs in the grouping variable ===============================================================
    set.seed(1)
    dtaTmp <- data.frame(ID = rep(as.character(seq(1, 100)), each = 10), Measure = rep(seq(10), times = 100),
                         V1 = runif(n = 100 * 10, 0, 100), V2 = round(rnorm(n = 100 * 10, 100, 15)))
    dtaTmp[sample(nrow(dtaTmp), 10), "ID"] <- NA

    expect_null(aggregate_omv(dtaInp = dtaTmp, fleOut = nmeOut, varAgg = c("V1", "V2"), grpAgg = "ID", clcMn = TRUE))
    expect_s3_class(df4Chk, "data.frame")
    expect_equal(dim(df4Chk), c(100, 25))
    expect_true(all(is.na(df4Chk[df4Chk$V1_N < 10, sprintf("V1_%s", setdiff(clcStr, c("N", "Mss")))])))
    expect_true(all(is.na(df4Chk[df4Chk$V2_N < 10, sprintf("V2_%s", setdiff(clcStr, c("N", "Mss")))])))
    expect_equal(unname(colMeans(df4Chk[-1])), c(9.9, 0.1, rep(NA, 10), 9.9, 0.1, rep(NA, 10)))
    unlink(nmeOut)

    expect_error(aggregate_omv(dtaInp = dtaTmp, fleOut = nmeOut, varAgg = c("V1", "V2"), grpAgg = "ID", drpNA = FALSE, clcMn = TRUE),
                 regexp = "The grouping variables must not contain empty or NA values \\(if drpNA is set to FALSE\\)\\.")

    # code coverage ===================================================================================================
    expect_error(aggregate_omv(fleInp = nmeInp, varAgg = c("V1", "V2"), grpAgg = "ID"),
                 regexp = "Please use the argument dtaInp instead of fleInp\\.")

    expect_error(aggregate_omv(dtaInp = nmeInp, varAgg = c("X", "Y"), grpAgg = "ID", clcMn = TRUE),
                 regexp = paste("Calling aggregate_omv requires giving at least one \\(valid\\) variable to aggregate,",
                                "and the grouping variable\\(s\\) needs to be empty or valid\\."))
    expect_error(aggregate_omv(dtaInp = nmeInp, varAgg = c("V1", "V2"), grpAgg = "ID"),
                 regexp = "At least one aggregation calculation \\(clc\\.\\.\\.\\) needs to be set to TRUE\\.")

    unlink(nmeInp)
})
