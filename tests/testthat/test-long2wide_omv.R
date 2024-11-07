test_that("long2wide_omv works", {
    # rather simple data set ==================================================================================================================================
    set.seed(1)
    dtaTmp <- data.frame(Year = sort(rep(1900:2020, 12)), Month = rep(month.abb[1:12], 121), X = runif(n = 121 * 12, 0, 100), Y = runif(n = 121 * 12, 0, 100))
    attr(dtaTmp[["X"]], "jmv-desc") <- "Variable X"
    attr(dtaTmp[["Y"]], "jmv-desc") <- "Variable Y"
    nmeInp <- tempfile(fileext = ".rds")
    nmeOut <- tempfile(fileext = "_W.omv")
    saveRDS(dtaTmp, nmeInp)

    expect_null(long2wide_omv(dtaInp = nmeInp, fleOut = nmeOut, varID = "Year", varTme = "Month"))
    df4Chk <- read_omv(nmeOut)
    expect_s3_class(df4Chk, "data.frame")
    expect_equal(dim(df4Chk), c(121, 25))
    expect_equal(vapply(df4Chk, typeof, character(1), USE.NAMES = FALSE), c("character", rep("double", 24)))
    expect_equal(names(df4Chk), c("Year", paste0(rep(c("X_", "Y_"), each = 12), month.abb[rep(1:12, times = 2)])))
    expect_equal(unname(colMeans(df4Chk[-1])), c(51.05398, 51.52200, 50.90146, 47.98040, 46.28997, 53.70601, 49.47946, 49.24704, 49.92602, 44.93970, 49.37357, 47.55488,
                                                 48.56846, 48.96117, 47.64545, 46.51572, 50.94652, 47.33624, 47.53437, 55.55701, 51.50431, 50.19580, 52.81145, 43.68338), tolerance = 1e-4)
    expect_equal(vapply(df4Chk[-1], attr, character(1), "jmv-desc", USE.NAMES = FALSE),
      sprintf("%s (Month: %s)", rep(c("Variable X", "Variable Y"), each = 12), month.abb[rep(1:12, times = 2)]))
    unlink(nmeOut)

    expect_null(long2wide_omv(dtaInp = dtaTmp, fleOut = nmeOut, varID = "Year", varTme = "Month", varOrd = "vars"))
    df4Chk <- read_omv(nmeOut)
    expect_s3_class(df4Chk, "data.frame")
    expect_equal(dim(df4Chk), c(121, 25))
    expect_equal(vapply(df4Chk, typeof, character(1), USE.NAMES = FALSE), c("character", rep("double", 24)))
    expect_equal(names(df4Chk), c("Year", paste0(rep(c("X_", "Y_"), times = 12), month.abb[rep(1:12, each = 2)])))
    expect_equal(unname(colMeans(df4Chk[-1])), c(51.05398, 48.56846, 51.52200, 48.96117, 50.90146, 47.64545, 47.98040, 46.51572, 46.28997, 50.94652, 53.70601, 47.33624,
                                                 49.47946, 47.53437, 49.24704, 55.55701, 49.92602, 51.50431, 44.93970, 50.19580, 49.37357, 52.81145, 47.55488, 43.68338), tolerance = 1e-4)
    expect_equal(vapply(df4Chk[-1], attr, character(1), "jmv-desc", USE.NAMES = FALSE),
      sprintf("%s (Month: %s)", rep(c("Variable X", "Variable Y"), times = 12), month.abb[rep(1:12, each = 2)]))
    unlink(nmeOut)

    expect_null(long2wide_omv(dtaInp = nmeInp, fleOut = nmeOut, varID = "Year", varTme = "Month", varTgt = c("X")))
    df4Chk <- read_omv(nmeOut)
    expect_s3_class(df4Chk, "data.frame")
    expect_equal(dim(df4Chk), c(121, 13))
    expect_equal(vapply(df4Chk, typeof, character(1), USE.NAMES = FALSE), c("character", rep("double", 12)))
    expect_equal(names(df4Chk), c("Year", paste0("X_", month.abb[1:12])))
    expect_equal(unname(colMeans(df4Chk[-1])), c(51.05398, 51.52200, 50.90146, 47.98040, 46.28997, 53.70601, 49.47946, 49.24704, 49.92602, 44.93970, 49.37357, 47.55488), tolerance = 1e-4)
    expect_equal(vapply(df4Chk[-1], attr, character(1), "jmv-desc", USE.NAMES = FALSE),
      sprintf("Variable X (Month: %s)", month.abb[1:12]))
    unlink(nmeOut)

    expect_null(long2wide_omv(dtaInp = nmeInp, fleOut = nmeOut, varID = "Year", varTme = "Month", varTgt = c("Y")))
    df4Chk <- read_omv(nmeOut)
    expect_s3_class(df4Chk, "data.frame")
    expect_equal(dim(df4Chk), c(121, 13))
    expect_equal(vapply(df4Chk, typeof, character(1), USE.NAMES = FALSE), c("character", rep("double", 12)))
    expect_equal(names(df4Chk), c("Year", paste0("Y_", month.abb[1:12])))
    expect_equal(unname(colMeans(df4Chk[-1])), c(48.56846, 48.96117, 47.64545, 46.51572, 50.94652, 47.33624, 47.53437, 55.55701, 51.50431, 50.19580, 52.81145, 43.68338), tolerance = 1e-4)
    expect_equal(vapply(df4Chk[-1], attr, character(1), "jmv-desc", USE.NAMES = FALSE),
      sprintf("Variable Y (Month: %s)", month.abb[1:12]))
    unlink(nmeOut)

    df4Chk <- long2wide_omv(dtaInp = dtaTmp, varID = "Year", varTme = "Month")
    expect_s3_class(df4Chk, "data.frame")
    expect_equal(dim(df4Chk), c(121, 25))
    expect_equal(vapply(df4Chk, typeof, character(1), USE.NAMES = FALSE), c("integer", rep("double", 24)))
    expect_equal(names(df4Chk), c("Year", paste0(rep(c("X_", "Y_"), each = 12), month.abb[rep(1:12, 2)])))
    expect_equal(unname(colMeans(df4Chk[-1])), c(51.05398, 51.52200, 50.90146, 47.98040, 46.28997, 53.70601, 49.47946, 49.24704, 49.92602, 44.93970, 49.37357, 47.55488,
                                                 48.56846, 48.96117, 47.64545, 46.51572, 50.94652, 47.33624, 47.53437, 55.55701, 51.50431, 50.19580, 52.81145, 43.68338), tolerance = 1e-4)
    expect_equal(vapply(df4Chk[-1], attr, character(1), "jmv-desc", USE.NAMES = FALSE),
      sprintf("%s (Month: %s)", rep(c("Variable X", "Variable Y"), each = 12), month.abb[rep(1:12, times = 2)]))

    expect_null(long2wide_omv(dtaInp = nmeInp, fleOut = nmeOut, varID = "Year", varTme = "Month", varSep = "."))
    df4Chk <- read_omv(nmeOut)
    expect_s3_class(df4Chk, "data.frame")
    expect_equal(dim(df4Chk), c(121, 25))
    expect_equal(vapply(df4Chk, typeof, character(1), USE.NAMES = FALSE), c("character", rep("double", 24)))
    expect_equal(names(df4Chk), c("Year", paste0(rep(c("X.", "Y."), each = 12), month.abb[rep(1:12, times = 2)])))
    expect_equal(unname(colMeans(df4Chk[-1])), c(51.05398, 51.52200, 50.90146, 47.98040, 46.28997, 53.70601, 49.47946, 49.24704, 49.92602, 44.93970, 49.37357, 47.55488,
                                                 48.56846, 48.96117, 47.64545, 46.51572, 50.94652, 47.33624, 47.53437, 55.55701, 51.50431, 50.19580, 52.81145, 43.68338), tolerance = 1e-4)
    expect_equal(vapply(df4Chk[-1], attr, character(1), "jmv-desc", USE.NAMES = FALSE),
      sprintf("%s (Month: %s)", rep(c("Variable X", "Variable Y"), each = 12), month.abb[rep(1:12, times = 2)]))
    unlink(nmeOut)

    # more complex data set ===================================================================================================================================
    set.seed(1)
    dtaTmp <- data.frame(ID = rep(sprintf("sbj_%03d", seq(1, 100)), each = 24), sex = rep(sample(rep(c("male", "female"), each = 50)), each = 24),
                         cond = rep(rep(c("cong", "incong", "neutral"), each = 8), times = 100), colour = rep(c("RED", "GREEN", "BLUE", "YELLOW"), times = 600),
                         rep = rep(rep(c(1, 2), each = 4), times = 300), rspCrr = as.integer(runif(2400, 0, 1) > rep(rep(c(0.002, 0.03, 0.02), each = 8), times = 100)),
                         rspTme = rep(rep(c(0.450, 0.530, 0.510), each = 8), times = 100) + rnorm(2400) * 0.05)[sample(2400), ]
    dtaTmp <- dtaTmp[order(dtaTmp$ID, dtaTmp$rep), ]
    lblTmp <- list(ID = "Participant ID", sex = "Sex", cond = "Exp. cond. – congruency", colour = "Colour to be named", rep = "Repetition", rspCrr = "Correct response", rspTme = "Reaction time")
    for (i in seq_along(lblTmp)) attr(dtaTmp[[names(lblTmp[i])]], "jmv-desc") <- lblTmp[[i]]

    df4Chk <- long2wide_omv(dtaInp = dtaTmp, varID = "ID", varTme = c("cond", "colour", "rep"), varTgt = c("rspCrr", "rspTme"), varExc = "sex")
    expect_s3_class(df4Chk, "data.frame")
    expect_equal(dim(df4Chk), c(100, 50))
    expect_equal(names(df4Chk), c("ID", "sex", sprintf("%s_%s_%s_%d", rep(c("rspCrr", "rspTme"), each = 24), rep(rep(c("neutral", "cong", "incong"), each = 8), times = 2),
                                                       rep(c("RED", "BLUE", "GREEN", "YELLOW"), each = 2), rep(1:2, times = 24))))
    avgTmp <- aggregate(x = dtaTmp[, c("rspCrr", "rspTme")], by = dtaTmp[, c("cond", "colour", "rep")], FUN = mean)
    expect_equal(unname(colMeans(df4Chk[, sprintf("rspCrr_%s_%s_%s", avgTmp$cond, avgTmp$colour, avgTmp$rep)])), avgTmp$rspCrr)
    expect_equal(unname(colMeans(df4Chk[, sprintf("rspTme_%s_%s_%s", avgTmp$cond, avgTmp$colour, avgTmp$rep)])), avgTmp$rspTme)
    expect_equal(vapply(df4Chk, attr, character(1), "jmv-desc", USE.NAMES = FALSE),
      c(unname(unlist(lblTmp[1:2])), sprintf("%s (cond: %s, colour: %s, rep: %d)",
      rep(unname(unlist(lblTmp[6:7])), each = 24), rep(rep(c("neutral", "cong", "incong"), each = 8), times = 2), rep(c("RED", "BLUE", "GREEN", "YELLOW"), each = 2), rep(1:2, times = 24))))

    df4Chk <- long2wide_omv(dtaInp = dtaTmp, varID = "ID", varTme = "cond", varTgt = c("rspCrr", "rspTme"), varExc = "sex")
    expect_s3_class(df4Chk, "data.frame")
    expect_equal(dim(df4Chk), c(100, 8))
    expect_equal(names(df4Chk), c("ID", "sex", sprintf("%s_%s", rep(c("rspCrr", "rspTme"), each = 3), rep(rep(c("cong", "incong", "neutral"), times = 2)))))
    avgTmp <- aggregate(x = dtaTmp[, c("rspCrr", "rspTme")], by = dtaTmp[, c("sex", "cond"), drop = FALSE], FUN = mean)
    expect_equal(as.vector(unlist(avgTmp[c(-1, -2)])), unname(unlist(aggregate(x = df4Chk[3:8], by = df4Chk[, "sex", drop = FALSE], FUN = mean)[-1])))
    expect_equal(vapply(df4Chk, attr, character(1), "jmv-desc", USE.NAMES = FALSE),
      c(unname(unlist(lblTmp[1:2])), sprintf("%s (cond: %s)",
      rep(unname(unlist(lblTmp[6:7])), each = 3), rep(c("cong", "incong", "neutral"), times = 2))))

    df4Chk <- long2wide_omv(dtaInp = dtaTmp, varID = "ID", varTme = "cond", varTgt = c("rspCrr", "rspTme"))
    expect_s3_class(df4Chk, "data.frame")
    expect_equal(dim(df4Chk), c(100, 7))
    expect_equal(names(df4Chk), c("ID", sprintf("%s_%s", rep(c("rspCrr", "rspTme"), each = 3), rep(rep(c("cong", "incong", "neutral"), times = 2)))))
    avgTmp <- aggregate(x = dtaTmp[, c("rspCrr", "rspTme")], by = dtaTmp[, c("cond"), drop = FALSE], FUN = mean)
    expect_equal(unname(colMeans(df4Chk[2:7])), as.vector(unlist(avgTmp[-1])))
    expect_equal(vapply(df4Chk, attr, character(1), "jmv-desc", USE.NAMES = FALSE), c(unname(unlist(lblTmp[1])), sprintf("%s (cond: %s)",
      rep(unname(unlist(lblTmp[6:7])), each = 3), rep(c("cong", "incong", "neutral"), times = 2))))

    df4Chk <- long2wide_omv(dtaInp = dtaTmp, varID = "ID", varTme = "cond", varTgt = c("rspCrr", "rspTme"), varExc = "sex", varOrd = "vars")
    expect_s3_class(df4Chk, "data.frame")
    expect_equal(dim(df4Chk), c(100, 8))
    expect_equal(names(df4Chk), c("ID", "sex", sprintf("%s_%s", rep(c("rspCrr", "rspTme"), times = 3), rep(rep(c("cong", "incong", "neutral"), each = 2)))))
    avgTmp <- aggregate(x = dtaTmp[, c("rspCrr", "rspTme")], by = dtaTmp[, c("cond"), drop = FALSE], FUN = mean)
    expect_equal(unname(colMeans(df4Chk[3:8])), as.vector(t(avgTmp[-1])))
    expect_equal(vapply(df4Chk, attr, character(1), "jmv-desc", USE.NAMES = FALSE), c(unname(unlist(lblTmp[1:2])), sprintf("%s (cond: %s)",
      rep(unname(unlist(lblTmp[6:7])), times = 3), rep(c("cong", "incong", "neutral"), each = 2))))

    df4Chk <- long2wide_omv(dtaInp = dtaTmp, varID = "ID", varTme = "cond", varTgt = c("rspCrr", "rspTme"), varExc = "sex", varAgg = "first")
    expect_s3_class(df4Chk, "data.frame")
    expect_equal(dim(df4Chk), c(100, 8))
    expect_equal(names(df4Chk), c("ID", "sex", sprintf("%s_%s", rep(c("rspCrr", "rspTme"), each = 3), rep(rep(c("cong", "incong", "neutral"), times = 2)))))
    avgTmp <- aggregate(x = dtaTmp[, c("rspCrr", "rspTme")], by = dtaTmp[, c("ID", "cond")], FUN = "[[", 1)
    expect_equal(unname(as.matrix(df4Chk[3:8])), cbind(matrix(avgTmp[, "rspCrr"], ncol = 3), matrix(avgTmp[, "rspTme"], ncol = 3)))
    expect_equal(vapply(df4Chk, attr, character(1), "jmv-desc", USE.NAMES = FALSE), c(unname(unlist(lblTmp[1:2])), sprintf("%s (cond: %s)",
      rep(unname(unlist(lblTmp[6:7])), each = 3), rep(c("cong", "incong", "neutral"), times = 2))))

    df4Chk <- long2wide_omv(dtaInp = dtaTmp, varID = "ID", varTme = "cond", varTgt = c("rspCrr", "rspTme"), varExc = "sex", varAgg = "mean")
    expect_s3_class(df4Chk, "data.frame")
    expect_equal(dim(df4Chk), c(100, 8))
    expect_equal(names(df4Chk), c("ID", "sex", sprintf("%s_%s", rep(c("rspCrr", "rspTme"), each = 3), rep(rep(c("cong", "incong", "neutral"), times = 2)))))
    avgTmp <- aggregate(x = dtaTmp[, c("rspCrr", "rspTme")], by = dtaTmp[, c("ID", "cond")], FUN = mean)
    expect_equal(unname(as.matrix(df4Chk[3:8])), cbind(matrix(avgTmp[, "rspCrr"], ncol = 3), matrix(avgTmp[, "rspTme"], ncol = 3)))
    expect_equal(vapply(df4Chk, attr, character(1), "jmv-desc", USE.NAMES = FALSE), c(unname(unlist(lblTmp[1:2])), sprintf("%s (cond: %s)",
      rep(unname(unlist(lblTmp[6:7])), each = 3), rep(c("cong", "incong", "neutral"), times = 2))))

    # ---------------------------------------------------------------------------------------------------------------------------------------------------------
    dtaTmp[seq(1, 2400, 25), c("rspCrr", "rspTme")] <- NA

    df4Chk <- long2wide_omv(dtaInp = dtaTmp, varID = "ID", varTme = "cond", varTgt = c("rspCrr", "rspTme"), varExc = "sex", varAgg = "first")
    expect_s3_class(df4Chk, "data.frame")
    expect_equal(dim(df4Chk), c(100, 8))
    expect_equal(names(df4Chk), c("ID", "sex", sprintf("%s_%s", rep(c("rspCrr", "rspTme"), each = 3), rep(rep(c("cong", "incong", "neutral"), times = 2)))))
    avgTmp <- aggregate(x = dtaTmp[, c("rspCrr", "rspTme")], by = dtaTmp[, c("ID", "cond")], FUN = function(x) x[!is.na(x)][1])
    expect_equal(unname(as.matrix(df4Chk[3:8])), cbind(matrix(avgTmp[, "rspCrr"], ncol = 3), matrix(avgTmp[, "rspTme"], ncol = 3)))
    expect_equal(vapply(df4Chk, attr, character(1), "jmv-desc", USE.NAMES = FALSE), c(unname(unlist(lblTmp[1:2])), sprintf("%s (cond: %s)",
      rep(unname(unlist(lblTmp[6:7])), each = 3), rep(c("cong", "incong", "neutral"), times = 2))))

    df4Chk <- long2wide_omv(dtaInp = dtaTmp, varID = "ID", varTme = "cond", varTgt = c("rspCrr", "rspTme"), varExc = "sex", varAgg = "mean")
    expect_s3_class(df4Chk, "data.frame")
    expect_equal(dim(df4Chk), c(100, 8))
    expect_equal(names(df4Chk), c("ID", "sex", sprintf("%s_%s", rep(c("rspCrr", "rspTme"), each = 3), rep(rep(c("cong", "incong", "neutral"), times = 2)))))
    avgTmp <- aggregate(x = dtaTmp[, c("rspCrr", "rspTme")], by = dtaTmp[, c("ID", "cond")], FUN = mean, na.rm = TRUE)
    expect_equal(unname(as.matrix(df4Chk[3:8])), cbind(matrix(avgTmp[, "rspCrr"], ncol = 3), matrix(avgTmp[, "rspTme"], ncol = 3)))
    expect_equal(vapply(df4Chk, attr, character(1), "jmv-desc", USE.NAMES = FALSE), c(unname(unlist(lblTmp[1:2])), sprintf("%s (cond: %s)",
      rep(unname(unlist(lblTmp[6:7])), each = 3), rep(c("cong", "incong", "neutral"), times = 2))))

    # ---------------------------------------------------------------------------------------------------------------------------------------------------------

    dtaTmp$rspCrr <- as.factor(dtaTmp$rspCrr)
    expect_error(long2wide_omv(dtaInp = dtaTmp, varID = "ID", varTme = "cond", varTgt = c("rspCrr", "rspTme"), varExc = "sex"),
      regexp = "^In order to calculate the mean when aggregating the data, all target variables \\(varTgt\\) need to be numeric\\.")

    # test cases for code coverage ============================================================================================================================
    expect_error(long2wide_omv(fleInp = nmeInp, varID = "Year", varTme = "Month"), regexp = "Please use the argument dtaInp instead of fleInp\\.")

    expect_error(long2wide_omv(dtaInp = nmeInp, fleOut = nmeOut, varID = "Year", varTme = ""),
      regexp = "^Using the arguments varID and varTme is mandatory \\(i\\.e\\., they can't be empty\\)\\.")
    expect_error(long2wide_omv(dtaInp = nmeInp, fleOut = nmeOut, varID = "", varTme = "Month"),
      regexp = "^Using the arguments varID and varTme is mandatory \\(i\\.e\\., they can't be empty\\)\\.")
    expect_error(long2wide_omv(dtaInp = nmeInp, fleOut = nmeOut, varID = "Year", varTme = c("Month", "Month2")),
      regexp = "^The variable\\(s\\) .* are not contained in the current data set\\.")
    expect_error(long2wide_omv(dtaInp = dtaTmp, fleOut = nmeOut, varID = "Year", varTme = ""),
      regexp = "^Using the arguments varID and varTme is mandatory \\(i\\.e\\., they can't be empty\\)\\.")
    expect_error(long2wide_omv(dtaInp = dtaTmp, fleOut = nmeOut, varID = "", varTme = "Month"),
      regexp = "^Using the arguments varID and varTme is mandatory \\(i\\.e\\., they can't be empty\\)\\.")
    expect_error(long2wide_omv(dtaInp = dtaTmp, fleOut = nmeOut, varID = "Year", varTme = c("Month", "Month2")),
      regexp = "^The variable\\(s\\) .* are not contained in the current data set\\.")

    unlink(nmeInp)
})
