test_that("long2wide_omv works", {
    set.seed(1)
    dtaTmp <- data.frame(Year = sort(rep(1900:2020, 12)), Month = rep(month.abb[1:12], 121), X = runif(n = 121 * 12, 0, 100), Y = runif(n = 121 * 12, 0, 100))
    attr(dtaTmp[["X"]], "jmv-desc") <- "Variable X"
    nmeInp <- paste0(tempfile(), ".rds")
    nmeOut <- gsub(".rds", "_W.omv", nmeInp)
    saveRDS(dtaTmp, nmeInp)

    expect_null(long2wide_omv(dtaInp = nmeInp, fleOut = nmeOut, varID = "Year", varTme = "Month", varSep = "_"))
    df4Chk <- read_omv(nmeOut)
    expect_s3_class(df4Chk, "data.frame")
    expect_equal(dim(df4Chk), c(121, 25))
    expect_equal(as.vector(sapply(df4Chk, typeof)), c("integer", rep("double", 24)))
    expect_equal(names(df4Chk), c("Year", paste0(c("X_", "Y_"), month.abb[sort(rep(1:12, 2))])))
    unlink(nmeOut)

    expect_null(long2wide_omv(dtaInp = dtaTmp, fleOut = nmeOut, varID = "Year", varTme = "Month", varSep = "_"))
    df4Chk <- read_omv(nmeOut)
    expect_s3_class(df4Chk, "data.frame")
    expect_equal(dim(df4Chk), c(121, 25))
    expect_equal(as.vector(sapply(df4Chk, typeof)), c("integer", rep("double", 24)))
    expect_equal(names(df4Chk), c("Year", paste0(c("X_", "Y_"), month.abb[sort(rep(1:12, 2))])))
    unlink(nmeOut)

    expect_null(long2wide_omv(dtaInp = nmeInp, fleOut = nmeOut, varID = "Year", varTme = "Month", varTgt = c("X"), varSep = "_"))
    df4Chk <- read_omv(nmeOut)
    expect_s3_class(df4Chk, "data.frame")
    expect_equal(dim(df4Chk), c(121, 13))
    expect_equal(as.vector(sapply(df4Chk, typeof)), c("integer", rep("double", 12)))
    expect_equal(names(df4Chk), c("Year", paste0("X_", month.abb[1:12])))
    expect_equal(unname(colMeans(df4Chk[2:13])), c(51.05398, 51.52200, 50.90146, 47.98040, 46.28997, 53.70601, 49.47946, 49.24704, 49.92602, 44.93970, 49.37357, 47.55488), tolerance = 1e-4)
    unlink(nmeOut)

    expect_null(long2wide_omv(dtaInp = nmeInp, fleOut = nmeOut, varID = "Year", varTme = "Month", varTgt = c("Y"), varSep = "_"))
    df4Chk <- read_omv(nmeOut)
    expect_s3_class(df4Chk, "data.frame")
    expect_equal(dim(df4Chk), c(121, 13))
    expect_equal(as.vector(sapply(df4Chk, typeof)), c("integer", rep("double", 12)))
    expect_equal(names(df4Chk), c("Year", paste0("Y_", month.abb[1:12])))
    unlink(nmeOut)

    expect_null(long2wide_omv(dtaInp = nmeInp, fleOut = nmeOut, varID = "Year", varTme = "Month", varSep = "_", varOrd = "vars"))
    df4Chk <- read_omv(nmeOut)
    expect_s3_class(df4Chk, "data.frame")
    expect_equal(dim(df4Chk), c(121, 25))
    expect_equal(as.vector(sapply(df4Chk, typeof)), c("integer", rep("double", 24)))
    expect_equal(names(df4Chk), c("Year", paste0("X_", month.abb[1:12]), paste0("Y_", month.abb[1:12])))
    expect_equal(unname(colMeans(df4Chk[2:25])), c(51.05398, 51.52200, 50.90146, 47.98040, 46.28997, 53.70601, 49.47946, 49.24704, 49.92602, 44.93970, 49.37357, 47.55488,
                                                   48.56846, 48.96117, 47.64545, 46.51572, 50.94652, 47.33624, 47.53437, 55.55701, 51.50431, 50.19580, 52.81145, 43.68338), tolerance = 1e-4)
    unlink(nmeOut)

    df4Chk <- long2wide_omv(dtaInp = nmeInp, varID = "Year", varTme = "Month", varSep = "_")
    expect_s3_class(df4Chk, "data.frame")
    expect_equal(dim(df4Chk), c(121, 25))
    expect_equal(as.vector(sapply(df4Chk, typeof)), c("integer", rep("double", 24)))
    expect_equal(names(df4Chk), c("Year", paste0(c("X_", "Y_"), month.abb[sort(rep(1:12, 2))])))

    set.seed(1)
    dtaTmp <- data.frame(ID = rep(sprintf("sbj_%03d", seq(1, 100)), each = 24), sex = rep(sample(rep(c("male", "female"), each = 50)), each = 24),
                         cond = rep(rep(c("cong", "incong", "neutral"), each = 8), times = 100), color = rep(c("RED", "GREEN", "BLUE", "YELLOW"), times = 600),
                         rep = rep(rep(c(1, 2), each = 4), times = 300), rspCrr = as.integer(runif(2400, 0, 1) > rep(rep(c(0.002, 0.03, 0.02), each = 8), times = 100)),
                         rspTme = rep(rep(c(0.450, 0.530, 0.510), each = 8), times = 100) + rnorm(2400) * 0.05)[sample(2400), ]
    dtaTmp <- dtaTmp[order(dtaTmp$ID, dtaTmp$rep), ]
    df4Chk <- long2wide_omv(dtaInp = dtaTmp, varID = "ID", varTme = c("cond", "color", "rep"), varTgt = c("rspCrr", "rspTme"), varExc = "sex", varSep = "_")
    expect_s3_class(df4Chk, "data.frame")
    expect_equal(dim(df4Chk), c(100, 50))
    expect_equal(names(df4Chk), c("ID", "sex", sprintf("%s_%s_%s_%d", c("rspCrr", "rspTme"), rep(rep(c("neutral", "cong", "incong"), each = 2), times = 8),
                                                       rep(c("RED", "BLUE", "GREEN", "YELLOW"), each = 6), rep(1:2, each = 24))))
    expect_equal(unname(colMeans(df4Chk[, seq(3, 50, 2)])),
      c(0.97, 1.00, 0.96, 0.97, 1.00, 0.97, 1.00, 1.00, 0.95, 0.98, 1.00, 0.98, 0.97, 0.99, 0.95, 0.98, 0.99, 0.98, 1.00, 1.00, 0.95, 0.99, 1.00, 0.97))


    # test cases for code coverage ============================================================================================================================
    expect_error(long2wide_omv(fleInp = nmeInp, varID = "Year", varTme = "Month", varSep = "_"), regexp = "Please use the argument dtaInp instead of fleInp\\.")

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
