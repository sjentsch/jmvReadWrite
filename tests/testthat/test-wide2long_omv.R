test_that("wide2long_omv works", {
    # rather simple data set ==================================================================================================================================
    set.seed(1)
    dtaTmp <- cbind(data.frame(Year = 1900:2020), as.data.frame(matrix(runif(121 * 12, 0, 100), nrow = 121, dimnames = list(1:121, paste0("X_", month.abb[1:12])))))
    for (i in 1:12) attr(dtaTmp[[i + 1]], "jmv-desc") <- paste0("Test variable (Month: ", month.abb[i], ")")
    nmeInp <- paste0(tempfile(), ".rds")
    nmeOut <- gsub(".rds", "_L.omv", nmeInp)
    saveRDS(dtaTmp, nmeInp)

    expect_null(wide2long_omv(dtaInp = nmeInp, fleOut = nmeOut, varLst = setdiff(names(dtaTmp), "Year"), varID = "Year", varTme = "Month", varSep = "_"))
    df4Chk <- read_omv(nmeOut)
    expect_s3_class(df4Chk, "data.frame")
    expect_equal(dim(df4Chk), c(1452, 3))
    expect_equal(as.vector(sapply(df4Chk, typeof)), c("integer", "integer", "double"))
    expect_equal(names(df4Chk), c("Year", "Month", "X"))
    expect_equal(names(attributes(df4Chk)), c("names", "row.names", "class", "removedRows", "addedRows", "transforms"))
    expect_equal(names(attributes(df4Chk[[3]])), c("jmv-desc", "name", "id", "columnType", "dataType", "measureType", "formula", "formulaMessage",
                                                   "parentId", "width", "type", "importName", "description", "transform", "edits", "missingValues"))
    expect_equal(attr(df4Chk[[3]], "jmv-desc"), "Test variable")
    expect_equal(c(mean(df4Chk[[3]]), sd(df4Chk[[3]])), c(49.33121, 28.93480), tolerance = 1e-4)
    unlink(nmeOut)

    expect_null(wide2long_omv(dtaInp = nmeInp, fleOut = nmeOut, varLst = setdiff(names(dtaTmp), "Year"), varID = "Year", varTme = "Month", varSep = "_"))
    df4Chk <- read_omv(nmeOut)
    expect_s3_class(df4Chk, "data.frame")
    expect_equal(dim(df4Chk), c(1452, 3))
    expect_equal(as.vector(sapply(df4Chk, typeof)), c("integer", "integer", "double"))
    expect_equal(names(attributes(df4Chk)), c("names", "row.names", "class", "removedRows", "addedRows", "transforms"))
    expect_equal(names(attributes(df4Chk[[3]])), c("jmv-desc", "name", "id", "columnType", "dataType", "measureType", "formula", "formulaMessage",
                                                   "parentId", "width", "type", "importName", "description", "transform", "edits", "missingValues"))
    expect_equal(attr(df4Chk[[3]], "jmv-desc"), "Test variable")
    expect_equal(c(mean(df4Chk[[3]]), sd(df4Chk[[3]])), c(49.33121, 28.93480), tolerance = 1e-4)
    unlink(nmeOut)

    expect_null(wide2long_omv(dtaInp = nmeInp, fleOut = nmeOut, varLst = setdiff(names(dtaTmp), "Year"), varID = "Year", varTme = "Month", varSrt = c("Year", "Month")))
    df4Chk <- read_omv(nmeOut)
    expect_s3_class(df4Chk, "data.frame")
    expect_equal(dim(df4Chk), c(1452, 3))
    expect_false(is.unsorted(df4Chk[["Year"]]))
    expect_true(all(df4Chk[["Month"]] == rep(sort(month.abb), length(unique(df4Chk[["Year"]])))))
    unlink(nmeOut)

    df4Chk <- wide2long_omv(dtaInp = nmeInp, varLst = setdiff(names(dtaTmp), "Year"), varID = "Year", varTme = "Month", varSrt = c("Year", "Month"))
    expect_s3_class(df4Chk, "data.frame")
    expect_equal(dim(df4Chk), c(1452, 3))
    expect_false(is.unsorted(df4Chk[["Year"]]))
    expect_true(all(df4Chk[["Month"]] == rep(sort(month.abb), length(unique(df4Chk[["Year"]])))))


    # more complex data set ===================================================================================================================================
    set.seed(1)
    tgtTmp <- c(0.002, 0.03, 0.02, 0.450, 0.530, 0.510)
    dtaTmp <- cbind(data.frame(ID = as.factor(sprintf("sbj_%03d", seq(100))), sex = as.factor(sample(rep(c("female", "male"), 50)))),
              setNames(as.data.frame(matrix(NA, nrow = 100, ncol = 48)), sprintf("%s_%s_%s_%s", rep(c("rspCrr", "rspTme"), each = 24),
                rep(rep(c("cong", "incong", "neutral"), each = 8), times = 2), rep(rep(c("BLUE", "GREEN", "RED", "YELLOW"), each = 2), times = 6),
                rep(c("1", "2"), times = 24))))
    for (i in seq(1, 48))
        if (i <= 24) dtaTmp[i + 2] <- as.integer(runif(100, 0, 1) > tgtTmp[ceiling((i - 0.1) / 8)]) else dtaTmp[i + 2] <- tgtTmp[ceiling((i - 0.1) / 8)] + rnorm(100) * 0.05

    df4Chk <- wide2long_omv(dtaInp = dtaTmp, fleOut = "", varLst = names(dtaTmp)[c(-1, -2)], varExc = "sex", varID = "ID", varTme = "cond", excLvl = 1)
    expect_s3_class(df4Chk, "data.frame")
    expect_equal(dim(df4Chk), c(2400, 7))
    expect_equal(as.vector(sapply(df4Chk, typeof)), c(rep("integer", 6), "double"))
    expect_equal(names(df4Chk), c("ID", "sex", sprintf("cond%d", 1:3), "rspCrr", "rspTme"))
    expect_equal(df4Chk[, "cond1"], as.factor(rep(rep(c("cong", "incong", "neutral"), each = 8), times = 100)))
    expect_equal(df4Chk[, "cond2"], as.factor(rep(rep(c("BLUE", "GREEN", "RED", "YELLOW"), each = 2), times = 300)))
    expect_equal(df4Chk[, "cond3"], as.factor(rep(c("1", "2"), times = 1200)))
    expect_equal(unname(colMeans(dtaTmp[3:50])), as.vector(unlist(aggregate(x = df4Chk[, c("rspCrr", "rspTme")], by = df4Chk[, c("cond3", "cond2", "cond1")], FUN = mean)[4:5])))

    df4Chk <- wide2long_omv(dtaInp = dtaTmp, fleOut = "", varLst = names(dtaTmp)[c(-1, -2)], varExc = "sex", varID = "ID", varTme = "cond")
    expect_s3_class(df4Chk, "data.frame")
    expect_equal(dim(df4Chk), c(4800, 7))
    expect_equal(as.vector(sapply(df4Chk, typeof)), c(rep("integer", 6), "double"))
    expect_equal(names(df4Chk), c("ID", "sex", sprintf("cond%d", 1:4), "measure"))
    expect_equal(df4Chk[, "cond1"], as.factor(rep(rep(c("rspCrr", "rspTme"), each = 24), times = 100)))
    expect_equal(df4Chk[, "cond2"], as.factor(rep(rep(c("cong", "incong", "neutral"), each = 8), times = 200)))
    expect_equal(df4Chk[, "cond3"], as.factor(rep(rep(c("BLUE", "GREEN", "RED", "YELLOW"), each = 2), times = 600)))
    expect_equal(df4Chk[, "cond4"], as.factor(rep(c("1", "2"), times = 2400)))
    expect_equal(unname(colMeans(dtaTmp[3:50])), aggregate(x = df4Chk[, "measure", drop = FALSE], by = df4Chk[, c("cond4", "cond3", "cond2", "cond1")], FUN = mean)[, 5])


    # test cases for code coverage ============================================================================================================================
    expect_error(wide2long_omv(fleInp = nmeInp, varID = "Year", varTme = "Month", varSep = "_"), regexp = "Please use the argument dtaInp instead of fleInp\\.")
    suppressMessages(expect_error(capture_output(wide2long_omv(dtaInp = nmeInp, fleOut = nmeOut, varTme = "Month", varSep = "_", varSrt = c("Year", "Month"))),
      regexp = "^\\s+The variable separator \\(.*\\) must be contained in all variables in the variable list \\(varLst\\)\\."))
    suppressMessages(expect_error(capture_output(wide2long_omv(dtaInp = nmeInp, fleOut = nmeOut, varSep = "x", varID = "Year")),
      regexp = "^\\s+The variable separator \\(.*\\) must be contained in all variables in the variable list \\(varLst\\)\\."))
#   suppressMessages(expect_error(wide2long_omv(dtaInp = nmeInp, nmeOut, varSep = ".", varID = "Year")))
#   expect_warning(wide2long_omv(dtaInp = nmeInp, nmeOut, varID = "Year", varTme = "Month", varSrt = c("Year", "Month")))

    suppressMessages(expect_error(capture_output(wide2long_omv(dtaInp = data.frame(Year = 1900:2020, X_1 = runif(121), X.2 = runif(121)),
      fleOut = nmeOut, varSep = "_", varID = "Year")),
      regexp = "^\\s+The variable separator \\(.*\\) must be contained in all variables in the variable list \\(varLst\\)\\."))
    suppressMessages(expect_error(capture_output(wide2long_omv(dtaInp = data.frame(Year = 1900:2020, X_1 = runif(121), X_2 = runif(121),
      X_2_A = runif(121)), fleOut = nmeOut, varSep = "_", varID = "Year")),
      regexp = "^The variable names in varLst need to have the same structure, i\\.e\\., the same number of separators within all variable names\\."))
    expect_error(capture_output(wide2long_omv(dtaInp = nmeInp, varID = "Year", varTme = "Month", varSep = "_", excLvl = 0)),
      regexp = "^excLvl must be numeric and must not be less \\(excLvl < 1\\) or more then the number of available levels \\(excLvl > \\d\\)\\.")
    expect_error(capture_output(wide2long_omv(dtaInp = nmeInp, varID = "Year", varTme = "Month", varSep = "_", excLvl = Inf)),
      regexp = "^excLvl must be numeric and must not be less \\(excLvl < 1\\) or more then the number of available levels \\(excLvl > \\d\\)\\.")

    expect_error(ordCol(varNme = c("Year", "Month", "X"), dtaNmV = c(names(dtaTmp), "Trial"), varID = "Year", varLst = names(dtaTmp)[-1]),
      regexp = "^Mismatch between old and new variable order - old: .*; new: .*\\.")
    expect_error(ordCol(varNme = c("Year", "Month", "X"), dtaNmV = names(dtaTmp), varID = c("Year", "Trial"), varLst = names(dtaTmp)[-1]),
      regexp = "^Mismatch between old and new variable order - old: .*; new: .*\\.")


    dtaTmp <- data.frame(ID = as.character(1:121), A = runif(121), B = runif(121), C = runif(121))
    attributes(dtaTmp[[1]]) <- list(`jmv-id` = TRUE, measureType = "ID")
    expect_output(wide2long_omv(dtaInp = dtaTmp, fleOut = nmeOut, varSep = "", varID = "ID"),
                  "Variable list \\(varLst\\) was generated using all variables in the data frame except those defined in varExc or varID \\(ID\\).")
    df4Chk <- read_omv(nmeOut)
    expect_s3_class(df4Chk, "data.frame")
    expect_equal(dim(df4Chk), c(363, 3))
    expect_equal(names(df4Chk), c("ID", "cond", "measure"))
    expect_equal(as.vector(sapply(df4Chk, class)), c("factor", "factor", "numeric"))
    expect_equal(table(df4Chk$cond), table(c(rep("A", 121), rep("B", 121), rep("C", 121))))

    unlink(nmeOut)
    unlink(nmeInp)
})
