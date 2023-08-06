test_that("write_omv works", {
    # check whether writing the data is working (file existence, size, contents [.omv-files are ZIP archives and must contain files that include meta, metadata.json, data.bin])
    dtaOut <- jmvReadWrite::ToothGrowth
    colOut <- dim(dtaOut)[1]
    nmeOut <- tempfile(fileext = ".omv")
    dtaDbg <- write_omv(dtaFrm = dtaOut, fleOut = nmeOut, retDbg = TRUE)
    expect_true(file.exists(nmeOut))
    expect_gt(file.info(nmeOut)$size, 1)
    expect_true(chkFle(nmeOut, isZIP = TRUE))
    expect_true(chkFle(nmeOut, fleCnt = "meta"))
    expect_true(chkFle(nmeOut, fleCnt = "metadata.json"))
    expect_true(chkFle(nmeOut, fleCnt = "data.bin"))
    unlink(nmeOut)

    # check the debugging information: name and type of the three parts that are returned, content of the metadata, whether all entries in xtdDta are labels, and
    # whether dtaFrm as a data frame with the correct sizes and attributes
    expect_equal(names(dtaDbg),                       c("mtaDta", "xtdDta", "dtaFrm"))
    expect_equal(as.character(sapply(dtaDbg, class)), c("list",   "list",   "data.frame"))
    expect_equal(names(dtaDbg$mtaDta), c("rowCount", "columnCount", "removedRows", "addedRows", "fields", "transforms", "weights"))
    expect_true(all(grepl("labels", sapply(dtaDbg$xtdDta, attributes))))
    expect_s3_class(dtaDbg$dtaFrm, "data.frame")
    expect_equal(dim(dtaDbg$dtaFrm), c(60, 7))
    expect_equal(names(attributes(dtaDbg$dtaFrm)), c("names", "row.names", "class"))
    expect_equal(names(attributes(dtaDbg$dtaFrm[[3]])), c("levels", "class", "description"))
    expect_equal(names(attributes(dtaDbg$dtaFrm[[7]])), c("jmv-desc"))
    expect_equal(attributes(dtaDbg$dtaFrm[[4]]), NULL)
    expect_equal(sapply(jmvReadWrite::ToothGrowth, class), sapply(dtaDbg$dtaFrm, class))

    # test cases for code coverage ============================================================================================================================
    expect_error(write_omv(NULL, nmeOut), regexp = "^The data frame to be written needs to be given as parameter \\(dtaFrm = \\.\\.\\.\\)\\.")
    expect_error(write_omv(data.frame(T1 = sample(9999, 100), T2 = as.complex(rnorm(100))), nmeOut),
      regexp = "Variable type \\w+ not implemented\\. Please send the data file that caused this problem to sebastian\\.jentschke@uib\\.no")
    expect_error(write_omv(dtaDbg$dtaFrm), regexp = "^Output file name needs to be given as parameter \\(fleOut = \\.\\.\\.\\)\\.")
    expect_error(add2ZIP(fleZIP = nmeOut, crrHdl = NULL),
      regexp = "fleZIP \\(a character with a file name\\), and either crrHdl \\(with a connection\\) or crrFle \\(with a file name and \\[optionally\\] a writing mode\\) need to be given as arguments.")
    expect_error(add2ZIP(fleZIP = nmeOut, crrHdl = 1),
      regexp = "^Parameter isn't a file handle pointing to a file to be zipped\\:")

    attr(dtaDbg$dtaFrm, "label.table") <- c("A", "B", "C")
    expect_error(write_omv(dtaDbg$dtaFrm, nmeOut),
      regexp = "^R-foreign-style value labels need to be implemented\\. Please send the data file that caused this problem to sebastian\\.jentschke@uib\\.no")
    attr(dtaDbg$dtaFrm, "label.table") <- NULL

    attr(dtaDbg$dtaFrm, "variable.labels") <- stats::setNames(c("Label for ID", "Label for supp", "Label for supp2"), c("ID", "supp", "supp2"))
    dtaDbg$dtaFrm$supp <- as.character(dtaDbg$dtaFrm$supp)
    expect_equal(sapply(c(1, 3), function(n) write_omv(dtaDbg$dtaFrm, nmeOut, frcWrt = TRUE, retDbg = TRUE)[["mtaDta"]][["fields"]][[n]][["description"]]),
      c("Label for ID", "Label for supp2"))
    unlink(nmeOut)
    expect_identical(sapply(c("dataType", "type"), function(f) write_omv(dtaDbg$dtaFrm, nmeOut, frcWrt = TRUE, retDbg = TRUE)[["mtaDta"]][["fields"]][[2]][[f]], USE.NAMES = FALSE),
      c("Text", "integer"))
    unlink(nmeOut)

    expect_error(write_omv(data.frame(T1 = sample(9999, 100), T2 = as.complex(rnorm(100))), fleOut = tempfile(fileext = ".omv")),
      regexp = "Variable type complex not implemented\\. Please send the data file that caused this problem to sebastian\\.jentschke@uib\\.no")
    expect_equal(write_omv(data.frame(ID = as.factor(seq(100)), T1 = sample(9999, 100), T2 = rnorm(100)), nmeOut, retDbg = TRUE)$mtaDta$field[[1]]$type, "string")
    unlink(nmeOut)

    tmpDF <- read_omv(file.path("..", "ToothGrowth.omv"))
    attr(tmpDF[[4]], "values") <- as.integer(c(0, 1))
    expect_error(write_omv(dtaFrm = tmpDF, fleOut = nmeOut),
      regexp = "\"values\"-attribute with unexpected values found for column \"supp - Transform 1\"\\. Please send the file to sebastian\\.jentschke@uib\\.no for debugging\\.")
    attr(tmpDF[[4]], "values") <- as.integer(c(1, 2))
    expect_null(write_omv(tmpDF, nmeOut))
    unlink(nmeOut)

    set.seed(1)
    dtaOut <- cbind(dtaOut, data.frame(Bool = sample(c(TRUE, FALSE), colOut, TRUE),
                                       Date = sample(seq(as.Date("1999/01/01"), as.Date("2000/01/01"), by = "day"), colOut),
                                       Time = sample(as.difftime(tim = seq(0, 3600), units = "secs"), colOut)))
    write_omv(dtaFrm = dtaOut, fleOut = nmeOut)
    dtaInp <- read_omv(fleInp = nmeOut)
    unlink(nmeOut)
    expect_equal(as.integer(table(dtaInp[["Bool"]])), c(29, 31))
    expect_equal(c(mean(dtaInp[["Date"]]), sd(dtaInp[["Date"]])), c(10787.3667, 108.7002), tolerance = 1e-4)
    expect_equal(attr(dtaInp[["Date"]], "jmv-desc"), "Date (date converted to numeric; days since 1970-01-01)")
    expect_equal(c(mean(dtaInp[["Time"]]), sd(dtaInp[["Time"]])), c(1538.367, 1041.579), tolerance = 1e-4)
    expect_equal(attr(dtaInp[["Time"]], "jmv-desc"), "Time (time converted to numeric; sec since 00:00)")
    dtaOut <- read_omv(file.path("..", "ToothGrowth.omv"))
    attr(dtaOut, "jmv-weights-name") <- "weights"
    attr(dtaOut, "jmv-weights") <- as.vector(dtaOut[, "weights"])
    expect_warning(dtaDbg <- write_omv(dtaFrm = dtaOut, fleOut = nmeOut, retDbg = TRUE), regexp = "Handling of weights not yet implemented\\.")
    expect_equal(dtaDbg$mtaDta$weights, "weights")
    # this actually tests read_omv
    dtaInp <- read_omv(nmeOut)
    unlink(nmeOut)
    expect_equal(names(attributes(dtaInp)), c("names", "row.names", "class", "fltLst", "removedRows", "addedRows", "transforms", "jmv-weights-name", "jmv-weights"))
    expect_equal(attr(dtaInp, "jmv-weights-name"), "weights")
    expect_equal(attr(dtaInp, "jmv-weights"), rep(1, 60))

    dtaOut <- jmvReadWrite::ToothGrowth[, 1, drop = FALSE]
    dtaOut[c(5, 7, 10), 1] <- as.character(NA)
    write_omv(dtaFrm = dtaOut, fleOut = nmeOut)
    expect_true(chkFle(nmeOut, isZIP = TRUE))
    expect_true(chkFle(nmeOut, fleCnt = "strings.bin"))
    unlink(nmeOut)
    dtaOut[, 1] <- as.character(NA)
    attr(dtaOut[, 1], "jmv-id") <- TRUE
    write_omv(dtaFrm = dtaOut, fleOut = nmeOut)
    expect_true(chkFle(nmeOut, isZIP = TRUE))
    expect_error(chkFle(nmeOut, fleCnt = "strings.bin"), regexp = "chkFle: File \".*?\" doesn't contain the file \"strings.bin\".")
    unlink(nmeOut)

    Sys.setenv(JAMOVI_R_VERSION = paste0(R.version$major, ".", R.version$minor))
    df4Chk <- write_omv(dtaFrm = jmvReadWrite::ToothGrowth, fleOut = nmeOut, retDbg = TRUE)$dtaFrm
    Sys.unsetenv("JAMOVI_R_VERSION")
    expect_true(all(sapply(df4Chk, function(x) identical(c("measureType", "dataType") %in% names(attributes(x)), c(TRUE, TRUE)))))
    expect_equal(unname(sapply(df4Chk, function(x) attr(x, "dataType"))), c("Text", "Text", "Text", "Decimal", "Text", "Decimal", "Decimal"))
    expect_equal(unname(sapply(df4Chk, function(x) attr(x, "measureType"))), c("ID", "Nominal", "Nominal", "Continuous", "Ordinal", "Continuous", "Continuous"))
    # do not unlink to provoke the error underneath
    expect_error(write_omv(dtaFrm = jmvReadWrite::ToothGrowth, fleOut = nmeOut),
      regexp = "The output file already exists\\. Either remove the file or set the parameter frcWrt to TRUE\\.")
    unlink(nmeOut)

    dtaOut <- jmvReadWrite::ToothGrowth
    expect_error(write_omv(dtaFrm = dtaOut, fleOut = nmeOut, wrtPtB = TRUE),
      regexp = "The data frame \\(dtaFrm\\) must contain the attribute \"protobuf\", there has to be at least one of them, and it has to be of the correct type \\(a RProtoBuf\\)\\.")
    attr(dtaOut, "protobuf")[["01 empty/analysis"]] <- TRUE
    expect_error(write_omv(dtaFrm = dtaOut, fleOut = nmeOut, wrtPtB = TRUE),
      regexp = "The data frame \\(dtaFrm\\) must contain the attribute \"protobuf\", there has to be at least one of them, and it has to be of the correct type \\(a RProtoBuf\\)\\.")
    attr(dtaOut, "protobuf")[["01 empty/analysis"]] <- RProtoBuf::new(jamovi.coms.AnalysisRequest)
    write_omv(dtaFrm = dtaOut, fleOut = nmeOut, wrtPtB = TRUE)
    expect_true(chkFle(nmeOut, isZIP = TRUE))
    expect_true(chkFle(nmeOut, fleCnt = "01 empty/analysis"))
    unlink(nmeOut)
})
