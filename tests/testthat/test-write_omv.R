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
    expect_equal(vapply(dtaDbg, class, character(1), USE.NAMES = FALSE), c("list",   "list",   "data.frame"))
    expect_equal(names(dtaDbg$mtaDta), c("rowCount", "columnCount", "removedRows", "addedRows", "fields", "transforms", "weights"))
    expect_true(all(grepl("labels", unlist(lapply(dtaDbg$xtdDta, attributes)))))
    expect_s3_class(dtaDbg$dtaFrm, "data.frame")
    expect_equal(dim(dtaDbg$dtaFrm), c(60, 7))
    expect_equal(names(attributes(dtaDbg$dtaFrm)), c("names", "row.names", "class"))
    expect_equal(names(attributes(dtaDbg$dtaFrm[[3]])), c("levels", "class", "description", "measureType", "dataType"))
    expect_equal(names(attributes(dtaDbg$dtaFrm[[7]])), c("jmv-desc", "measureType", "dataType"))
    expect_equal(names(attributes(dtaDbg$dtaFrm[[4]])), c("measureType", "dataType"))
    expect_equal(lapply(jmvReadWrite::ToothGrowth, class), lapply(dtaDbg$dtaFrm, class))

    # add columns with only NAs to the dataset and attach jamovi-attributes to them, write the resulting data frame
    dtaDbg <- write_omv(dtaFrm = cbind(dtaOut, jmvAtt(data.frame(T1 = rep(as.integer(NA), nrow(dtaOut)), T2 = as.numeric(NA), T3 = factor(NA),
                                                                 T4 = factor(NA, ordered = TRUE), T5 = as.character(NA), T6 = NA))), fleOut = nmeOut, retDbg = TRUE)
    expect_true(file.exists(nmeOut))
    expect_gt(file.info(nmeOut)$size, 1)
    expect_true(chkFle(nmeOut, isZIP = TRUE))
    expect_true(chkFle(nmeOut, fleCnt = "meta"))
    expect_true(chkFle(nmeOut, fleCnt = "metadata.json"))
    expect_true(chkFle(nmeOut, fleCnt = "data.bin"))
    unlink(nmeOut)

    expect_equal(names(dtaDbg),                       c("mtaDta", "xtdDta", "dtaFrm"))
    expect_equal(vapply(dtaDbg, class, character(1), USE.NAMES = FALSE), c("list",   "list",   "data.frame"))
    expect_equal(names(dtaDbg$mtaDta), c("rowCount", "columnCount", "removedRows", "addedRows", "fields", "transforms", "weights"))
    expect_true(all(grepl("labels", unlist(lapply(dtaDbg$xtdDta, attributes)))))
    expect_s3_class(dtaDbg$dtaFrm, "data.frame")
    expect_equal(dim(dtaDbg$dtaFrm), c(60, 13))
    expect_equal(names(attributes(dtaDbg$dtaFrm)), c("names", "row.names", "class"))
    expect_equal(names(attributes(dtaDbg$dtaFrm[[3]])),  c("levels", "class", "description", "measureType", "dataType"))
    expect_equal(names(attributes(dtaDbg$dtaFrm[[7]])),  c("jmv-desc", "measureType", "dataType"))
    expect_equal(names(attributes(dtaDbg$dtaFrm[[10]])), c("class", "levels", "measureType", "dataType"))
    expect_equal(names(attributes(dtaDbg$dtaFrm[[11]])), c("class", "levels", "measureType", "dataType"))
    expect_equal(names(attributes(dtaDbg$dtaFrm[[12]])), c("levels", "class", "measureType", "dataType"))
    expect_equal(names(attributes(dtaDbg$dtaFrm[[13]])), c("levels", "class", "measureType", "dataType"))
    expect_equal(attributes(dtaDbg$dtaFrm[[4]]),  list(measureType = "Continuous", dataType = "Decimal"))
    expect_equal(attributes(dtaDbg$dtaFrm[[8]]),  list(measureType = "Continuous", dataType = "Integer"))
    expect_equal(attributes(dtaDbg$dtaFrm[[9]]),  list(measureType = "Continuous", dataType = "Decimal"))
    expect_equal(attributes(dtaDbg$dtaFrm[[10]]), list(class =              "factor",  levels = character(0), measureType = "Nominal",    dataType = "Integer"))
    expect_equal(attributes(dtaDbg$dtaFrm[[11]]), list(class = c("ordered", "factor"), levels = character(0), measureType = "Ordinal",    dataType = "Integer"))
    expect_equal(attributes(dtaDbg$dtaFrm[[12]]), list(levels = character(0), class = "factor",               measureType = "Nominal",    dataType = "Text"))
    expect_equal(attributes(dtaDbg$dtaFrm[[13]]), list(levels = character(0), class = "factor",               measureType = "Nominal",    dataType = "Integer"))
    expect_equal(c(lapply(jmvReadWrite::ToothGrowth, class), list(T1 = "integer", T2 = "numeric", T3 = "factor", T4 = c("ordered", "factor"), T5 = "factor", T6 = "factor")),
                 lapply(dtaDbg$dtaFrm, class))

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
    expect_equal(vapply(c(1, 3), function(n) write_omv(dtaDbg$dtaFrm, nmeOut, frcWrt = TRUE, retDbg = TRUE)[["mtaDta"]][["fields"]][[n]][["description"]], character(1)),
      c("Label for ID", "Label for supp2"))
    unlink(nmeOut)
    expect_identical(vapply(c("dataType", "type"), function(f) write_omv(dtaDbg$dtaFrm, nmeOut, frcWrt = TRUE, retDbg = TRUE)[["mtaDta"]][["fields"]][[2]][[f]], character(1), USE.NAMES = FALSE),
      c("Text", "integer"))
    unlink(nmeOut)

    expect_error(write_omv(data.frame(T1 = sample(9999, 100), T2 = as.complex(rnorm(100))), fleOut = tempfile(fileext = ".omv")),
      regexp = "Variable type \\w+ not implemented\\. Please send the data file that caused this problem to sebastian\\.jentschke@uib\\.no")
    expect_equal(write_omv(data.frame(ID = as.factor(seq(100)),  T1 = sample(9999, 100), T2 = rnorm(100)), nmeOut, retDbg = TRUE)$mtaDta$field[[1]]$type, "string")
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
    write_omv(dtaFrm = dtaOut, fleOut = nmeOut)
    expect_true(chkFle(nmeOut, isZIP = TRUE))
    expect_true(chkFle(nmeOut, fleCnt = "strings.bin"))
    unlink(nmeOut)
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
    expect_true(all(vapply(df4Chk, function(x) identical(c("measureType", "dataType") %in% names(attributes(x)), c(TRUE, TRUE)), logical(1))))
    expect_equal(vapply(df4Chk, function(x) attr(x, "dataType"),    character(1), USE.NAMES = FALSE), c("Text", "Text", "Integer", "Decimal", "Text", "Decimal", "Decimal"))
    expect_equal(vapply(df4Chk, function(x) attr(x, "measureType"), character(1), USE.NAMES = FALSE), c("ID", "Nominal", "Nominal", "Continuous", "Ordinal", "Continuous", "Continuous"))
    # do not unlink to provoke the error underneath
    expect_error(write_omv(dtaFrm = jmvReadWrite::ToothGrowth, fleOut = nmeOut),
      regexp = "The output file .* already exists\\. Either remove the file or set the parameter frcWrt to TRUE\\.")
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

    tmpDF <- data.frame(ID = sprintf("P_%04d", sample(9999, 100)), I = as.integer(sample(1e6, 100)), D = rnorm(100),
                        OT = factor(sample(c("low", "middle", "high"), 100, replace = TRUE), levels = c("low", "middle", "high"), ordered = TRUE),
                        ON = factor(sample(seq(7), 100, replace = TRUE), levels = seq(7), ordered = TRUE),
                        NT = factor(sample(c("low", "middle", "high"), 100, replace = TRUE), levels = c("low", "middle", "high")),
                        NN = factor(sample(seq(7), 100, replace = TRUE), levels = seq(7)),
                        CR = sample(c("low", "middle", "high"), 100, replace = TRUE))
    attr(tmpDF[["ID"]], "jmv-id")   <- TRUE
    attr(tmpDF[["ON"]], "values")   <- seq(7)
    attr(tmpDF[["NN"]], "values")   <- seq(7)
    attr(tmpDF[["CR"]], "jmv-desc") <- "Trial (is description kept?)"
    expect_equal(lapply(lapply(jmvAtt(tmpDF), attributes), names), list(ID = c("jmv-id", "measureType", "dataType"),
        I = c("measureType", "dataType"), D = c("measureType", "dataType"),
        OT = c("levels", "class", "measureType", "dataType"), ON = c("levels", "class", "values", "measureType", "dataType"),
        NT = c("levels", "class", "measureType", "dataType"), NN = c("levels", "class", "values", "measureType", "dataType"),
        CR = c("jmv-desc", "measureType", "dataType")))
    expect_equal(unlist(attributes(jmvAtt(tmpDF)[["ID"]]), use.names = FALSE), c("TRUE", "ID", "Text"))
    expect_equal(unlist(attributes(jmvAtt(tmpDF)[["I"]]),  use.names = FALSE), c("Continuous", "Integer"))
    expect_equal(unlist(attributes(jmvAtt(tmpDF)[["D"]]),  use.names = FALSE), c("Continuous", "Decimal"))
    expect_equal(unlist(attributes(jmvAtt(tmpDF)[["OT"]]), use.names = FALSE), c("low", "middle", "high", "ordered", "factor", "Ordinal", "Text"))
    expect_equal(unlist(attributes(jmvAtt(tmpDF)[["ON"]]), use.names = FALSE), c(sprintf("%d", seq(1:7)), "ordered", "factor", sprintf("%d", seq(1:7)), "Ordinal", "Integer"))
    expect_equal(unlist(attributes(jmvAtt(tmpDF)[["NT"]]), use.names = FALSE), c("low", "middle", "high", "factor", "Nominal", "Text"))
    expect_equal(unlist(attributes(jmvAtt(tmpDF)[["NN"]]), use.names = FALSE), c(sprintf("%d", seq(1:7)), "factor", sprintf("%d", seq(1:7)), "Nominal", "Integer"))
    expect_equal(unlist(attributes(jmvAtt(tmpDF)[["CR"]]), use.names = FALSE), c("Trial (is description kept?)", "Nominal", "Text"))
    expect_error(jmvAtt("Trial"),      regexp = "^Input data are either not a data frame or have incorrect \\(only one or more than two\\) dimensions\\.")
    expect_error(jmvAtt(data.frame()), regexp = "^The first dimension of the input data frame has not the required size \\(0 < 1\\)\\.")
    expect_error(jmvAtt(cbind(tmpDF, data.frame(EC = sample(as.complex(seq(10)), 100, replace = TRUE)))), regexp = "^\\s+\\w+: Variable type \\w+ not implemented.")

    tmpCol <- rnorm(100)
    attr(tmpCol, "Trial") <- "Check whether attribute is preserved"
    expect_equal(cnvCol(tmpCol, tgtTyp = "integer"), round(tmpCol))
    expect_equal(attr(cnvCol(tmpCol, tgtTyp = "integer"), "Trial"), "Check whether attribute is preserved")
})
