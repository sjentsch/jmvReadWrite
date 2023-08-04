test_that("globals work", {
    nmeOMV <- tempfile(fileext = ".omv")
    write_omv(stats::setNames(as.data.frame(matrix(runif(200, -10, 10), ncol = 8)), paste0("X_", sprintf("%02d", 1:8))), nmeOMV)
    expect_equal(adjArg("read_omv", list(fleInp = "Trial_dflArg.omv", getSyn = TRUE, getHTM = TRUE),
                                    list(fleInp = "Trial_varArg.omv", sveAtt = FALSE, getSyn = FALSE, getHTM = FALSE),
                                    fxdArg = c("fleInp", "getSyn")),
                 list(fleInp = "Trial_dflArg.omv", getSyn = TRUE, sveAtt = FALSE, getHTM = FALSE))
    expect_equal(fcnArg(c("merge", "data.frame")), c("x", "y", "by", "by.x", "by.y", "all", "all.x", "all.y", "sort", "suffixes", "no.dups", "incomparables", "..."))
    expect_true(chkDir(nmeOMV))
    if (.Platform$OS.type == "unix") {
        # permissions on *nix-systems
        dir.create(file.path(tempdir(), "chkPrm"), mode = "0111")
        expect_error(chkDir(file.path(tempdir(), "chkPrm", "Trial.omv")),
          regexp = "The directory \\(.*\\) exists, but you don't have writing permissions in that directory\\.")
        unlink(file.path(tempdir(), "chkPrm"), recursive = TRUE)
    }
    expect_true(chkDtF(jmvReadWrite::ToothGrowth, minSze = c(60, 7)))
    expect_true(chkExt(nmeOMV, vldExt))
    expect_true(chkExt(gsub("omv$", "rds", nmeOMV), vldExt))
    expect_true(chkFle(nmeOMV, isZIP = TRUE))
    expect_true(chkFle(nmeOMV, fleCnt = "meta"))
    expect_true(chkVar(jmvReadWrite::ToothGrowth, c("len", "dose", "supp")))
    expect_error(chkDir(file.path(tempdir(), "not", "file")), regexp = "^Directory \\(.*?\\) doesn't exist\\.")
    expect_error(chkFle(NA),            regexp = "^chkFle: Unsupported input parameter type\\.")
    expect_error(chkFle("", isZIP = 1), regexp = "^chkFle: Unsupported input parameter type\\.")
    expect_error(chkFle(tempfile()), regexp = "^File \".*?\" not found\\.")
    expect_error(chkDtF(data.frame(A = runif(n = 100)), minSze = 2), regexp = "^The \\w+ dimension of the input data frame has not the required size")
    expect_true(chkExt(tempfile(), ""))
    expect_error(chkExt(tempfile(), "chk"), regexp = "^File name \\(.*?\\) contains an unsupported file extension \\(.*?\\)\\.")
    expect_error(chkFle("no_file"), regexp = "^File \".*?\" not found.")
    expect_error(chkFle(nmeOMV, fleCnt = "no_file"), regexp = "^chkFle: File \".*?\" doesn't contain the file \".*?\"\\.")
    expect_error(chkVar(dtaFrm = data.frame(A = runif(100)), varNme = c("A", "B")), regexp = "^The variable\\(s\\) \\w+ are not contained in the current data set\\.")
    expect_error(fmtFlI(fleInp = tempfile(), minLng = 2), regexp = "^The fleInp-argument is supposed to be a character vector with a minimal length of \\d+ and a maximal length of")
    expect_error(fmtFlI(fleInp = c(tempfile(), tempfile()), maxLng = 1),
      regexp = "^The fleInp-argument is supposed to be a character vector with a minimal length of \\d+ and a maximal length of")
    expect_error(fmtFlO(fleOut = ""),          regexp = "^fleOut needs to be a valid non-empty file name \\(character\\), and the file extension for output file needs to be \\.omv\\.")
    expect_error(fmtFlO(fleOut = "Trial"),     regexp = "^fleOut needs to be a valid non-empty file name \\(character\\), and the file extension for output file needs to be \\.omv\\.")
    expect_error(fmtFlO(fleOut = "Trial.rds"), regexp = "^fleOut needs to be a valid non-empty file name \\(character\\), and the file extension for output file needs to be \\.omv\\.")
    expect_error(fcnArg(c("stats::sd", "stats::mean", "C")), regexp = "^The argument to fcnArg must be a character \\(vector\\) with 1 or 2 elements.")
    expect_true(jmvPtB())
    tmpPB <- var2PB(inpVar = list(list(A = NULL, B = TRUE, C = 1, D = 0.01, E = "Trial", F = c(TRUE, FALSE, TRUE), G = c(1, 2, 3), H = c(0.01, 0.02, 0.03), I = c("A", "B"))))
    expect_s4_class(tmpPB, "Message")
    expect_equal(length(tmpPB), 1)
    expect_equal(length(tmpPB$c$options[[1]]$c$options), 9)
    expect_equal(tmpPB$as.character(), paste0("c {\n  options {\n    c {\n      options {\n        o: NONE\n      }\n      options {\n        o: TRUE\n      }\n      options {\n",
                                              "        i: 1\n      }\n      options {\n        d: 0.01\n      }\n      options {\n        s: \"Trial\"\n      }\n      options {\n",
                                              "        c {\n          options {\n            o: TRUE\n          }\n          options {\n            o: FALSE\n          }\n",
                                              "          options {\n            o: TRUE\n          }\n        }\n      }\n      options {\n        c {\n          options {\n",
                                              "            i: 1\n          }\n          options {\n            i: 2\n          }\n          options {\n            i: 3\n",
                                              "          }\n        }\n      }\n      options {\n        c {\n          options {\n            d: 0.01\n          }\n",
                                              "          options {\n            d: 0.02\n          }\n          options {\n            d: 0.03\n          }\n        }\n",
                                              "      }\n      options {\n        c {\n          options {\n            s: \"A\"\n          }\n          options {\n",
                                              "            s: \"B\"\n          }\n        }\n      }\n      hasNames: true\n      names: \"A\"\n      names: \"B\"\n",
                                              "      names: \"C\"\n      names: \"D\"\n      names: \"E\"\n      names: \"F\"\n      names: \"G\"\n      names: \"H\"\n",
                                              "      names: \"I\"\n    }\n  }\n}\n"))
    expect_error(var2PB(inpVar = as.complex(pi)), regexp = "^Element not implemented for conversion to protocol buffer.")
    expect_error(setAtt(attLst = "Trial", inpObj = list(),       outObj = list()),
      regexp = "^Error when storing or accessing meta-data information\\. Please send the file")
    expect_error(setAtt(attLst = "Trial", inpObj = data.frame(), outObj = data.frame()),
      regexp = "^Error when storing or accessing meta-data information\\. Please send the file")
    expect_error(setAtt(attLst = NULL,    inpObj = data.frame(), outObj = data.frame()), regexp = "^setAtt: The parameter attLst is supposed to be a character vector\\.")
    expect_error(setAtt(attLst = "Trial", inpObj = NULL,         outObj = data.frame()), regexp = "^setAtt: The parameter inpObj is supposed to be either a list or a data frame\\.")
    expect_error(setAtt(attLst = "Trial", inpObj = data.frame(), outObj = NULL),         regexp = "^setAtt: The parameter outObj is supposed to be either a list or a data frame\\.")
    expect_equal(names(attributes(rmvAtt(jmvReadWrite::AlbumSales))), c("names", "class", "row.names"))
    expect_equal(setdiff(names(attributes(jmvReadWrite::AlbumSales)), names(attributes(rmvAtt(jmvReadWrite::AlbumSales)))), c("datalabel", "var.labels"))
    unlink(nmeOMV)

    expect_error(inp2DF(dtaInp = 1), regexp = "^dtaInp must either be a data frame or a character \\(pointing to a location where the input file can be found\\)\\.")
    inpDF <- jmvReadWrite::AlbumSales
    inpNme <- file.path("..", "ToothGrowth.omv")
    expect_s3_class(inp2DF(dtaInp = inpNme), "data.frame")
    expect_equal(dim(inp2DF(dtaInp = inpNme)), c(60, 14))

    inpDF <- jmvReadWrite::AlbumSales
    attr(inpDF, "fleInp") <- file.path("..", "ToothGrowth.omv")
    expect_error(inp2DF(dtaInp = inpDF), regexp = "")
    df4Chk <- inp2DF(dtaInp = inpDF, minDF = 2, maxDF = 2)
    expect_type(df4Chk, "list")
    expect_length(df4Chk, 2)
    expect_s3_class(df4Chk[[1]], "data.frame")
    expect_s3_class(df4Chk[[2]], "data.frame")
    expect_equal(dim(df4Chk[[1]]), c(200, 5))
    expect_equal(dim(df4Chk[[2]]), c(60, 14))
    expect_equal(names(df4Chk[[1]]), c("selSbj", "Adverts", "Airplay", "Image", "Sales"))
    expect_equal(names(df4Chk[[2]]), c("Filter 1", "ID", "logLen", "supp - Transform 1", "len", "supp", "dose", "dose2", "Trial", "Residuals", "J", "K", "L", "weights"))
    expect_equal(names(attributes(df4Chk[[1]])), c("datalabel", "names", "var.labels", "class", "row.names", "fleInp"))
    expect_equal(attr(df4Chk[[1]], "fleInp"), file.path("..", "ToothGrowth.omv"))

    Sys.setenv(JAMOVI_R_VERSION = paste0(R.version$major, ".", R.version$minor))
    expect_warning(expect_error(rtnDta(fleOut = "", psvAnl = TRUE),
      regexp = "The position of the jamovi executable could not be determined or it was not found at the determined position\\. Determined position:"),
      regexp = "psvAnl is only possible if fleOut is a file name \\(analyses are not stored in data frames, only in the jamovi files\\)\\.")
    Sys.unsetenv("JAMOVI_R_VERSION")
    expect_null(jmvPth(inpPth = R.home(), strTgt = "not_in_path", bfrTgt = TRUE))

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
    expect_equal(sapply(sapply(jmvAtt(tmpDF), attributes), names), list(ID = c("jmv-id", "measureType", "dataType"),
        I = c("measureType", "dataType"), D = c("measureType", "dataType"),
        OT = c("levels", "class", "measureType", "dataType"), ON = c("levels", "class", "values", "measureType", "dataType"),
        NT = c("levels", "class", "measureType", "dataType"), NN = c("levels", "class", "values", "measureType", "dataType"),
        CR = c("levels", "class", "jmv-desc", "measureType", "dataType")))
    expect_equal(unlist(attributes(jmvAtt(tmpDF)[["ID"]]), use.names = FALSE), c("TRUE", "ID", "Text"))
    expect_equal(unlist(attributes(jmvAtt(tmpDF)[["I"]]),  use.names = FALSE), c("Continuous", "Integer"))
    expect_equal(unlist(attributes(jmvAtt(tmpDF)[["D"]]),  use.names = FALSE), c("Continuous", "Decimal"))
    expect_equal(unlist(attributes(jmvAtt(tmpDF)[["OT"]]), use.names = FALSE), c("low", "middle", "high", "ordered", "factor", "Ordinal", "Text"))
    expect_equal(unlist(attributes(jmvAtt(tmpDF)[["ON"]]), use.names = FALSE), c(sprintf("%d", seq(1:7)), "ordered", "factor", sprintf("%d", seq(1:7)), "Ordinal", "Integer"))
    expect_equal(unlist(attributes(jmvAtt(tmpDF)[["NT"]]), use.names = FALSE), c("low", "middle", "high", "factor", "Nominal", "Text"))
    expect_equal(unlist(attributes(jmvAtt(tmpDF)[["NN"]]), use.names = FALSE), c(sprintf("%d", seq(1:7)), "factor", sprintf("%d", seq(1:7)), "Nominal", "Integer"))
    expect_equal(unlist(attributes(jmvAtt(tmpDF)[["CR"]]), use.names = FALSE), c("high", "low", "middle", "factor", "Trial (is description kept?)", "Nominal", "Text"))
    tmpCR <- tmpDF["CR"]
    attr(tmpCR[["CR"]], "measureType") <- attr(tmpCR[["CR"]], "dataType") <- "Trial"
    expect_equal(attributes(jmvAtt(tmpCR)[["CR"]]), list(`jmv-desc` = "Trial (is description kept?)", dataType = "Trial", measureType = "Trial"))
    expect_error(jmvAtt("Trial"),      regexp = "^Input data are either not a data frame or have incorrect \\(only one or more than two\\) dimensions\\.")
    expect_error(jmvAtt(data.frame()), regexp = "^The first dimension of the input data frame has not the required size \\(0 < 1\\)\\.")
    expect_error(jmvAtt(cbind(tmpDF, data.frame(ER = sample(seq(as.Date("2000/01/01"), as.Date("2019/12/31"), by = "day"), 100)))),
      regexp = "^\\s+\\w+: Variable type \\w+ not implemented:")
})
