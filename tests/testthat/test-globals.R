test_that("globals work", {
    nmeOMV <- tempfile(fileext = ".omv")
    write_omv(stats::setNames(as.data.frame(matrix(runif(200, -10, 10), ncol = 8)), paste0("X_", sprintf("%02d", 1:8))), nmeOMV)
    expect_equal(adjArg("read_omv", list(fleInp = "Trial_dflArg.omv", getSyn = TRUE, getHTM = TRUE),
                                    list(fleInp = "Trial_varArg.omv", sveAtt = FALSE, getSyn = FALSE, getHTM = FALSE),
                                    fxdArg = c("fleInp", "getSyn")),
                 list(fleInp = "Trial_dflArg.omv", getSyn = TRUE, sveAtt = FALSE, getHTM = FALSE))
    expect_equal(fcnArg(c("merge", "data.frame")), c("x", "y", "by", "by.x", "by.y", "all", "all.x", "all.y", "sort", "suffixes", "no.dups", "incomparables", "..."))
    expect_true(chkDir(nmeOMV))
    if (.Platform$OS.type == "unix" && Sys.getenv("NO_CHKDIR") != "true") {
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
    expect_equal(dim(inp2DF(dtaInp = inpNme)), c(60, 16))

    inpDF <- jmvReadWrite::AlbumSales
    attr(inpDF, "fleInp") <- file.path("..", "ToothGrowth.omv")
    expect_error(inp2DF(dtaInp = inpDF), regexp = "")
    df4Chk <- inp2DF(dtaInp = inpDF, minDF = 2, maxDF = 2)
    expect_type(df4Chk, "list")
    expect_length(df4Chk, 2)
    expect_s3_class(df4Chk[[1]], "data.frame")
    expect_s3_class(df4Chk[[2]], "data.frame")
    expect_equal(dim(df4Chk[[1]]), c(200, 5))
    expect_equal(dim(df4Chk[[2]]), c(60, 16))
    expect_equal(names(df4Chk[[1]]), c("selSbj", "Adverts", "Airplay", "Image", "Sales"))
    expect_equal(names(df4Chk[[2]]),
      c("Filter 1", "ID", "logLen", "supp - Transform 1", "len", "supp", "dose", "dose2", "dose3", "Trial", "Residuals", "J", "K", "L", "M", "weights"))
    expect_equal(names(attributes(df4Chk[[1]])), c("datalabel", "names", "var.labels", "class", "row.names", "fleInp"))
    expect_equal(attr(df4Chk[[1]], "fleInp"), file.path("..", "ToothGrowth.omv"))

    inpDF <- rbind(jmvReadWrite::AlbumSales[seq(sample(10)), ] * NA, jmvReadWrite::AlbumSales, jmvReadWrite::AlbumSales[seq(sample(10)), ] * NA)
    df4Chk <- inp2DF(dtaInp = inpDF, rmvEmp = TRUE)
    expect_s3_class(df4Chk, "data.frame")
    expect_equal(dim(df4Chk), c(200, 5))
    expect_equal(names(df4Chk), c("selSbj", "Adverts", "Airplay", "Image", "Sales"))
    expect_equal(names(attributes(df4Chk)), c("names", "row.names", "class"))
    expect_true(all(df4Chk == jmvReadWrite::AlbumSales))

    inpDF <- rbind(jmvReadWrite::AlbumSales[seq(sample(10)), ] * NA, jmvReadWrite::AlbumSales[seq(1, 100), ], jmvReadWrite::AlbumSales[1, ] * NA,
                   jmvReadWrite::AlbumSales[seq(101, 200), ], jmvReadWrite::AlbumSales[seq(sample(10)), ] * NA)
    expect_error(df4Chk <- inp2DF(dtaInp = inpDF, rmvEmp = TRUE),
      regexp = "Empty rows are not permitted execpt from the begin or the end of an input data frame \\(in such case, they are automatically removed\\)\\.")

    Sys.setenv(JAMOVI_R_VERSION = paste0(R.version$major, ".", R.version$minor))
    expect_warning(expect_error(rtnDta(fleOut = "", psvAnl = TRUE),
      regexp = "The position of the jamovi executable could not be determined or it was not found at the determined position\\. Determined position:"),
      regexp = "psvAnl is only possible if fleOut is a file name \\(analyses are not stored in data frames, only in the jamovi files\\)\\.")
    Sys.unsetenv("JAMOVI_R_VERSION")
    expect_null(jmvPth(inpPth = R.home(), strTgt = "not_in_path", bfrTgt = TRUE))

    Sys.setenv(JAMOVI_R_VERSION = paste0(R.version$major, ".", R.version$minor))
    expect_equal(jmvTtl("_arr_col"), "Dataset_arr_col")
    Sys.unsetenv("JAMOVI_R_VERSION")
    expect_equal(jmvTtl("_arr_col"), "")

    set.seed(1)
    tmpDF <- as.data.frame(cor(matrix(rnorm(1000), nrow = 100)))
    expect_error(mtxF2S(tmpDF[-1, ]), regexp = "Input matrix needs to be symmetric.")
    df4Chk <- mtxF2S(tmpDF)
    expect_equal(dim(df4Chk), c(10, 10))
    expect_equal(names(df4Chk), row.names(df4Chk))
    expect_equal(names(df4Chk), sprintf("V%d", seq(10)))
    expect_false(any(is.na(df4Chk)))
    expect_equal(unname(colMeans(df4Chk)), c(0.128183893, 0.096625131, 0.070568445, 0.115919003, 0.139886614, 0.058323693, 0.092441925, 0.106711615, 0.168887776, 0.10403618))

    df4Chk <- mtxF2S(tmpDF, rmvTrU = TRUE)
    expect_equal(dim(df4Chk), c(10, 10))
    expect_equal(names(df4Chk), row.names(df4Chk))
    expect_equal(names(df4Chk), sprintf("V%d", seq(10)))
    expect_identical(as.integer(colSums(is.na(df4Chk))), seq(0, 9))
    expect_equal(unname(colMeans(df4Chk, na.rm = TRUE)), c(0.12818389, 0.10747174, 0.09210481, 0.16532417, 0.17717275, 0.18858648, 0.25467175, 0.34689183, 0.59969889, 1))

    df4Chk <- mtxF2S(tmpDF, rmvDgn = TRUE)
    expect_equal(dim(df4Chk), c(10, 10))
    expect_equal(names(df4Chk), row.names(df4Chk))
    expect_equal(names(df4Chk), sprintf("V%d", seq(10)))
    expect_identical(unname(colSums(is.na(df4Chk))), rep(1, 10))
    expect_equal(unname(colMeans(df4Chk, na.rm = TRUE)), c(0.031315436, -0.003749854, -0.032701727, 0.017687781, 0.044318459, -0.046307008, -0.008397862, 0.007457349, 0.076541973, 0.004484645))

    df4Chk <- mtxF2S(tmpDF, rmvTrU = TRUE, mtxXps = TRUE)
    expect_equal(dim(df4Chk), c(10, 10))
    expect_equal(names(df4Chk), row.names(df4Chk))
    expect_equal(names(df4Chk), sprintf("V%d", seq(10)))
    expect_identical(as.integer(colSums(is.na(df4Chk))), seq(9, 0))
    expect_equal(unname(colMeans(df4Chk, na.rm = TRUE)), c(1, 0.49950284, 0.322948658, 0.250480203, 0.26716593, 0.106717426, 0.129390323, 0.128305082, 0.165497776, 0.10403618))

    df4Chk <- mtxF2S(tmpDF, rmvTrU = TRUE, rmvDgn = TRUE, mtxXps = TRUE)
    expect_equal(dim(df4Chk), c(10, 10))
    expect_equal(names(df4Chk), row.names(df4Chk))
    expect_equal(names(df4Chk), sprintf("V%d", seq(10)))
    expect_identical(as.integer(colSums(is.na(df4Chk))), seq(10, 1))
    expect_equal(unname(colMeans(df4Chk, na.rm = TRUE)), c(NA, -0.0009943199, -0.0155770134, 0.0006402703, 0.0839574127, -0.0719390890, -0.0157112896, 0.0037772361, 0.0611849980, 0.0044846446))

    df4Chk <- mtxF2S(tmpDF, mtxSps = TRUE)
    expect_equal(dim(df4Chk), c(9, 10))
    expect_equal(names(df4Chk), c("Variable", sprintf("V%d", seq(1, 9))))
    expect_equal(row.names(df4Chk), sprintf("V%d", seq(2, 10)))
    expect_identical(unname(colSums(is.na(df4Chk))), c(0, seq(0, 8)))
    expect_equal(unname(colMeans(df4Chk[, -1], na.rm = TRUE)), c(0.031315436, -0.004094296, -0.037594503,  0.026211536, 0.012607297, -0.014266906, 0.006228995, 0.020337746, 0.199397772))
})
