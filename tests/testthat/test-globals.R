test_that("globals work", {
    nmeOMV <- paste0(tempfile(), ".omv")
    write_omv(stats::setNames(as.data.frame(matrix(runif(200, -10, 10), ncol = 8)), paste0("X_", sprintf("%02d", 1:8))), nmeOMV)
    expect_equal(adjArg("read_omv", list(fleInp = "Trial_dflArg.omv", getSyn = TRUE, getHTM = TRUE),
                                    list(fleInp = "Trial_varArg.omv", sveAtt = FALSE, getSyn = FALSE, getHTM = FALSE),
                                    fxdArg = c("fleInp", "getSyn")),
                 list(fleInp = "Trial_dflArg.omv", getSyn = TRUE, sveAtt = FALSE, getHTM = FALSE))
    expect_equal(fcnArg(c("merge", "data.frame")), c("x", "y", "by", "by.x", "by.y", "all", "all.x", "all.y", "sort", "suffixes", "no.dups", "incomparables", "..."))
    expect_true(chkDir(nmeOMV))
    expect_true(chkDtF(jmvReadWrite::ToothGrowth, minSze = c(60, 7)))
    expect_true(chkExt(nmeOMV, vldExt))
    expect_true(chkExt(gsub("omv$", "rds", nmeOMV), vldExt))
    expect_true(chkFle(nmeOMV, isZIP = TRUE))
    expect_true(chkFle(nmeOMV, fleCnt = "meta"))
    expect_true(chkVar(jmvReadWrite::ToothGrowth, c("len", "dose", "supp")))
    expect_error(chkDir(file.path(tempdir(), "not", "file")))
    expect_error(chkFle(NA))
    expect_error(chkFle("", isZIP = 1))
    expect_error(chkFle(tempfile()))
    expect_error(chkDtF(data.frame(A = runif(n = 100)), minSze = 2))
    expect_true(chkExt(tempfile(), ""))
    expect_error(chkExt(tempfile(), ".chk"))
    expect_error(chkFle("no_file"))
    expect_error(chkFle(nmeOMV, fleCnt = "no_file"))
    expect_error(chkVar(dtaFrm = data.frame(A = runif(100)), varNme = c("A", "B")))
    expect_error(fmtFlI(fleInp = tempfile(), minLng = 2))
    expect_error(fmtFlI(fleInp = c(tempfile(), tempfile()), maxLng = 1))
    expect_equal(fmtFlO(fleOut = "", fleInp = "Trial.omv", rplExt = "_adj.omv"), "Trial_adj.omv")
    expect_error(fmtFlO(fleOut = "", fleInp = ""))
    expect_error(fmtFlO(fleOut = "Trial"))
    expect_error(fmtFlO(fleOut = "Trial.rds"))
    expect_error(fmtFlO(fleOut = "", fleInp = "Trial.omv", rplExt = "_adj.csv"))
    expect_error(fcnArg(c("stats::sd", "stats::mean", "C")))
    expect_error(capture.output(setAtt(attLst = "Trial", inpObj = list(), outObj = outObj)))
    expect_equal(names(attributes(rmvAtt(jmvReadWrite::AlbumSales))), c("names", "class", "row.names"))
    expect_equal(setdiff(names(attributes(jmvReadWrite::AlbumSales)), names(attributes(rmvAtt(jmvReadWrite::AlbumSales)))), c("datalabel", "var.labels"))
    unlink(nmeOMV)

    expect_error(inp2DF(dtaInp = 1))
    inpDF <- jmvReadWrite::AlbumSales
    expect_error(inp2DF(dtaInp = inpDF))
    expect_error(inp2DF(dtaInp = inpDF, fleOut = 1))
    attr(inpDF, "fleOut") <- "Trial1.omv"
    expect_equal(basename(attr(inp2DF(dtaInp = inpDF),                        "fleOut")), "Trial1.omv")
    expect_equal(basename(attr(inp2DF(dtaInp = inpDF, fleOut = "Trial2.omv"), "fleOut")), "Trial2.omv")
    inpNme <- file.path("..", "ToothGrowth.omv")
    expect_s3_class(inp2DF(fleInp = inpNme), "data.frame")
    expect_equal(dim(inp2DF(fleInp = inpNme)), c(60, 13))
    expect_equal(basename(attr(inp2DF(dtaInp = inpNme), "fleOut")), "ToothGrowth_chgd.omv")
    expect_equal(basename(attr(inp2DF(fleInp = inpNme), "fleOut")), "ToothGrowth_chgd.omv")
    expect_equal(basename(attr(inp2DF(fleInp = inpNme, sfxOut = "_trial.omv"), "fleOut")), "ToothGrowth_trial.omv")
    expect_equal(basename(attr(inp2DF(fleInp = inpNme, fleOut = "Trial.omv"), "fleOut")),  "Trial.omv")
    expect_equal(basename(attr(inp2DF(fleInp = inpNme, fleOut = "Trial.omv", sfxOut = "_trial.omv"), "fleOut")), "Trial.omv")

    tmpDF <- data.frame(ID = sprintf("P_%04d", sample(9999, 100)), I = as.integer(sample(1e6, 100)), D = rnorm(100),
                        OT = factor(sample(c("low", "middle", "high"), 100, replace = TRUE), levels = c("low", "middle", "high"), ordered = TRUE),
                        ON = factor(sample(seq(7), 100, replace = TRUE), levels = seq(7), ordered = TRUE),
                        NT = factor(sample(c("low", "middle", "high"), 100, replace = TRUE), levels = c("low", "middle", "high")),
                        NN = factor(sample(seq(7), 100, replace = TRUE), levels = seq(7)))
    attr(tmpDF[["ID"]], "jmv-id") <- TRUE
    attr(tmpDF[["ON"]], "values") <- seq(7)
    attr(tmpDF[["NN"]], "values") <- seq(7)
    expect_equal(sapply(sapply(addAtt(tmpDF), attributes), names), list(ID = c("jmv-id", "measureType", "dataType"),
        I = c("measureType", "dataType"), D = c("measureType", "dataType"),
        OT = c("levels", "class", "measureType", "dataType"), ON = c("levels", "class", "values", "measureType", "dataType"),
        NT = c("levels", "class", "measureType", "dataType"), NN = c("levels", "class", "values", "measureType", "dataType")))
    expect_equal(unlist(attributes(addAtt(tmpDF)[["ID"]]), use.names = FALSE), c("TRUE", "ID", "Text"))
    expect_equal(unlist(attributes(addAtt(tmpDF)[["I"]]),  use.names = FALSE), c("Continuous", "Integer"))
    expect_equal(unlist(attributes(addAtt(tmpDF)[["D"]]),  use.names = FALSE), c("Continuous", "Decimal"))
    expect_equal(unlist(attributes(addAtt(tmpDF)[["OT"]]), use.names = FALSE), c("low", "middle", "high", "ordered", "factor", "Ordinal", "Text"))
    expect_equal(unlist(attributes(addAtt(tmpDF)[["ON"]]), use.names = FALSE), c(sprintf("%d", seq(1:7)), "ordered", "factor", sprintf("%d", seq(1:7)), "Ordinal", "Integer"))
    expect_equal(unlist(attributes(addAtt(tmpDF)[["NT"]]), use.names = FALSE), c("low", "middle", "high", "factor", "Nominal", "Text"))
    expect_equal(unlist(attributes(addAtt(tmpDF)[["NN"]]), use.names = FALSE), c(sprintf("%d", seq(1:7)), "factor", sprintf("%d", seq(1:7)), "Nominal", "Integer"))
    expect_error(addAtt("Trial"))
    expect_error(addAtt(data.frame()))
    expect_error(capture.output(addAtt(cbind(tmpDF, data.frame(ER = sample(seq(as.Date("2000/01/01"), as.Date("2019/12/31"), by = "day"), 100))))))
})
