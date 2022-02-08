test_that("globals work", {
    nmeOMV = paste0(tempfile(), ".omv");
    write_omv(setNames(as.data.frame(matrix(runif(200, -10, 10), ncol = 8)), paste0("X_", sprintf("%02d", 1:8))), nmeOMV);    
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
})
