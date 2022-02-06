test_that("globals work", {
    expect_equal(adjArg("read_omv", list(fleInp = "Trial_dflArg.omv", getSyn = TRUE, getHTM = TRUE),
                                    list(fleInp = "Trial_varArg.omv", sveAtt = FALSE, getSyn = FALSE, getHTM = FALSE),
                                    fxdArg = c("fleInp", "getSyn")),
                 list(fleInp = "Trial_dflArg.omv", getSyn = TRUE, sveAtt = FALSE, getHTM = FALSE))
    expect_equal(fcnArg(c("merge", "data.frame")), c("x", "y", "by", "by.x", "by.y", "all", "all.x", "all.y", "sort", "suffixes", "no.dups", "incomparables", "..."))
    expect_true(chkDir(system.file("data", "ToothGrowth.rda", package = "jmvReadWrite")))
    expect_true(chkDtF(ToothGrowth, minSze = c(20, 2)))
    expect_true(chkExt(system.file("data", "ToothGrowth.rda", package = "jmvReadWrite"), vldExt))
    expect_true(chkFle(system.file("extdata", "ToothGrowth.omv", package = "jmvReadWrite"), isZIP = TRUE))
    expect_true(chkFle(system.file("extdata", "ToothGrowth.omv", package = "jmvReadWrite"), fleCnt = "meta"))
    expect_true(chkVar(ToothGrowth, c("len", "dose", "supp")))
})
