test_that("long2wide_omv works", {
    set.seed(1);
    dtaTmp <- data.frame(Year = sort(rep(1900:2020, 12)), Month = rep(month.abb[1:12], 121), X = runif(n = 121 * 12, 0, 100), Y = runif(n = 121 * 12, 0, 100));
    attr(dtaTmp[["X"]], "jmv-desc") <- "Variable X";
    nmeInp <- paste0(tempfile(), ".rds");
    nmeOut <- gsub(".rds", "_W.omv", nmeInp);
    saveRDS(dtaTmp, nmeInp);

    long2wide_omv(nmeInp, nmeOut, varID = "Year", varTme = "Month");
    dtaFrm <- read_omv(nmeOut);
    expect_s3_class(dtaFrm, "data.frame");
    expect_equal(dim(dtaFrm), c(121, 25));
    expect_equal(as.vector(sapply(dtaFrm, typeof)), c("integer", rep("double", 24)));
    expect_equal(names(dtaFrm), c("Year", paste0(c("X.", "Y."), month.abb[sort(rep(1:12, 2))])));

    long2wide_omv(nmeInp, nmeOut, varID = "Year", varTme = "Month", varTgt = c("X"));
    dtaFrm <- read_omv(nmeOut);
    expect_s3_class(dtaFrm, "data.frame");
    expect_equal(dim(dtaFrm), c(121, 13));
    expect_equal(as.vector(sapply(dtaFrm, typeof)), c("integer", rep("double", 12)));
    expect_equal(names(dtaFrm), c("Year", paste0("X.", month.abb[1:12])));
    expect_equal(unname(colMeans(dtaFrm[2:13])), c(51.05398, 51.52200, 50.90146, 47.98040, 46.28997, 53.70601, 49.47946, 49.24704, 49.92602, 44.93970, 49.37357, 47.55488), tolerance = 1e-4);

    long2wide_omv(nmeInp, nmeOut, varID = "Year", varTme = "Month", varTgt = c("Y"), varSep = "_");
    dtaFrm <- read_omv(nmeOut);
    expect_s3_class(dtaFrm, "data.frame");
    expect_equal(dim(dtaFrm), c(121, 13));
    expect_equal(as.vector(sapply(dtaFrm, typeof)), c("integer", rep("double", 12)));
    expect_equal(names(dtaFrm), c("Year", paste0("Y_", month.abb[1:12])));

    long2wide_omv(nmeInp, nmeOut, varID = "Year", varTme = "Month", varSep = "_", varOrd = "vars");
    dtaFrm <- read_omv(nmeOut);
    expect_s3_class(dtaFrm, "data.frame");
    expect_equal(dim(dtaFrm), c(121, 25));
    expect_equal(as.vector(sapply(dtaFrm, typeof)), c("integer", rep("double", 24)));
    expect_equal(names(dtaFrm), c("Year", paste0("X_", month.abb[1:12]), paste0("Y_", month.abb[1:12])));
    expect_equal(unname(colMeans(dtaFrm[2:25])), c(51.05398, 51.52200, 50.90146, 47.98040, 46.28997, 53.70601, 49.47946, 49.24704, 49.92602, 44.93970, 49.37357, 47.55488,
                                                   48.56846, 48.96117, 47.64545, 46.51572, 50.94652, 47.33624, 47.53437, 55.55701, 51.50431, 50.19580, 52.81145, 43.68338), tolerance = 1e-4);

    # test cases for code coverage ============================================================================================================================
    expect_error(long2wide_omv(fleInp = nmeInp, fleOut = nmeInp, varID = "Year", varTme = ""));
    expect_error(long2wide_omv(fleInp = nmeInp, fleOut = nmeInp, varID = "", varTme = "Month"));
    expect_error(long2wide_omv(fleInp = nmeInp, fleOut = nmeInp, varID = "Year", varTme = c("Month", "Month2")));

    unlink(nmeInp);
    unlink(nmeOut);
})
