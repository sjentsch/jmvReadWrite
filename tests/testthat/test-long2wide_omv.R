test_that("long2wide_omv works", {
    dtaTmp <- rbind(ts2l(datasets::nottem), ts2l(datasets::AirPassengers), ts2l(datasets::co2));
    dtaTmp <- srtFrm(dtaTmp, c("Year", "Month"));
    dtaTmp <- dtaTmp[!duplicated(dtaTmp[, c("Year", "Month")]), ];
    dtaTmp <- cbind(dtaTmp, list(Y = sample(dtaTmp[["X"]], size = dim(dtaTmp)[1])));
    nmeInp <- paste0(tempfile(), ".rds");
    nmeOut <- gsub(".rds", "_W.omv", nmeInp);
    saveRDS(dtaTmp, nmeInp);

    long2wide_omv(nmeInp, nmeOut, varID = "Year", varTme = "Month");
    dtaFrm <- read_omv(nmeOut);
    expect_s3_class(dtaFrm, "data.frame");
    expect_equal(dim(dtaFrm), c(69, 25));
    expect_equal(as.vector(sapply(dtaFrm, typeof)), c("integer", rep("double", 24)));
    expect_equal(names(dtaFrm), c("Year", paste0(c("X.", "Y."), sort(rep(1:12, 2)))));

    long2wide_omv(nmeInp, nmeOut, varID = "Year", varTme = "Month", varTgt = c("X"));
    dtaFrm <- read_omv(nmeOut);
    expect_s3_class(dtaFrm, "data.frame");
    expect_equal(dim(dtaFrm), c(69, 13));
    expect_equal(as.vector(sapply(dtaFrm, typeof)), c("integer", rep("double", 12)));
    expect_equal(names(dtaFrm), c("Year", paste0("X.", 1:12)));
    expect_equal(unname(colMeans(dtaFrm[2:13])), c(234.5509, 233.6465, 241.1030, 242.4181, 245.3710, 253.5822, 260.8455, 259.3532, 248.7883, 240.5020, 233.3239, 238.1483), tolerance = 1e-4);

    long2wide_omv(nmeInp, nmeOut, varID = "Year", varTme = "Month", varTgt = c("Y"), varSep = "_");
    dtaFrm <- read_omv(nmeOut);
    expect_s3_class(dtaFrm, "data.frame");
    expect_equal(dim(dtaFrm), c(69, 13));
    expect_equal(as.vector(sapply(dtaFrm, typeof)), c("integer", rep("double", 12)));
    expect_equal(names(dtaFrm), c("Year", paste0("Y_", 1:12)));

    long2wide_omv(nmeInp, nmeOut, varID = "Year", varTme = "Month", varSep = "_", varOrd = "vars");
    dtaFrm <- read_omv(nmeOut);
    expect_s3_class(dtaFrm, "data.frame");
    expect_equal(dim(dtaFrm), c(69, 25));
    expect_equal(as.vector(sapply(dtaFrm, typeof)), c("integer", rep("double", 24)));
    expect_equal(names(dtaFrm), c("Year", paste0("X_", 1:12), paste0("Y_", 1:12)));
    expect_equal(unname(colMeans(dtaFrm[2:13])), c(234.5509, 233.6465, 241.1030, 242.4181, 245.3710, 253.5822, 260.8455, 259.3532, 248.7883, 240.5020, 233.3239, 238.1483), tolerance = 1e-4);
})
