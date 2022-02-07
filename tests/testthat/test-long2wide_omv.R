test_that("long2wide_omv works", {
    dtaTmp <- rbind(ts2l(AirPassengers), ts2l(ldeaths), ts2l(co2), ts2l(nottem), ts2l(UKDriverDeaths), ts2l(USAccDeaths));
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
    expect_equal(unname(colMeans(dtaFrm[2:13])), c(462.9771, 456.3822, 450.5623, 410.6177, 373.1371, 364.2978, 366.6525, 352.9341, 341.5625, 358.7452, 367.4564, 424.7346), tolerance = 1e-4);
 
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
    expect_equal(unname(colMeans(dtaFrm[2:13])), c(462.9771, 456.3822, 450.5623, 410.6177, 373.1371, 364.2978, 366.6525, 352.9341, 341.5625, 358.7452, 367.4564, 424.7346), tolerance = 1e-4);
})
