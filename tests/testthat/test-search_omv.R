test_that("search_omv works", {
    tmpDF <- jmvReadWrite::bfi_sample
    expect_equal(search_omv(dtaInp = tmpDF, srcTrm = "A64365", whlTrm = FALSE), list(ID = "185", ID2 = "168"))
    expect_equal(search_omv(dtaInp = tmpDF, srcTrm = "a64365", whlTrm = FALSE), setNames(list(), character(0)))
    expect_equal(search_omv(dtaInp = tmpDF, srcTrm = "a64365", whlTrm = FALSE, ignCse = TRUE), list(ID = "185", ID2 = "168"))
    expect_equal(search_omv(dtaInp = tmpDF, srcTrm = 24, whlTrm = TRUE), list(age = c("2", "6", "11", "12", "61", "75", "77", "99", "109", "169", "197", "200")))
    expect_equal(sapply(search_omv(dtaInp = jmvReadWrite::bfi_sample, srcTrm = 24, whlTrm = FALSE), length), setNames(c(13, 12, 60, 8), c("ID", "age", "AD", "ID2")))
    expect_equal(search_omv(dtaInp = tmpDF, srcTrm = NA,  whlTrm = TRUE),  list(A1 = c("16", "17", "253", "254")))
    expect_equal(search_omv(dtaInp = tmpDF, srcTrm = NA,  whlTrm = FALSE), list(A1 = c("16", "17", "253", "254")))
    set.seed(1)
    tmpDF[sample(254, 10), "AD"] <- Inf
    tmpDF[sample(254, 10), "AD"] <- -Inf
    # whlTrm = TRUE only finds Inf, FALSE finds both Inf and -Inf
    expect_equal(search_omv(dtaInp = tmpDF, srcTrm = Inf, whlTrm = TRUE),  list(AD = c("14", "43", "68", "129", "162", "167", "187", "210", "215", "249")))
    expect_equal(search_omv(dtaInp = tmpDF, srcTrm = Inf, whlTrm = FALSE), list(AD = c("7", "14", "21", "43", "51", "68", "73", "74", "85", "106", "129",
      "162", "167", "182", "187", "210", "215", "225", "238", "249")))
    # NA's are in another column and shouldn't be affected
    expect_equal(search_omv(dtaInp = tmpDF, srcTrm = NA,  whlTrm = TRUE),  list(A1 = c("16", "17", "253", "254")))
    expect_equal(search_omv(dtaInp = tmpDF, srcTrm = NA,  whlTrm = FALSE), list(A1 = c("16", "17", "253", "254")))
    for (i in sample(33, 10)) tmpDF[sample(254, 2), i] <-  NA
    expect_equal(search_omv(dtaInp = tmpDF, srcTrm = NA,  whlTrm = TRUE),  list(A1 = c("16", "17", "44", "121", "253", "254"), A4 = c("33", "84"), C3 = c("70", "163"), C4 = c("70", "87"),
      E3 = c("111", "166"), E4 = c("89", "126"), N5 = c("172", "207"), O4 = c("42", "74"), gender = c("20", "250"), AG = c("148", "156")))

    expect_error(search_omv(fleInp = tmpDF),                       regexp = "^Calling search_omv requires the parameter srcTrm \\(a character vector with length 1\\)\\.")
    expect_error(search_omv(fleInp = tmpDF, srcTrm = c("1", "2")), regexp = "^Calling search_omv requires the parameter srcTrm \\(a character vector with length 1\\)\\.")
    expect_error(search_omv(fleInp = tmpDF, srcTrm = "A64365"),    regexp = "^Please use the argument dtaInp instead of fleInp\\.")
})
