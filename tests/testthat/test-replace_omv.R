test_that("replace_omv works", {
    tmpDF <- jmvReadWrite::bfi_sample
    expect_identical(as.list(table(tmpDF$gender)),
                     list(Females = 174L, Males = 80L))
    expect_identical(as.list(table(replace_omv(dtaInp = tmpDF, rplLst = list(c("Females", "Female"), c("Males", "Male")))$gender)),
                     list(Female  = 174L, Male  = 80L))
    expect_identical(as.list(table(replace_omv(dtaInp = tmpDF, rplLst = list(c("females", "Female"), c("males", "Male")))$gender)),
                     list(Females = 174L, Males = 80L))
    expect_identical(which(is.na(replace_omv(dtaInp = tmpDF, rplLst = list(c(24, NA)))$age)),
                     c(2L, 6L, 11L, 12L, 61L, 75L, 77L, 99L, 109L, 169L, 197L, 200L))
    expect_identical(which(is.na(replace_omv(dtaInp = tmpDF, rplLst = list(c(24, NA)))$age)),
                     which(tmpDF$age == 24))
    expect_false(any(replace_omv(dtaInp = tmpDF, rplLst = list(c(24, NA)))$age == 24, na.rm = TRUE))

    tmpDF <- jmvReadWrite::bfi_sample2
    tmpDF <- cbind(tmpDF, data.frame(age_log = log(tmpDF$age)))
    expect_identical(vapply(replace_omv(dtaInp = tmpDF, rplLst = list(c(1, 6)), varExc = c("A1", "A2", "A3", "A4", "A5"))[2:26],
                            function(x) c(sum(x == 1), sum(x == 6)), integer(2), USE.NAMES = FALSE),
                     t(matrix(c(78L, 5L, 8L, 9L, 6L, rep(0L, 20), 9L, 82L, 69L, 106L, 65L, 49L, 55L, 47L, 78L, 75L, 73L,
                                76L, 54L, 68L, 68L, 81L, 65L, 72L, 74L, 94L, 83L, 95L, 54L, 110L, 66L),
                              ncol = 2)))
    expect_identical(vapply(replace_omv(dtaInp = tmpDF, rplLst = list(c(1, 6)), varInc = c("A1", "A2", "A3", "A4", "A5"))[2:26],
                            function(x) c(sum(x == 1), sum(x == 6)), integer(2), USE.NAMES = FALSE),
                     t(matrix(c(rep(0L, 5), 2L, 7L, 7L, 71L, 45L, 52L, 51L, 15L, 14L, 4L, 59L, 30L, 42L, 41L, 64L, 3L,
                                75L, 4L, 4L, 60L, 87L, 87L, 77L, 115L, 71L, 47L, 48L, 40L, 7L, 30L, 21L, 25L, 39L, 54L,
                                64L, 22L, 35L, 30L, 33L, 30L, 80L, 20L, 50L, 106L, 6L),
                              ncol = 2)))
    expect_identical(jmvReadWrite::replace_omv(dtaInp = tmpDF, rplLst = list(c("66574", "Trial")))[244, "ID"],  "Trial")
    expect_identical(which(jmvReadWrite::replace_omv(dtaInp = tmpDF, rplLst = list(c("66574", "Trial"))) != tmpDF),  244L)
    expect_identical(jmvReadWrite::replace_omv(dtaInp = tmpDF, rplLst = list(c("A7209", "Trial")))[244, "ID2"], "Trial")
    expect_identical(which(jmvReadWrite::replace_omv(dtaInp = tmpDF, rplLst = list(c("A7209", "Trial"))) != tmpDF), 7244L)
    expect_identical(unname(colSums(jmvReadWrite::replace_omv(dtaInp = tmpDF, rplLst = list(c("4.", "0.")), whlTrm = FALSE) != tmpDF)),
                     c(rep(0, 29), 5))
    expect_identical(mean(jmvReadWrite::replace_omv(dtaInp = tmpDF, rplLst = list(c("4.", "0.")), whlTrm = FALSE)[, "age_log"]),
                     3.1970426824419937)

    expect_warning(expect_identical(vapply(replace_omv(dtaInp = tmpDF, rplLst = list(c(1, 6)),
                                                       varInc = c("A1", "A2", "A3", "A4", "A5"), varExc = "ID")[2:26],
                                           function(x) c(sum(x == 1), sum(x == 6)), integer(2), USE.NAMES = FALSE),
                                    t(matrix(c(rep(0L, 5), 2L, 7L, 7L, 71L, 45L, 52L, 51L, 15L, 14L, 4L, 59L, 30L, 42L, 41L,
                                               64L, 3L, 75L, 4L, 4L, 60L, 87L, 87L, 77L, 115L, 71L, 47L, 48L, 40L, 7L, 30L,
                                               21L, 25L, 39L, 54L, 64L, 22L, 35L, 30L, 33L, 30L, 80L, 20L, 50L, 106L, 6L),
                                             ncol = 2))),
                   "Both varInc and varExc are given, varInc takes precedence\\.")

    expect_error(replace_omv(dtaInp = tmpDF),
                 "^Calling replace_omv requires the parameter rplLst \\(a list where each entry is a vector with length 2; see Details in help\\)\\.")
    expect_error(replace_omv(dtaInp = tmpDF, rplLst = NULL),
                 "^Calling replace_omv requires the parameter rplLst \\(a list where each entry is a vector with length 2; see Details in help\\)\\.")
    expect_error(replace_omv(dtaInp = tmpDF, rplLst = list(c(1, 2), c(3, 4, 5), c(6, 7))),
                 "^Calling replace_omv requires the parameter rplLst \\(a list where each entry is a vector with length 2; see Details in help\\)\\.")
    expect_error(replace_omv(fleInp = tmpDF, rplLst = list(c(1, 2))),
                 "^Please use the argument dtaInp instead of fleInp\\.")
    expect_error(replace_omv(dtaInp = cbind(tmpDF, data.frame(AD2 = as.complex(tmpDF$age))), rplLst = list(c(24, NA))),
                 "Variable type \\w+ not implemented.")
    expect_error(replace_omv(dtaInp = tmpDF, rplLst = list(c(1, 6)), varInc = "not_a_var"),
                 "^All variables in varInc must be contained in the original data set \\(\\w+ are not\\)\\.")
    expect_error(replace_omv(dtaInp = tmpDF, rplLst = list(c(1, 6)), varExc = "not_a_var"),
                 "^All variables in varExc must be contained in the original data set \\(\\w+ are not\\)\\.")

    rm(tmpDF)
})
