test_that("replace_omv works", {
    tmpDF <- jmvReadWrite::bfi_sample
    expect_equal(as.list(table(tmpDF$gender)),                                                                                  list(Females = 174L, Males = 80L))
    expect_equal(as.list(table(replace_omv(dtaInp = tmpDF, rplLst = list(c("Females", "Female"), c("Males", "Male")))$gender)), list(Female  = 174L, Male  = 80L))
    expect_equal(as.list(table(replace_omv(dtaInp = tmpDF, rplLst = list(c("females", "Female"), c("males", "Male")))$gender)), list(Females = 174L, Males = 80L))
    expect_equal(which(is.na(replace_omv(dtaInp = tmpDF, rplLst = list(c(24, NA)))$age)), c(2, 6, 11, 12, 61, 75, 77, 99, 109, 169, 197, 200))
    expect_equal(which(is.na(replace_omv(dtaInp = tmpDF, rplLst = list(c(24, NA)))$age)), which(tmpDF$age == 24))
    expect_false(any(replace_omv(dtaInp = tmpDF, rplLst = list(c(24, NA)))$age == 24, na.rm = TRUE))

    tmpDF <- jmvReadWrite::bfi_sample2
    tmpDF <- cbind(tmpDF, data.frame(age_log = log(tmpDF$age)))
    expect_equal(vapply(replace_omv(dtaInp = tmpDF, rplLst = list(c(1, 6)), varExc = c("A1", "A2", "A3", "A4", "A5"))[2:26], function(x) c(sum(x == 1), sum(x == 6)), integer(2), USE.NAMES = FALSE),
      t(matrix(c(78, 5, 8, 9, 6, rep(0, 20), 9, 82, 69, 106, 65, 49, 55, 47, 78, 75, 73, 76, 54, 68, 68, 81, 65, 72, 74, 94, 83, 95, 54, 110, 66), ncol = 2)))
    expect_equal(vapply(replace_omv(dtaInp = tmpDF, rplLst = list(c(1, 6)), varInc = c("A1", "A2", "A3", "A4", "A5"))[2:26], function(x) c(sum(x == 1), sum(x == 6)), integer(2), USE.NAMES = FALSE),
      t(matrix(c(rep(0, 5), 2, 7, 7, 71, 45, 52, 51, 15, 14, 4, 59, 30, 42, 41, 64, 3, 75, 4, 4, 60,
                 87, 87, 77, 115, 71, 47, 48, 40, 7, 30, 21, 25, 39, 54, 64, 22, 35, 30, 33, 30, 80, 20, 50, 106, 6), ncol = 2)))
    expect_equal(jmvReadWrite::replace_omv(dtaInp = tmpDF, rplLst = list(c("66574", "Trial")))[244, "ID"],  "Trial")
    expect_equal(which(jmvReadWrite::replace_omv(dtaInp = tmpDF, rplLst = list(c("66574", "Trial"))) != tmpDF),  244)
    expect_equal(jmvReadWrite::replace_omv(dtaInp = tmpDF, rplLst = list(c("A7209", "Trial")))[244, "ID2"], "Trial")
    expect_equal(which(jmvReadWrite::replace_omv(dtaInp = tmpDF, rplLst = list(c("A7209", "Trial"))) != tmpDF), 7244)
    expect_equal(unname(colSums(jmvReadWrite::replace_omv(dtaInp = tmpDF, rplLst = list(c("4.", "0.")), whlTrm = FALSE) != tmpDF)), c(rep(0, 29), 5))
    expect_equal(mean(jmvReadWrite::replace_omv(dtaInp = tmpDF, rplLst = list(c("4.", "0.")), whlTrm = FALSE)[, "age_log"]), 3.1970427)

    expect_warning(expect_equal(vapply(replace_omv(dtaInp = tmpDF, rplLst = list(c(1, 6)), varInc = c("A1", "A2", "A3", "A4", "A5"), varExc = c("ID"))[2:26],
      function(x) c(sum(x == 1), sum(x == 6)), integer(2), USE.NAMES = FALSE),
      t(matrix(c(rep(0, 5), 2, 7, 7, 71, 45, 52, 51, 15, 14, 4, 59, 30, 42, 41, 64, 3, 75, 4, 4, 60,
                 87, 87, 77, 115, 71, 47, 48, 40, 7, 30, 21, 25, 39, 54, 64, 22, 35, 30, 33, 30, 80, 20, 50, 106, 6), ncol = 2))),
      regexp = "Both varInc and varExc are given, varInc takes precedence\\.")

    expect_error(replace_omv(dtaInp = tmpDF),
      regexp = "^Calling replace_omv requires the parameter rplLst \\(a list where each entry is a vector with length 2; see Details in help\\)\\.")
    expect_error(replace_omv(dtaInp = tmpDF, rplLst = c()),
      regexp = "^Calling replace_omv requires the parameter rplLst \\(a list where each entry is a vector with length 2; see Details in help\\)\\.")
    expect_error(replace_omv(dtaInp = tmpDF, rplLst = list(c(1, 2), c(3, 4, 5), c(6, 7))),
      regexp = "^Calling replace_omv requires the parameter rplLst \\(a list where each entry is a vector with length 2; see Details in help\\)\\.")
    expect_error(replace_omv(fleInp = tmpDF, rplLst = list(c(1, 2))),    regexp = "^Please use the argument dtaInp instead of fleInp\\.")
    expect_error(replace_omv(dtaInp = cbind(tmpDF, data.frame(AD2 = as.complex(tmpDF$age))), rplLst = list(c(24, NA))), regexp = "Variable type \\w+ not implemented:")
    expect_error(replace_omv(dtaInp = tmpDF, rplLst = list(c(1, 6)), varInc = c("not_a_var")), regexp = "^All variables in varInc must be contained in the original data set \\(\\w+ are not\\)\\.")
    expect_error(replace_omv(dtaInp = tmpDF, rplLst = list(c(1, 6)), varExc = c("not_a_var")), regexp = "^All variables in varExc must be contained in the original data set \\(\\w+ are not\\)\\.")

    rm(tmpDF)
})
