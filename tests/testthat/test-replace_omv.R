test_that("replace_omv works", {
    tmpDF <- jmvReadWrite::bfi_sample
    expect_equal(as.list(table(tmpDF$gender)),                                                                                  list(Females = 174L, Males = 80L))
    expect_equal(as.list(table(replace_omv(dtaInp = tmpDF, rplLst = list(c("Females", "Female"), c("Males", "Male")))$gender)), list(Female  = 174L, Male  = 80L))
    expect_equal(as.list(table(replace_omv(dtaInp = tmpDF, rplLst = list(c("females", "Female"), c("males", "Male")))$gender)), list(Females = 174L, Males = 80L))
    expect_equal(which(is.na(replace_omv(dtaInp = tmpDF, rplLst = list(c(24, NA)))$age)), c(2, 6, 11, 12, 61, 75, 77, 99, 109, 169, 197, 200))
    expect_equal(which(is.na(replace_omv(dtaInp = tmpDF, rplLst = list(c(24, NA)))$age)), which(tmpDF$age == 24))
    expect_false(any(replace_omv(dtaInp = tmpDF, rplLst = list(c(24, NA)))$age == 24, na.rm = TRUE))

    expect_error(replace_omv(dtaInp = tmpDF),
      regexp = "^Calling replace_omv requires the parameter rplLSt \\(a list where each entry is a vector with length 2; see Details in help\\)\\.")
    expect_error(replace_omv(dtaInp = tmpDF, rplLst = c()),
      regexp = "^Calling replace_omv requires the parameter rplLSt \\(a list where each entry is a vector with length 2; see Details in help\\)\\.")
    expect_error(replace_omv(dtaInp = tmpDF, rplLst = list(c(1, 2), c(3, 4, 5), c(6, 7))),
      regexp = "^Calling replace_omv requires the parameter rplLSt \\(a list where each entry is a vector with length 2; see Details in help\\)\\.")
    expect_error(replace_omv(fleInp = tmpDF, rplLst = list(c(1, 2))),    regexp = "^Please use the argument dtaInp instead of fleInp\\.")
    expect_error(replace_omv(dtaInp = cbind(tmpDF, data.frame(AD2 = as.complex(tmpDF$AD))), rplLst = list(c(24, NA))), regexp = "Variable type \\w+ not implemented:")

    rm(tmpDF)
})
