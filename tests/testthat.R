if (nzchar(system.file(package = "testthat"))) {
    library(testthat)
    library(jmvReadWrite)
    
    test_check("jmvReadWrite")
}
