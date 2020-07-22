Sys.setenv("R_TESTS" = "")
library(testthat)
library(validateIt)

test_check("validateIt")
