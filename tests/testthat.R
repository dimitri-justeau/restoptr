# load packages
library(testthat)
library(restoptr)

# enable parallel testing
Sys.unsetenv("R_TESTS")

# run tests
test_check("restoptr")
