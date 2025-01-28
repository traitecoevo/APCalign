# This file is part of the standard setup for testthat.
# It is recommended that you do not modify it.
#
# Where should you do additional test configuration?
# Learn more about the roles of various files in:
# * https://r-pkgs.org/tests.html
# * https://testthat.r-lib.org/reference/test_package.html#special-files

# Setting the CPU Thread limit to max 2 to meet CRAN guidelines. 
Sys.setenv("OMP_THREAD_LIMIT" = 2)

library(testthat)
library(APCalign)

test_check("APCalign")
