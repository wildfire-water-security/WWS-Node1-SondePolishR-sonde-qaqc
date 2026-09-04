# This file is part of the standard setup for testthat.
# It is recommended that you do not modify it.
#
# Where should you do additional test configuration?
# Learn more about the roles of various files in:
# * https://r-pkgs.org/testing-design.html#sec-tests-files-overview
# * https://testthat.r-lib.org/articles/special-files.html

library(testthat)
library(SondePolishR)
options(shinytest2.load_timeout = 30 * 1000) # Increases timeout to 30 seconds

test_check("SondePolishR")
