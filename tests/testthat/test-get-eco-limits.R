test_that("eco-limit functions work", {
  #test when there are no stations
  suppressMessages(expect_true(is.na(get_eco_limits("Blue Mountains", "32295"))))

  #test the basic case
  suppressMessages(limits <- get_eco_limits("Coast Range", "00010", nsamp=1))
  expect_equal(dim(limits), c(2,13))
  expect_s3_class(limits, "data.frame")
  expect_equal(limits$n_stats, c(1,1))

  #test when there's two parameters
  suppressMessages(limits <- get_eco_limits("Coast Range", c("00400", "32295"), nsamp=1))

  expect_true(inherits(limits, "list"))
  expect_s3_class(limits[[1]], "data.frame")
  expect_equal(dim(limits[[1]]), c(2,13))
  expect_true(is.na(limits[[2]]))

})
