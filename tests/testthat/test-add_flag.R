test_that("flags are added to dataset", {

  data <- add_flags(example_sondeproj$data, "fDOM_QSU", 2:7, "TEST01")

  expect_true(all(inherits(data$fDOM_QSU_flag, "list"))) #make sure stored as list

  expect_equal(data$fDOM_QSU_flag[1], list(c("RM01")))
  expect_equal(data$fDOM_QSU_flag[3], list(c("RM01", "TEST01")))
  expect_equal(data$fDOM_QSU_flag[8], list(c(NA)))

})
