test_that("field form is read in nicely", {
  #should error if wrong data is added
    wrongfile <- file.path(testthat::test_path(), "testdata/example-calcheck.csv")
    expect_warning(expect_error(read_ff(wrongfile), "Unexpected column names"))

  #reading in correct file
    rightfile <- file.path(testthat::test_path(), "testdata/example-fieldform.csv")
    ff <- read_ff(rightfile)

    #expect the columns to look at certain way
    expect_equal(dim(ff), c(3,16))
    expect_equal(as.Date(ff$Date), as.Date(c("2024-07-31", "2024-08-20", "2024-10-23")), ignore_attr = TRUE)
    expect_equal(unname(sapply(ff, class)), c("Date", rep("character", 9), "logical", "character", "logical", rep("character",3)))
    expect_true(sum(is.na(ff)) > 0)

})

test_that("calibration check is read in nicely", {
  #should error if wrong data is added
  wrongfile <- file.path(testthat::test_path(), "testdata/example-fieldform.csv")
  expect_error(read_cal(wrongfile), "Unexpected column names")

  #reading in correct file
  rightfile <- file.path(testthat::test_path(), "testdata/example-calcheck.csv")
  cal <- read_cal(rightfile)

  #expect the columns to look at certain way
  expect_equal(dim(cal), c(12, 8))
  expect_equal(class(cal$Date), "Date")
  #expect_equal(as.Date(cal$Date), as.Date(rep(c("2024-08-20", "2024-10-23"), each=6)), ignore_attr = TRUE)
  expect_equal(unname(sapply(cal, class)), c("Date", rep("character", 3), "numeric", "character", "numeric", "character"))
  expect_true(sum(is.na(cal)) > 0)

})
