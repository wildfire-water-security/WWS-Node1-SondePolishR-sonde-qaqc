test_that("data is loaded", {
  #test csv
  path <- file.path(testthat::test_path(), "testdata/example-csv-data1.csv")
  data <- read_sonde(path)

  expect_equal(dim(data), c(1916, 13))
  expect_equal(colnames(data)[2], "Date")

  #test usb csv
  path <- file.path(testthat::test_path(), "testdata/sonde-usb-example.csv")
  data <- read_sonde(path)

  expect_equal(dim(data), c(2701, 13))
  expect_equal(colnames(data)[2], "Date")

  #check tz
  path <- file.path(testthat::test_path(), "testdata/example-csv-data1.csv")
  expect_equal(attr(data$DateTime, "tzone")[1], "Etc/GMT+8")
  data <- read_sonde(path, tz="America/Los_Angeles")
  expect_equal(attr(data$DateTime, "tzone")[1], "America/Los_Angeles")

  #ensure dates are parsed correctly
  expect_s3_class(data$Date, "Date")
  expect_s3_class(data$DateTime, "POSIXct")
  expect_s3_class(data$DateTime_rd, "POSIXct")

  #check that serial numbers are pulled out if requested
  sonde <- read_sonde(path, return="sonde")
  expect_s3_class(sonde, "sonde")
  expect_s3_class(sonde$serials, "data.frame")
  expect_s3_class(sonde$data, "data.frame")
  expect_equal(dim(sonde$data), c(1916, 13))
  expect_equal(class(sonde$file), "character")


})
