test_that("data is loaded", {
  #test csv
  path <- file.path(testthat::test_path(), "testdata/sonde-example.csv")
  expect_no_error(df <- read_sonde(path))

  expect_equal(dim(df), c(2209, 39))
  expect_equal(colnames(df)[2], "Date_MM_DD_YYYY")

  #test usb csv
  path <- file.path(testthat::test_path(), "testdata/sonde-usb-example.csv")
  expect_no_error(df <- read_sonde(path))

  expect_equal(dim(df), c(2701, 26))
  expect_equal(colnames(df)[2], "Date_MM_DD_YYYY")

  #check tz
  path <- file.path(testthat::test_path(), "testdata/sonde-example.csv")
  expect_equal(attr(df$DateTime, "tzone"), Sys.timezone())
  expect_no_error(df <- read_sonde(path, tz="Etc/GMT-8"))
  expect_equal(attr(df$DateTime, "tzone"), "Etc/GMT-8")

  #check flags
  df <- read_sonde(path, flags = FALSE)
  expect_false(all(grepl("_flag", colnames(df))))

  df <- read_sonde(path, flags = TRUE)
  expect_equal(sum(grepl("_flag", colnames(df))), 15)

  #ensure dates are parsed correctly
  expect_s3_class(df$Date_MM_DD_YYYY, "POSIXct")
  expect_s3_class(df$DateTime, "POSIXct")

})
