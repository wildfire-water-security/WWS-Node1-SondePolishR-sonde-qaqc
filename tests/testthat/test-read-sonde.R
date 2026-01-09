test_that("data is loaded", {
  #test csv
  path <- file.path(testthat::test_path(), "testdata/sonde-example.csv")
  df <- read_sonde(path)

  expect_equal(dim(df), c(2209, 35))
  expect_equal(colnames(df)[2], "Date_MM_DD_YYYY")

  #test usb csv
  path <- file.path(testthat::test_path(), "testdata/sonde-usb-example.csv")
  df <- read_sonde(path)

  expect_equal(dim(df), c(2701, 24))
  expect_equal(colnames(df)[2], "Date_MM_DD_YYYY")

  #check tz
  path <- file.path(testthat::test_path(), "testdata/sonde-example.csv")
  expect_equal(attr(df$DateTime, "tzone")[1], "Etc/GMT+8")
  df <- read_sonde(path, tz="America/Los_Angeles")
  expect_equal(attr(df$DateTime, "tzone")[1], "America/Los_Angeles")

  #check flags
  df <- read_sonde(path, flags = FALSE)
  expect_false(all(grepl("_flag", colnames(df))))

  df <- read_sonde(path, flags = TRUE)
  expect_equal(sum(grepl("_flag", colnames(df))), 14)

  #ensure dates are parsed correctly
  expect_s3_class(df$Date_MM_DD_YYYY, "POSIXct")
  expect_s3_class(df$DateTime, "POSIXct")

})
