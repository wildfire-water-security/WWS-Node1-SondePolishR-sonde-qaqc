test_that("file encoding works", {
  path <- file.path(testthat::test_path(), "testdata/sonde-example.csv")
  expect_equal(get_encoding(path), "UTF-16LE")

})

test_that("skip guessing works", {
  path <- file.path(testthat::test_path(), "testdata/sonde-example.csv")

  expect_equal(get_skip(path),9)

})



test_that("time zones are convertly correctly", {
  time <- as.POSIXct(Sys.time())
  time2 <- as.POSIXct(Sys.time(), tz = "Etc/GMT+8")

  expect_equal(unname(nice_tz()["UTC-8"]), "Etc/GMT+8")
  expect_true(abs(as.numeric(difftime(time, time2,units="hours"))) < 1.02)
})
