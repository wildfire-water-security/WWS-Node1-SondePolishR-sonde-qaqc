test_that("file encoding works", {
  path <- file.path(testthat::test_path(), "testdata/sonde-example.csv")
  expect_equal(get_encoding(path), "UTF-16LE")

})

test_that("skip guessing works", {
  path <- file.path(testthat::test_path(), "testdata/sonde-example.csv")

  expect_equal(get_skip(path),9)

})

test_that("flags are omitted", {
 #add flags
  data <- add_flags(raw_sonde, "fDOM_QSU", "test_flag", c(1,2,3))
  data <- add_flags(data, "fDOM_QSU", "test_flag2", c(1,3))

  expect_equal(data$fDOM_QSU[1], 6.23)
  expect_equal(data$fDOM_QSU[2], 6.18)
  expect_equal(data$fDOM_QSU[3], 6.10)

  #remove all flags
  data2 <- remove_flagged(data, c("test_flag", "test_flag2"))
  expect_equal(data2$fDOM_QSU[1], as.numeric(NA))
  expect_equal(data2$fDOM_QSU[2], as.numeric(NA))
  expect_equal(data2$fDOM_QSU[3], as.numeric(NA))

  #remove just one flag
  data2 <- remove_flagged(data, c("test_flag2"))
  expect_equal(data2$fDOM_QSU[1], as.numeric(NA))
  expect_equal(data2$fDOM_QSU[2], 6.18)
  expect_equal(data2$fDOM_QSU[3], as.numeric(NA))


})

test_that("time zones are convertly correctly", {
  time <- as.POSIXct(Sys.time())
  time2 <- as.POSIXct(Sys.time(), tz = "Etc/GMT+8")

  expect_equal(unname(nice_tz()["UTC-8"]), "Etc/GMT+8")
  expect_true(abs(as.numeric(difftime(time, time2,units="hours"))) < 1.02)
})
