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
  df <- add_flags(raw_sonde, "fDOM_QSU", "test_flag", c(1,2,3))
  df <- add_flags(df, "fDOM_QSU", "test_flag2", c(1,3))

  expect_equal(df$fDOM_QSU[1], 6.23)
  expect_equal(df$fDOM_QSU[2], 6.18)
  expect_equal(df$fDOM_QSU[3], 6.10)

  #remove all flags
  df2 <- remove_flagged(df, c("test_flag", "test_flag2"))
  expect_equal(df2$fDOM_QSU[1], as.numeric(NA))
  expect_equal(df2$fDOM_QSU[2], as.numeric(NA))
  expect_equal(df2$fDOM_QSU[3], as.numeric(NA))

  #remove just one flag
  df2 <- remove_flagged(df, c("test_flag2"))
  expect_equal(df2$fDOM_QSU[1], as.numeric(NA))
  expect_equal(df2$fDOM_QSU[2], 6.18)
  expect_equal(df2$fDOM_QSU[3], as.numeric(NA))


})
