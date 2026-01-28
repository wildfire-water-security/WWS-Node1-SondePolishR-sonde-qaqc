test_that("project is read", {
  path <- file.path(testthat::test_path(), "testdata/example-sonde-project.RDS")

  df <- read_project(path)

  #ensure it reads in
  expect_length(df, 3)
  expect_equal(names(df), c("raw", "ae82b5a3c51f73d907c92729ec5a19c6",
                            "fae3dc14bd21f5b2f8b92a253548e8ad"))

  #ensure it loads the data and log
  expect_equal(names(df), names(get_data()))
  expect_equal(nrow(get_log()), 2)
  expect_equal(get_log()$version, names(df)[-1])

})
