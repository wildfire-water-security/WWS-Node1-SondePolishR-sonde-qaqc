test_that("project is read", {
  #make sure we start with a clean slate
  clear_data()
  clear_log()
  clear_prjpath()

  path <- file.path(testthat::test_path(), "testdata/example-sonde-project.RDS")

  data <- read_project(path)

  #ensure it reads in
  expect_length(data, 3)

  #ensure it loads the data and log
  expect_equal(names(data), names(get_data()))
  expect_equal(nrow(get_log()), 3)
  expect_equal(get_log()$version, names(data))

  expect_equal(get_prjpath(), list(type="package", path="extdata/example-sonde-project.RDS"))
})
