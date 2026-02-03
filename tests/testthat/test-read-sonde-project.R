test_that("project is read", {
  #make sure we start with a clean slate
  clear_data()
  clear_log()
  set_prjpath(character())

  path <- file.path(testthat::test_path(), "testdata/example-sonde-project.RDS")

  data <- read_project(path)

  #ensure it reads in
  expect_length(data, 3)
  expect_equal(names(data), c("raw", "ae82b5a3c51f73d907c92729ec5a19c6",
                            "fae3dc14bd21f5b2f8b92a253548e8ad"))

  #ensure it loads the data and log
  expect_equal(names(data), names(get_data()))
  expect_equal(nrow(get_log()), 3)
  expect_equal(get_log()$version, names(data))

  expect_equal(get_prjpath(), "inst/extdata/example-sonde-project.RDS")
})
