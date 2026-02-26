test_that("project is read", {
  #make sure we start with a clean slate
  clear_data()
  clear_log()
  set_prjpath(character())

  path <- file.path(testthat::test_path(), "testdata/example-sonde-project.RDS")

  data <- read_project(path)

  #ensure it reads in
  expect_length(data, 3)
  expect_equal(names(data), c("raw", "89c22249e8164cf1ad454d2f1e0e8abe",
                            "c6b5a0644acac829d66f6729ae9009db"))

  #ensure it loads the data and log
  expect_equal(names(data), names(get_data()))
  expect_equal(nrow(get_log()), 3)
  expect_equal(get_log()$version, names(data))

  expect_equal(get_prjpath(), "inst/extdata/example-sonde-project.RDS")
})
