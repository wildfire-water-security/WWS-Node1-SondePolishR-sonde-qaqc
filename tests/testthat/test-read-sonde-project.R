test_that("project is read", {
  path <- file.path(testthat::test_path(), "testdata/example-sonde-project.qs")

  df <- read_project(path)

  #ensure it reads in
  expect_length(df, 3)
  expect_equal(names(df), c("raw", "61cded2baf59d3a388af321e9a6aa6a3",
                            "f74bb543f3300fb0be70f0aa2315c737"))

  #ensure it loads the data and log
  expect_equal(names(df), names(get_data()))
  expect_equal(nrow(get_log()), 2)
  expect_equal(get_log()$version, names(df)[-1])

})
