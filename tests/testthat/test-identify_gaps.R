test_that("gaps are identifed", {
  #test with example with no dups, should return NULL
  expect_equal(identify_gaps(example_data), NULL)

  #add some dups
  messy <- readRDS(file.path(test_path(), "testdata/example-sondeproj-messy.RDS"))
  tab <- identify_gaps(messy$data)

  #check vals
  expect_true(inherits(tab, "data.frame"))
  expect_equal(nrow(tab), 1)
  expect_equal(tab$gap_length, 81)

  #add in a second gap
  messy$data <- messy$data[-(80:100),]
  tab <- identify_gaps(messy$data)
  expect_equal(nrow(tab), 2)
  expect_equal(tab$gap_length, c(21,81))


})
