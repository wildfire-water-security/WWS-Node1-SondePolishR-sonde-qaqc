test_that("duplicates are identifed", {
  #test with example with no dups, should return NULL
  expect_equal(identify_dups(example_data), NULL)

  #add some dups
    messy <- readRDS(file.path(test_path(), "testdata/example-sondeproj-messy.RDS"))
    tab <- identify_dups(messy$data)

    #check vals
      expect_true(inherits(tab, "data.frame"))
      expect_equal(nrow(tab), 2)
      expect_equal(tab$length, c(14,14))
      expect_equal(tab$likely_issue, c("sonde malfunctioned duplicating data", "data downloaded multiple times"))

  #see if ndif works
    messy$data$fDOM_QSU[messy$data$FileName == "dupfile2.csv"] <- messy$data$fDOM_QSU[messy$data$FileName == "dupfile2.csv"] * 1.1
    tab <- identify_dups(messy$data)
    expect_true(tab$ndif[2] > 0)
    expect_true(tab$perc_dif[2] > 0)

})
