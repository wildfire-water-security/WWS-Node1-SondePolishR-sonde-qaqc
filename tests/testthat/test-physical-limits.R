test_that("physical limits work", {
  #test we're getting what we expect
  results <- physical_limit(raw_sonde, min=0, max=98, par="ODO_sat")
  expect_length(results, 2)
  expect_s3_class(results$outlier, "data.frame")
  expect_s3_class(results$within, "data.frame")
  expect_equal(rownames(results$outlier), c("1436", "1437", "1438"))

  #test keep works
  results <- physical_limit(raw_sonde, min=0, max=98, par="ODO_sat", keep=1436)
  expect_equal(rownames(results$outlier), c("1437", "1438"))

  #test changing limits changes rows
  results1 <- physical_limit(raw_sonde, min=0, max=98, par="ODO_sat")$outlier
  results2 <- physical_limit(raw_sonde, min=0, max=97, par="ODO_sat")$outlier

  expect_true(nrow(results1) < nrow(results2))
})
