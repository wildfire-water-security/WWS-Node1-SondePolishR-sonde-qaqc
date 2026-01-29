test_that("ecoregions work", {
  site <- c("44.20",	"-122.2")
  expect_s3_class(get_ecoregion(site), "sf")

  expect_equal(get_ecoregion(site, geometry = FALSE), "Cascades")

  site <- c("40.0",	"-122.2")
  expect_equal(get_ecoregion(site, geometry = FALSE), "Central California Valley")

})
