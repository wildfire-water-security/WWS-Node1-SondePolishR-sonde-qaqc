test_that("getting precip works", {
    data <- example_data[example_data$Date <= "2024-08-05",] #only do a few days so we're not making API angry
   precip <- get_precip(data, 43.96775, -122.63012, method="merra-2")

   expect_true(inherits(precip, "data.frame"))
   expect_equal(nrow(precip), 132)

   expect_true(min(precip$DateTime) <= min(data$DateTime_rd))
   expect_true(max(precip$DateTime) <= max(data$DateTime_rd))

})
