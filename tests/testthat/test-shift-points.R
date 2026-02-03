test_that("shift guessing works", {
  expect_equal(guess_shift(raw_sonde, "Cond_uS_cm", 1591:1630), 7.5)

  data <- shift_points(raw_sonde, "Cond_uS_cm", 1591:1630)
  expect_equal(data$Cond_uS_cm[1591:1600],raw_sonde$Cond_uS_cm[1591:1600] + 7.5)
})
