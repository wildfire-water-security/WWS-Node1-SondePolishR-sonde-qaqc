test_that("shift guessing works", {
  expect_equal(guess_shift(raw_sonde, "Cond_uS_cm", 1591:1630), 8.03)

  data <- shift_points(raw_sonde, "Cond_uS_cm", 1591:1630)
  expect_equal(data$Cond_uS_cm[1591:1600],raw_sonde$Cond_uS_cm[1591:1600] + 8.03)

  expect_equal(guess_shift(raw_sonde, "Cond_uS_cm", 1:6), 0.47)
  expect_equal(guess_shift(raw_sonde, "Cond_uS_cm", (nrow(raw_sonde)-5):nrow(raw_sonde)), -0.22)

})
