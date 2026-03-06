test_that("shift guessing works", {
  expect_equal(guess_shift(raw_sonde, "Cond_uS_cm", 1591:1630), list(slope=0.02, int=7.4))

  data <- shift_points(raw_sonde, "Cond_uS_cm", 1591:1630)
  expected <- raw_sonde$Cond_uS_cm[1591:1600] + (0.02 * (seq_along(1591:1600)-1)) + 7.4
  expect_equal(data$Cond_uS_cm[1591:1600],expected)

  expect_equal(guess_shift(raw_sonde, "Cond_uS_cm", 1:6), list(slope=0, int=0.1))
  expect_equal(guess_shift(raw_sonde, "Cond_uS_cm", (nrow(raw_sonde)-5):nrow(raw_sonde)), list(slope=0, int=-0.1))

})
