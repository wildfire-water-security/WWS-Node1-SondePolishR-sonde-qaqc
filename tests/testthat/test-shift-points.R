test_that("shift guessing works", {
  expect_equal(guess_shift(raw_sonde, "Cond_S_cm", 1591:1630), 7.5)

  df <- shift_points(raw_sonde, "Cond_S_cm", 1591:1630)
  expect_equal(df$Cond_S_cm[1591:1600],raw_sonde$Cond_S_cm[1591:1600] + 7.5)
})
