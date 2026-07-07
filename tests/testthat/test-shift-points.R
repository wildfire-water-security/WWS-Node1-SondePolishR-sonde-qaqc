test_that("shift guessing works", {
  #test reverting the real data
    proj <- example_sondeproj
    y_var <- "ODO_mg_L"
    index <- 5:7
    newdata <- shift_points(proj$data, y_var, index)
    expect_equal(newdata[[y_var]][index], proj$data[[y_var]][index]/0.8)

  #test a few different types of shifts
    #linear shift down
    y_var <- "Temp_C"
    index <- 20:36
    adjdata <- proj$data
    adjdata[[y_var]][index] <- adjdata[[y_var]][index] - 1.8
    newdata <- shift_points(adjdata, y_var, index)

    #may not be perfectly linear, but we just want it not crazy off
    dif <- (newdata[[y_var]][index] - proj$data[[y_var]][index]) / proj$data[[y_var]][index]
    expect_true(all(dif < 0.1)) #needs to be within 10 % of original point

    #divide by value
    y_var <- "Temp_C"
    index <- 40:63
    adjdata <- proj$data
    adjdata[[y_var]][index] <- adjdata[[y_var]][index] / 0.2
    newdata <- shift_points(adjdata, y_var, index)

    #may not be perfectly linear, but we just want it not crazy off
    dif <- (newdata[[y_var]][index] - proj$data[[y_var]][index]) / proj$data[[y_var]][index]
    expect_true(all(dif < 0.1)) #needs to be within 10 % of original point

    #divide by value and add value
    y_var <- "Temp_C"
    index <- 40:63
    adjdata <- proj$data
    adjdata[[y_var]][index] <- (adjdata[[y_var]][index] + 1.1) / 0.3
    newdata <- shift_points(adjdata, y_var, index)

    #may not be perfectly linear, but we just want it not crazy off
    dif <- (newdata[[y_var]][index] - proj$data[[y_var]][index]) / proj$data[[y_var]][index]
    expect_true(all(dif < 0.1)) #needs to be within 10 % of original point

})
