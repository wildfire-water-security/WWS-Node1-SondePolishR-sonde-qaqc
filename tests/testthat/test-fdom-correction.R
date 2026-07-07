test_that("fdom equations are returned", {
  info <- SondePolishR:::get_equation("none")
  expect_true(inherits(info, "list"))
  expect_true(inherits(info$params, "list"))
  expect_equal(info$equation, "")
  expect_equal(info$source, "")

  #test that fun returns the same thing
    return <- info$fun(1:10, NA, NA)
    expect_equal(return, 1:10)

  #try a different method with more things
    info <- SondePolishR:::get_equation("inverse_poly")
    expect_true(inherits(info, "list"))
    expect_true(inherits(info$params, "list"))
    expect_equal(length(info$params), 3)
    expect_true(is.character(info$equation))
    expect_equal(info$source, "Fleck et al. 2026")

    #test that fun doesn't returns the same thing
    params <- lapply(info$params, "[[", 1)
    return <- info$fun(1:10, rep(1, 10), params)
    expect_true(all(!return == 1:10))


})
