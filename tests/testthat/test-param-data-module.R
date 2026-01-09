library(shiny)

test_that("when no function is provided, names the are the column names", {

  testServer(update_parms_server, {
    #force the reactive system to re-execute observers and render functions
    session$flushReact()

    #checking the data table doesn't change
    print(y_var())
    expect_equal(y_var(), c("x","y","z"))
  },
  #these are passed to the module
  args = list(
    df = reactiveVal(data.frame(x=1,y=2,z=3))
  ))
})

test_that("parameters get updated", {

  testServer(update_parms_server, {
    #simulates a click
    session$setInputs(rm_points = 1)

    #force the reactive system to re-execute observers and render functions immediately, may not be needed
    #session$flushReact()

    #checking the data table doesn't change
    #updated_df() calls the data.frame
    expect_equal(updated_df(),raw_sonde)
  },
  #these are passed to the module
  args = list(
    df = reactiveVal(raw_sonde),
    choices_fun = nice_yvar
  ))
})
