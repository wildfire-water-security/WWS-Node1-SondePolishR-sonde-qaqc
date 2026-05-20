library(shiny)

test_that("name update works simply", {

  testServer(update_parms_server, {
    #force the reactive system to re-execute observers and render functions
    session$flushReact()

    #we haven't set up a y_var yet so expect null
    expect_true(is.null(session$input$y_var))
    #expect choices to the be the same as the data
    expect_equal(choices_r(), c("x","y", "z"))

    # Simulate user selecting a variable
    session$setInputs(y_var = "y")
    session$flushReact()

    # Returned reactive should update
    expect_equal(session$returned(), "y")

    #test changing the data
    data(data.frame(a=1,b=2,c=3))
    session$flushReact()
    expect_equal(choices_r(), c("a","b", "c"))

  },
  #these are passed to the module
  args = list(
    data = reactiveVal(data.frame(x=1,y=2,z=3))
  ))
})


test_that("name update works with raw sonde data", {

  testServer(update_parms_server, {
    #force the reactive system to re-execute observers and render functions
    session$flushReact()

    #we haven't set up a y_var yet so expect null
    expect_true(is.null(session$input$y_var))
    #expect choices to the be the same as the data
    expect_equal(choices_r(), colnames(raw_sonde))

    # Simulate user selecting a variable
    session$setInputs(y_var = "fDOM_RFU")
    session$flushReact()

    # Returned reactive should update
    expect_equal(session$returned(), "fDOM_RFU")

  },
  #these are passed to the module
  args = list(
    data = reactiveVal(raw_sonde)
  ))
})
