# library(shiny)
#
# test_that("name update works with raw sonde data", {
#
#   testServer(update_parms_server, {
#     #force the reactive system to re-execute observers and render functions
#     session$flushReact()
#
#     #we haven't set up a y_var yet so expect null
#     expect_true(is.null(session$input$y_var))
#     #expect choices to the be the same as the data
#     expect_equal(choices_r(), colnames(example_data))
#
#     # Simulate user selecting a variable
#     session$setInputs(y_var = "fDOM_QSU")
#     session$flushReact()
#
#     # Returned reactive should update
#     expect_equal(session$returned(), "fDOM_QSU")
#
#   },
#   #these are passed to the module
#   args = list(
#     sondeproj = reactiveVal(example_sondeproj)
#   ))
# })
