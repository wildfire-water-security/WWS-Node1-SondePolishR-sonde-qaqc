library(shiny)

test_that("df is not updated when prj_path is empty", {

  testServer(confirm_changes_server, {
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
    index = reactiveVal(1),
    par = reactiveVal("x"),
    flag_name = "flagged",
    prj_path = reactiveVal(character(0))
  ))
})



test_that("df is not updated when index is empty", {

  testServer(confirm_changes_server, {
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
    index = reactiveVal(integer()),
    par = reactiveVal("x"),
    flag_name = "flagged",
    prj_path = reactiveVal("path")
  ))
})

test_that("flag_data is applied when inputs are valid", {

      testServer(confirm_changes_server, {
        #simulates a click
        session$setInputs(rm_points = 1)

        #pull out new dataframe to make sure it looks right
        result <- updated_df()

        expect_true("Cond_uS_cm_flag" %in% colnames(result))
        expect_true(inherits(result$Cond_uS_cm_flag, "list"))
        expect_equal(names(result$Cond_uS_cm_flag[[1]]), "flagged")
        expect_equal(sapply(result$Cond_uS_cm_flag, "[[", 1)[1:3],c(TRUE, TRUE, FALSE))

      },
      args = list(
        df = reactiveVal(raw_sonde),
        index = reactiveVal(1:2),
        par = reactiveVal("Cond_uS_cm"),
        flag_name = "flagged",
        prj_path = reactiveVal("path")
      ))


})
