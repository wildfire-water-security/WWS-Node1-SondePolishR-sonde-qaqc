library(shiny)

test_that("data is not updated when prj_path is empty", {

  testServer(confirm_changes_server, {
    #simulates a click
    session$setInputs(rm_points = 1)

    #force the reactive system to re-execute observers and render functions immediately, may not be needed
    #session$flushReact()

    #checking the data table doesn't change
      #updated_data() calls the data.frame
    expect_equal(sdata(),raw_sonde)
  },
  #these are passed to the module
  args = list(
    newdata = reactiveVal(raw_sonde),
    sdata = reactiveVal(raw_sonde),
    index = reactiveVal(1),
    par = reactiveVal("x"),
    flag_name = "flagged",
    note = reactive("test"),
    prj_path = reactiveVal(list(type=character(), path=character()))
  ))
})



test_that("data is not updated when index is empty", {

  testServer(confirm_changes_server, {
    #simulates a click
    session$setInputs(rm_points = 1)

    #force the reactive system to re-execute observers and render functions immediately, may not be needed
    #session$flushReact()

    #checking the data table doesn't change
    #updated_data() calls the data.frame
    expect_equal(sdata(),raw_sonde)
  },
  #these are passed to the module
  args = list(
    newdata = reactiveVal(raw_sonde),
    sdata = reactiveVal(raw_sonde),
    index = reactiveVal(integer()),
    par = reactiveVal("x"),
    flag_name = "flagged",
    note = reactive("test"),
    prj_path = reactiveVal(list(type="absolute", path="test"))
  ))
})


test_that("flag_data is applied when inputs are valid", {
  test_dir <- withr::local_tempfile()
      testServer(confirm_changes_server, {
        #simulates a click
        session$setInputs(rm_points = 1)

        #pull out new dataframe to make sure it looks right
        result <- sdata()

        expect_true("Cond_uS_cm_flag" %in% colnames(result))
        expect_true(inherits(result$Cond_uS_cm_flag, "list"))
        expect_equal(names(result$Cond_uS_cm_flag[[1]]), "flagged")
        expect_equal(sapply(result$Cond_uS_cm_flag, "[[", 1)[1:3],c(TRUE, TRUE, FALSE))

      },
      args = list(
        newdata = reactiveVal(raw_sonde),
        sdata = reactiveVal(raw_sonde),
        index = reactiveVal(1:2),
        par = reactiveVal("Cond_uS_cm"),
        flag_name = "flagged",
        note = reactive("test"),
        prj_path = reactiveVal(list(type="absolute", path=test_dir )),
        log = reactiveVal(get_log())

      ))


})


test_that("new data replaces sdata", {
  newdat <- raw_sonde
  newdat$Cond_uS_cm[1:4] <- NA

  test_dir <- withr::local_tempfile()
  testServer(confirm_changes_server, {
    #simulates a click
    session$setInputs(rm_points = 1)

    #pull out new dataframe to make sure it looks right
    expect_equal(sdata()$Cond_uS_cm,newdat$Cond_uS_cm)

  },
  args = list(
    newdata = reactiveVal(newdat),
    sdata = reactiveVal(raw_sonde),
    index = reactiveVal(1:4),
    par = reactiveVal("Cond_uS_cm"),
    flag_name = "flagged",
    note = reactive("test"),
    prj_path = reactiveVal(list(type="absolute", path=test_dir )),
    log = reactiveVal(get_log())

  ))


})

