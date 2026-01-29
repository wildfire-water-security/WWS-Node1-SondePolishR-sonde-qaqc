test_that("log can be read, written, and cleared", {
  #make sure log is empty
  clear_log()

  #empty log
  expect_equal(get_log(), data.frame(datetime=as.POSIXct(character()),
                                    parameter = character(),
                                    step = character(),
                                    n_changed = numeric(),
                                    user = character(),
                                    version = character()))

  #write to the log
  write_log("Cond_S_cm", "physical limits", 5, "V1")
  log <- get_log()
  expect_equal(nrow(log), 1)
  expect_equal(log$parameter, "Cond_S_cm")
  expect_equal(log$step,  "physical limits")
  expect_equal(log$n_changed, 5)
  expect_equal(log$version, "V1")


  #write another line
  write_log("Cond_S_cm", "physical limits", 2, "V2")
  log <- get_log()
  expect_equal(nrow(log), 2)

  #clear log
  clear_log()
  expect_equal(get_log(), data.frame(datetime=as.POSIXct(character()),
                                     parameter = character(),
                                     step = character(),
                                     n_changed = numeric(),
                                     user = character(),
                                     version = character()))



})


test_that("data saving and logging works", {
  #make sure no data
  clear_data()

  #at the start we have no data
    expect_equal(length(get_data()), 0)
    expect_true(inherits(get_data(), "list"))

  #write some data
    write_data(raw_sonde, "V1")
    expect_equal(length(get_data()), 1)
    expect_equal(get_data()[[1]], raw_sonde)

  #check to see if there's a new version
    expect_true(!new_version(raw_sonde))
    new_sonde <- raw_sonde
    new_sonde$fDOM_QSU[1] <- 2.0234 #change a value so it's new
    expect_true(new_version(new_sonde))


  #clear data
    clear_data()
    expect_equal(length(get_data()), 0)
    expect_true(inherits(get_data(), "list"))

})

test_that("prj path setting and getting works", {
  set_prjpath(character()) #reset

  expect_equal(get_prjpath(), character())

  set_prjpath("test/path")
  expect_equal(get_prjpath(), "test/path")

  set_prjpath("test/path2")
  expect_equal(get_prjpath(), "test/path2")
})
