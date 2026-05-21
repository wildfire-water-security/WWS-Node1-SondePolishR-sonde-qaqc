library(shinytest2)
library(shiny)
library(SondePolishR)


test_that("{shinytest2} recording: checking-module2", {
  app_dir <- system.file("app", package = "SondePolishR")
  local_app_support(app_dir)
  app <- AppDriver$new(app_dir, variant = platform_variant(),
                       name = "checking-plotting-module", height = 911, width = 1619,
                       expect_values_screenshot_args = FALSE)
  app$upload_file(`data1-pj_file` = file.path(test_path(), "testdata", "example-sonde-project.RDS"))

  #click to load files and create project
  app$click("data1-load_prj")

  #check initial plot is made
  plot_obj <- app$get_value(export = "data2-plot_obj")
  vdiffr::expect_doppelganger("initial plot is made", plot_obj)

  #check putting in week view
  app$set_inputs(`data2-week_view` = TRUE)
  plot_obj <- app$get_value(export = "data2-plot_obj")
  vdiffr::expect_doppelganger("week view works", plot_obj)

  #clicking next week
  app$click("data2-next_week")
  rng <- app$get_value(input= "data2-dates")
  expect_equal(rng, as.Date(c("2024-08-07", "2024-08-14")))

  #click previous week
  app$click("data2-prev_week")
  rng <- app$get_value(input= "data2-dates")
  expect_equal(rng, as.Date(c("2024-07-31", "2024-08-07")))

  #unclick weekly and make sure we get the full plot again
  app$set_inputs(`data2-week_view` = FALSE)
  plot_obj <- app$get_value(export = "data2-plot_obj")
  vdiffr::expect_doppelganger("removing week view works", plot_obj)
  rng <- app$get_value(input= "data2-dates")
  expect_equal(rng, as.Date(c("2024-07-31", "2024-10-23")))

  #check changing variable to plot
  app$set_inputs(`data2-update_parms-y_var` = "Temp_C")
  plot_obj <- app$get_value(export = "data2-plot_obj")
  vdiffr::expect_doppelganger("changing plot variable", plot_obj)

  #check on the table
  app$expect_values(output = "data2-tab")

  #change to get other tables
  tab <- app$get_value(export = "data2-tab") #changelog

  app$set_inputs(`data2-table_opt` = "Field Form")
  tab <- app$get_value(export = "data2-tab") #fieldform

  app$set_inputs(`data2-table_opt` = "Calibration Check")
  tab <- app$get_value(export = "data2-tab") #cal check



})

