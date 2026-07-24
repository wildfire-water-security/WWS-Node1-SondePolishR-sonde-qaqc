library(shinytest2)
library(shiny)

test_that("{shinytest2} recording: checking-module2", {
  app_dir <- system.file("app", package = "SondePolishR")
  local_app_support(app_dir)
  app <- AppDriver$new(app_dir, variant = platform_variant(),
                       name = "m2", height = 911, width = 1619)
  app$upload_file(`data1-pj_file` = file.path(test_path(), "testdata", "example-sonde-project.RDS"))

  #click to load files and create project
  app$click("data1-load_prj")

  #check initial plot is made
  app$set_inputs(modules = "step-2")
  app$set_inputs(`data2-update_parms-y_var` = "fDOM_QSU")
  app$wait_for_idle()
  Sys.sleep(1)
  plot_obj <- app$get_value(export = "data2-plot_obj")
  expect_snapshot_value(get_plotly_snap(plot_obj), style = "json2")
  app$expect_screenshot(name = "intial_plot")

  #check putting in week view
  app$set_inputs(`data2-date_nav-period_view` = TRUE)
  app$wait_for_idle()
  plot_obj <- app$get_value(export = "data2-plot_obj")
  expect_snapshot_value(get_plotly_snap(plot_obj), style = "json2")
  app$expect_screenshot(name = "weekly_view")

  #clicking next week
  app$click("data2-date_nav-next_period")
  app$wait_for_idle()
  rng <- app$get_value(input= "data2-date_nav-dates")
  expect_equal(rng, as.Date(c("2024-07-31", "2024-12-29"))) #range shouldn't change
  plot_obj <- app$get_value(export = "data2-plot_obj")
  expect_snapshot_value(get_plotly_snap(plot_obj), style = "json2")
  app$expect_screenshot(name = "next_week")

  #click previous week
  app$click("data2-date_nav-prev_period")
  app$wait_for_idle()
  plot_obj <- app$get_value(export = "data2-plot_obj")
  expect_snapshot_value(get_plotly_snap(plot_obj), style = "json2")
  app$expect_screenshot(name = "prev_week")

  #unclick weekly and make sure we get the full plot again
  app$set_inputs(`data2-date_nav-period_view` = FALSE)
  app$wait_for_idle()
  plot_obj <- app$get_value(export = "data2-plot_obj")
  expect_snapshot_value(get_plotly_snap(plot_obj), style = "json2")
  app$expect_screenshot(name = "removing_weekly_view")

  #test changing the date range and setting weekly view
  app$set_inputs(`data2-date_nav-dates` = c("2024-09-01", "2024-12-29"))
  app$wait_for_idle()
  plot_obj <- app$get_value(export = "data2-plot_obj")
  expect_snapshot_value(get_plotly_snap(plot_obj), style = "json2")
  app$expect_screenshot(name = "changing_date_range")

  app$set_inputs(`data2-date_nav-period_view` = TRUE)
  app$wait_for_idle()
  plot_obj <- app$get_value(export = "data2-plot_obj")
  expect_snapshot_value(get_plotly_snap(plot_obj), style = "json2")
  app$expect_screenshot(name = "weekly_view_adjstart")
  app$set_inputs(`data2-date_nav-period_view` = FALSE)

  #check changing variable to plot
  app$set_inputs(`data2-update_parms-y_var` = "Temp_C")
  app$wait_for_idle()
  plot_obj <- app$get_value(export = "data2-plot_obj")
  expect_snapshot_value(get_plotly_snap(plot_obj), style = "json2")
  app$expect_screenshot(name = "change_variable")

  #check on the table
  app$expect_values(export = "data2-table", name="changelog-table",screenshot_args = FALSE)

  #change to get other tables
  app$set_inputs(`data2-table_opt` = "Field Form")
  app$expect_values(export = "data2-table", name="fieldform-table",screenshot_args = FALSE) #fieldform

  app$set_inputs(`data2-table_opt` = "Calibration Check")
  app$expect_values(export = "data2-table", name="calcheck-table",screenshot_args = FALSE) #cal check

  app$set_inputs(`data2-table_opt` = "Data Summary")
  app$expect_values(export = "data2-table", name="datasum-table",screenshot_args = FALSE) #cal check

  app$set_inputs(`data2-date_nav-period_view` = TRUE)
  app$expect_values(export = "data2-table", name="datasum-table-weekly",screenshot_args = FALSE) #cal check

  #test reverting changes
  app$set_inputs(`data2-log_table_rows_selected` = 4, allow_no_input_binding_ = TRUE)
  app$set_inputs(`data2-conf` = TRUE, allow_no_input_binding_ = TRUE, priority_ = "event")
  app$set_inputs(`data2-table_opt` = "Change Log")
  app$expect_values(export = "data2-table", name="changelog-table-undo",screenshot_args = FALSE)

  app$wait_for_idle()
  Sys.sleep(2)
  plot_obj <- app$get_value(export = "data2-plot_obj")
  expect_snapshot_value(get_plotly_snap(plot_obj), style = "json2")
  app$expect_screenshot(name = "undo_change")
})

