library(shinytest2)
library(shiny)

test_that("{shinytest2} recording: checking-module6", {
  app_dir <- system.file("app", package = "SondePolishR")
  local_app_support(app_dir)
  app <- AppDriver$new(app_dir, variant = platform_variant(),
                       name = "m6", height = 911, width = 1619,
                       expect_values_screenshot_args = FALSE)
  app$upload_file(`data1-pj_file` = file.path(test_path(), "testdata", "example-sondeproj-messy.RDS"))

  #click to load files and create project
  app$click("data1-load_prj")

  app$set_inputs(modules = "step-6")

  #check initial plot
  plot_obj <- app$get_value(export = "data6-plot_obj")
  expect_snapshot_value(get_plotly_snap(plot_obj), style = "json2")
  app$expect_screenshot(name = "intial_plot")

  #test changing method
  app$set_inputs(`data6-method` = "spline")
  app$wait_for_idle()
  plot_obj <- app$get_value(export = "data6-plot_obj")
  expect_snapshot_value(get_plotly_snap(plot_obj), style = "json2")
  app$expect_screenshot(name = "spline")

  #test changing max length
  app$set_inputs(`data6-max_length` = 100)
  plot_obj <- app$get_value(export = "data6-plot_obj")
  expect_snapshot_value(get_plotly_snap(plot_obj), style = "json2")
  app$expect_screenshot(name = "change_max_len")

  #test changing method
  app$set_inputs(`data6-max_length` = 8)
  app$set_inputs(`data6-method` = "ts_interp")
  app$wait_for_idle()
  plot_obj <- app$get_value(export = "data6-plot_obj")
  expect_snapshot_value(get_plotly_snap(plot_obj), style = "json2")
  app$expect_screenshot(name = "linear_ts")

  #change freq
  app$set_inputs(`data6-freq` = 100)
  app$wait_for_idle()
  plot_obj <- app$get_value(export = "data6-plot_obj")
  expect_snapshot_value(get_plotly_snap(plot_obj), style = "json2")
  app$expect_screenshot(name = "change_freq")

  #test flagging points
  app$click("data6-apply_limits-apply_flags", timeout_ = 10000)
  plot_obj <- app$get_value(export = "data6-plot_obj")
  expect_snapshot_value(get_plotly_snap(plot_obj), style = "json2")
  app$expect_screenshot(name = "after_flagging")

  tab <- app$get_value(export = "data6-changelog")
  expect_true(nrow(tab) > 1)
  expect_equal(tab$parameter[nrow(tab)], "fDOM_QSU")

})

