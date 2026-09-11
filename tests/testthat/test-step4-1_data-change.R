library(shinytest2)
library(shiny)

test_that("{shinytest2} recording: checking-module4-1", {
  app_dir <- system.file("app", package = "SondePolishR")
  local_app_support(app_dir)
  app <- AppDriver$new(app_dir, variant = platform_variant(),
                       name = "m41", height = 911, width = 1619,
                       expect_values_screenshot_args = FALSE)
  app$upload_file(`data1-pj_file` = file.path(test_path(), "testdata", "example-sonde-project.RDS"))

  #since it's now required
  app$set_inputs(`data1-username` = "Smith")

  #click to load files and create project
  app$click("data1-load_prj")

  app$set_inputs(modules = "step-5")

  #check initial commit without period view
  app$click("data5-apply_limits-apply_flags")
  plot_obj <- app$get_value(export = "data5-plot_obj")
  expect_snapshot_value(get_plotly_snap(plot_obj), style = "json2")
  app$expect_screenshot(name = "flag-noperiod")

  #try with period view
  app$set_inputs(`data5-max` = 100)
  app$set_inputs(`data5-date_nav-p_length` = 14)
  app$set_inputs(`data5-date_nav-period_view` = TRUE)
  app$click("data5-date_nav-next_period")
  app$click("data5-apply_limits-apply_flags")
  app$set_inputs(`data5-apply_limits-confirm_full_save` = TRUE, allow_no_input_binding_ = TRUE, priority_ = "event")
  plot_obj <- app$get_value(export = "data5-plot_obj")
  expect_snapshot_value(get_plotly_snap(plot_obj), style = "json2")
  app$expect_screenshot(name = "flag-gotofull")

  #try with period view not reverting
  app$set_inputs(`data5-date_nav-period_view` = TRUE)
  app$click("data5-date_nav-next_period")
  app$set_inputs(`data5-max` = 25)
  app$set_inputs(`data5-apply_limits-confirm_full_save` = FALSE, allow_no_input_binding_ = TRUE, priority_ = "event")
  app$set_inputs(`data5-date_nav-period_view` = FALSE)
  plot_obj <- app$get_value(export = "data5-plot_obj")
  expect_snapshot_value(get_plotly_snap(plot_obj), style = "json2")
  app$expect_screenshot(name = "flag-periodonly")


})

