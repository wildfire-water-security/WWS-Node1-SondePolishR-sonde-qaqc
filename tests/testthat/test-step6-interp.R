library(shinytest2)
library(shiny)

test_that("{shinytest2} recording: checking-module6", {
  app_dir <- system.file("app", package = "SondePolishR")
  local_app_support(app_dir)
  app <- AppDriver$new(app_dir, variant = platform_variant(),
                       name = "checking-plotting-module", height = 911, width = 1619,
                       expect_values_screenshot_args = FALSE)
  app$upload_file(`data1-pj_file` = file.path(test_path(), "testdata", "example-sondeproj-messy.RDS"))

  #click to load files and create project
  app$click("data1-load_prj")

  #check initial plot
  plot_obj <- app$get_value(export = "data6-plot_obj")
  vdiffr::expect_doppelganger("intial interpolation plot", plot_obj)

  #test changing method
  app$set_inputs(`data6-method` = "spline")
  app$wait_for_idle()
  plot_obj <- app$get_value(export = "data6-plot_obj")
  vdiffr::expect_doppelganger("switching to spline", plot_obj)

  #test changing max length
  app$set_inputs(`data6-max_length` = 100)
  plot_obj <- app$get_value(export = "data6-plot_obj")
  vdiffr::expect_doppelganger("changing max length", plot_obj)

  #test changing method
  app$set_inputs(`data6-max_length` = 8)
  app$set_inputs(`data6-method` = "ts_interp")
  app$wait_for_idle()
  plot_obj <- app$get_value(export = "data6-plot_obj")
  vdiffr::expect_doppelganger("switching to linear ts", plot_obj)

  #change freq
  app$set_inputs(`data6-freq` = 100)
  app$wait_for_idle()
  plot_obj <- app$get_value(export = "data6-plot_obj")
  vdiffr::expect_doppelganger("changing freq", plot_obj)

  #test flagging points
  app$click("data6-apply_limits-apply_flags")
  plot_obj <- app$get_value(export = "data6-plot_obj")
  vdiffr::expect_doppelganger("after values are flagged", plot_obj)
  tab <- app$get_value(export = "data6-changelog")
  expect_true(nrow(tab) > 1)
  expect_equal(tab$parameter[nrow(tab)], "fDOM_QSU")

})

