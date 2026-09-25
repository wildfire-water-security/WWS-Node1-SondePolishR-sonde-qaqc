library(shinytest2)
library(shiny)

test_that("{shinytest2} recording: checking-module2-5", {
  app_dir <- system.file("app", package = "SondePolishR")
  local_app_support(app_dir)
  app <- AppDriver$new(app_dir, variant = platform_variant(),
                       name = "m2-5", height = 911, width = 1619)
  app$upload_file(`data1-pj_file` = file.path(test_path(), "testdata", "example-sonde-project.RDS"))

  #click to load files and create project
  app$set_inputs(`data1-username` = "Smith") #so my username isn't stored with tests
  app$click("data1-load_prj")
  app$set_inputs(modules = "step-2")

  #test changing the version viewed
  app$set_inputs(`data2-date_nav-p_length` = 3)
  app$set_inputs(`data2-date_nav-period_view` = TRUE)
  app$set_inputs(`data2-plot-yaxismax` = 20)
  app$set_inputs(`data2-log_table_rows_selected` = 2, allow_no_input_binding_ = TRUE)
  plot_obj <- app$get_value(export = "data2-plot_obj")
  expect_snapshot_value(get_plotly_snap(plot_obj), style = "json2")
  app$expect_screenshot(name = "changing_version_view")

  #test viewing removals
  app$set_inputs(`data2-update_parms-y_var` = "Temp_C")
  app$set_inputs(`data2-data_version-display_changes` = TRUE)
  app$set_inputs(`data2-log_table_rows_selected` = 5, allow_no_input_binding_ = TRUE)
  plot_obj <- app$get_value(export = "data2-plot_obj")
  expect_snapshot_value(get_plotly_snap(plot_obj), style = "json2")
  app$expect_screenshot(name = "view_data_rm")

  #test viewing additions
  app$set_inputs(`data2-log_table_rows_selected` = 6, allow_no_input_binding_ = TRUE)
  plot_obj <- app$get_value(export = "data2-plot_obj")
  expect_snapshot_value(get_plotly_snap(plot_obj), style = "json2")
  app$expect_screenshot(name = "view_data_add")

  #test viewing changes
  app$set_inputs(`data2-update_parms-y_var` = "ODO_mg_L")
  app$set_inputs(`data2-log_table_rows_selected` = 4, allow_no_input_binding_ = TRUE)
  plot_obj <- app$get_value(export = "data2-plot_obj")
  expect_snapshot_value(get_plotly_snap(plot_obj), style = "json2")
  app$expect_screenshot(name = "view_data_chg")


  #test reverting changes (can't get to work)
  # app$set_inputs(`data2-date_nav-period_view` = FALSE)
  # app$set_inputs(`data2-table_opt` = "Change Log") #have to put back to changelog to let it work
  # app$set_inputs(`data2-log_table_rows_selected` = 4, allow_no_input_binding_ = TRUE)
  # app$set_inputs(`data2-conf` = TRUE, allow_no_input_binding_ = TRUE, priority_ = "event")
  # app$expect_values(export = "data2-table", name="changelog-table-undo",screenshot_args = FALSE)
  # plot_obj <- app$get_value(export = "data2-plot_obj")
  # expect_snapshot_value(get_plotly_snap(plot_obj), style = "json2")
  # app$expect_screenshot(name = "undo_change")
})

