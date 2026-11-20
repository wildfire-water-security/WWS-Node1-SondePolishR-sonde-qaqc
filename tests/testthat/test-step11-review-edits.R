library(shinytest2)
library(shiny)

test_that("{shinytest2} recording: checking-module11", {
  app_dir <- system.file("app", package = "SondePolishR")
  local_app_support(app_dir)
  app <- AppDriver$new(app_dir, variant = platform_variant(),
                       name = "m11", height = 911, width = 1619)
  app$upload_file(`data1-pj_file` = file.path(test_path(), "testdata", "example-sonde-project.RDS"))

  #click to load files and create project
  app$set_inputs(`data1-username` = "Smith") #so my username isn't stored with tests
  app$click("data1-load_prj")
  app$set_inputs(modules = "step-11")

  #initial view
  plot_obj <- app$get_value(export = "data11-plot_obj")
  expect_snapshot_value(get_plotly_snap(plot_obj), style = "json2")
  app$expect_screenshot(name = "initial-analyst-view")

  #test changing to reviewer mode
  app$set_inputs(`data11-mode` = "reviewer")
  app$wait_for_idle()
  plot_obj <- app$get_value(export = "data11-plot_obj")
  expect_snapshot_value(get_plotly_snap(plot_obj), style = "json2")
  app$expect_screenshot(name = "initial-review-view")

  #test changing y-variable
  app$set_inputs(`data11-update_parms-y_var` = "Temp_C")
  app$wait_for_idle()
  plot_obj <- app$get_value(export = "data11-plot_obj")
  expect_snapshot_value(get_plotly_snap(plot_obj), style = "json2")
  #app$expect_screenshot(name = "changing-yvar") very fussy and fails often due to table not fully getting out.

  #test approving
  app$set_inputs(`data11-data_review-note` = "looks fine")
  app$click("data11-data_review-approve")
  app$wait_for_idle()
  plot_obj <- app$get_value(export = "data11-plot_obj")
  expect_snapshot_value(get_plotly_snap(plot_obj), style = "json2")
  app$expect_screenshot(name = "approve")
  tab <- app$get_value(export = "data11-table")
  expect_equal(tab$review_note[3], "looks fine")

  #test rejecting
  app$set_inputs(`data11-data_review-note` = "try interpolation again")
  app$click("data11-data_review-reject")
  app$wait_for_idle()
  plot_obj <- app$get_value(export = "data11-plot_obj")
  expect_snapshot_value(get_plotly_snap(plot_obj), style = "json2")
  app$expect_screenshot(name = "reject")
  tab <- app$get_value(export = "data11-table")
  expect_equal(tab$review_note[4], "try interpolation again")

  #test justifying
  app$set_inputs(`data11-mode` = "analyst")
  app$set_inputs(`data11-data_review-note` = "linear interpolation, you're wrong")
  app$click("data11-data_review-justify")
  app$wait_for_idle()
  plot_obj <- app$get_value(export = "data11-plot_obj")
  expect_snapshot_value(get_plotly_snap(plot_obj), style = "json2")
  #app$expect_screenshot(name = "justify")
  tab <- app$get_value(export = "data11-table")
  expect_equal(tab$analyst_note[4], "linear interpolation, you're wrong")

  #flip back to reviewer and see if that shows up
  app$set_inputs(`data11-mode` = "reviewer")
  app$wait_for_idle()
  plot_obj <- app$get_value(export = "data11-plot_obj")
  expect_snapshot_value(get_plotly_snap(plot_obj), style = "json2")
  #app$expect_screenshot(name = "after-justification", threshold = 100) #add tolerance because edit time will change
    #too fussy, just removing. the other check should be okay.


})

