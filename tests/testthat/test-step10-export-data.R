library(shinytest2)
library(shiny)

test_that("{shinytest2} recording: checking-module10", {
  app_dir <- system.file("app", package = "SondePolishR")
  local_app_support(app_dir)
  app <- AppDriver$new(app_dir, variant = platform_variant(),
                       name = "m10", height = 911, width = 1619,
                       expect_values_screenshot_args = FALSE)
  app$upload_file(`data1-pj_file` = file.path(test_path(), "testdata", "example-sonde-project.RDS"))

  #click to load files and create project
  app$click("data1-load_prj")

  app$set_inputs(modules = "step-10")
  app$wait_for_idle()

  #check initial plot
    plot_obj <- app$get_value(export = "data10-plot_obj")
    expect_snapshot_value(get_plotly_snap(plot_obj), style = "json2")
    app$expect_screenshot(name = "initial_plot")

  #test different methods of summarizing
    app$set_inputs(`data10-frequency` = "week")
    plot_obj <- app$get_value(export = "data10-plot_obj")
    expect_snapshot_value(get_plotly_snap(plot_obj), style = "json2")
    app$expect_screenshot(name = "weekly_summary")

    app$set_inputs(`data10-summary_method` = "min")
    plot_obj <- app$get_value(export = "data10-plot_obj")
    expect_snapshot_value(get_plotly_snap(plot_obj), style = "json2")
    app$expect_screenshot(name = "change_min")

  #test changing date range
    app$set_inputs(`data10-frequency` = "day")
    app$set_inputs(`data10-dates` = as.Date(c("2024-08-07", "2024-08-13")))
    plot_obj <- app$get_value(export = "data10-plot_obj")
    expect_snapshot_value(get_plotly_snap(plot_obj), style = "json2")
    app$expect_screenshot(name = "change_daterng")

})

