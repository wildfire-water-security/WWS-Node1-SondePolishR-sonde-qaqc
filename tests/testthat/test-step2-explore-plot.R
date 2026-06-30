library(shinytest2)
library(shiny)



test_that("{shinytest2} recording: checking-module2", {
  app_dir <- system.file("app", package = "SondePolishR")
  local_app_support(app_dir)
  app <- AppDriver$new(app_dir, variant = platform_variant(),
                       name = "checking-plotting-module", height = 911, width = 1619)
  app$upload_file(`data1-pj_file` = file.path(test_path(), "testdata", "example-sonde-project.RDS"))

  #function for checking plotly objects
  check_plotly <- function(plot_obj){
    expect_snapshot_value(
      list(
        names = purrr::map_chr(plot_obj$x$data, "name"),
        traces = length(plot_obj$x$data),
        shapes = length(plot_obj$x$layout$shapes %||% list()),
        y2 = !is.null(plot_obj$x$layout$yaxis2)
      ),
      style = "json2"
    )

    #app$expect_screenshot()
  }

  #click to load files and create project
  app$click("data1-load_prj")

  #check initial plot is made
  app$set_inputs(modules = "step-2")
  plot_obj <- app$get_value(export = "data2-plot_obj")
  check_plotly(plot_obj)

  #check putting in week view
  app$set_inputs(`data2-weekly_range-week_view` = TRUE)
  plot_obj <- app$get_value(export = "data2-plot_obj")
  check_plotly(plot_obj)

  #clicking next week
  app$click("data2-weekly_range-next_week")
  rng <- app$get_value(input= "data2-weekly_range-dates")
  expect_equal(rng, as.Date(c("2024-08-07", "2024-08-13")))

  #click previous week
  app$click("data2-weekly_range-prev_week")
  rng <- app$get_value(input= "data2-weekly_range-dates")
  expect_equal(rng, as.Date(c("2024-07-31", "2024-08-06")))

  #unclick weekly and make sure we get the full plot again
  app$set_inputs(`data2-weekly_range-week_view` = FALSE)
  plot_obj <- app$get_value(export = "data2-plot_obj")
  check_plotly(plot_obj)
  rng <- app$get_value(input= "data2-weekly_range-dates")
  expect_equal(rng, as.Date(c("2024-07-31", "2024-12-29")))

  #check changing variable to plot
  app$set_inputs(`data2-update_parms-y_var` = "Temp_C")
  plot_obj <- app$get_value(export = "data2-plot_obj")
  check_plotly(plot_obj)

  #check on the table
  app$expect_values(export = "data2-table")

  #change to get other tables
  app$set_inputs(`data2-table_opt` = "Field Form")
  app$expect_values(export = "data2-table") #fieldform

  app$set_inputs(`data2-table_opt` = "Calibration Check")
  app$expect_values(export = "data2-table") #cal check

})

