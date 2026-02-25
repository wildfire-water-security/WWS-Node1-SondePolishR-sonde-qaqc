# test_that("module sever 2 works", {
#   #read in project (would be module 1 task)
#   data_prj <- read_project(file.path(test_path(), "testdata", "example-sonde-project.RDS"))[[1]]
#
#   testServer(explore_data_server, {
#
#    #make sure data is created
#     expect_equal(data(), raw_sonde)
#     session$flushReact() #needed here to run reactives
#
#   #make sure plot_data gets written
#     expect_true(is.data.frame(plot_data()))
#
#   #make sure log is gotten
#     expect_true(inherits(output$log_table, "json"))
#
#     #can't easily check plot because it needs y_var
#
#   #check dates
#     #absolute min and max of df
#     expect_equal(date_bounds(), list(min=as.Date(min(raw_sonde$Date_MM_DD_YYYY)), max= as.Date(max(raw_sonde$Date_MM_DD_YYYY))))
#
#   },
#   #these are passed to the module
#   args = list(
#     data = reactive({data_prj}) #expected that data is a reactive value so need to pass it that
#   ))
# })

library(shinytest2)
library(shiny)
library(SondePolishR)


test_that("{shinytest2} recording: checking-module2", {
  app_dir <- system.file("app", package = "SondePolishR")
  local_app_support(app_dir)
  app <- AppDriver$new(app_dir, variant = platform_variant(),
                       name = "checking-module2", height = 911, width = 1619)
  app$upload_file(`data1-file` = file.path(test_path(), "testdata/example-sonde-project.RDS"))

  #check initial plot is made
  plot_obj <- app$get_value(export = "data2-plot_obj")
  vdiffr::expect_doppelganger("initial plot is made", plot_obj)

  #check putting in week view
  app$set_inputs(`data2-week_view` = TRUE)
  plot_obj <- app$get_value(export = "data2-plot_obj")
  vdiffr::expect_doppelganger("week view works", plot_obj)

  #clicking next week
  app$click("data2-next_week")
  rng <- app$get_value(input= "data2-date_range")
  expect_equal(rng, as.Date(c("2024-04-29", "2024-05-06")))

  #click previous week
  app$click("data2-prev_week")
  rng <- app$get_value(input= "data2-date_range")
  expect_equal(rng, as.Date(c("2024-04-22", "2024-04-29")))

  #unclick weekly and make sure we get the full plot again
  app$set_inputs(`data2-week_view` = FALSE)
  plot_obj <- app$get_value(export = "data2-plot_obj")
  vdiffr::expect_doppelganger("removing week view works", plot_obj)
  rng <- app$get_value(input= "data2-date_range")
  expect_equal(rng, as.Date(c("2024-04-22", "2024-05-15")))

  #check changing variable to plot
  app$set_inputs(`data2-update_parms-y_var` = "fDOM_RFU")
  plot_obj <- app$get_value(export = "data2-plot_obj")
  vdiffr::expect_doppelganger("changing plot variable", plot_obj)

  #check on the table
  tab <- app$get_value(export = "data2-log_table")
  expect_true(nrow(tab) == 3)

  #try with other df
  app$upload_file(`data1-file` = file.path(test_path(), "testdata/sonde-example.csv"))
  tab <- app$get_value(export = "data2-log_table")
  expect_true(nrow(tab) == 1)

})

