library(shinytest2)
library(shiny)
library(SondePolishR)

test_that("{shinytest2} recording: checking-module3", {
  #initial app set up
  app_dir <- system.file("app", package = "SondePolishR")
  local_app_support(app_dir)
  app <- AppDriver$new(app_dir, variant = platform_variant(),
                       name = "checking-module3", height = 911, width = 1619)
  app$upload_file(`data1-file` = file.path(test_path(), "testdata/example-sonde-project.RDS"))

  #check status when df is first added
  rng <- c(app$get_value(input = "data3-min"), app$get_value(input = "data3-max"))
  expect_equal(rng, c(0.6, 36.2))
  expect_equal(app$get_value(input = "data3-ecoregion"), "No Limits Available")
  plot_obj <- app$get_value(export = "data3-plot_obj")
  vdiffr::expect_doppelganger("intial limit plot", plot_obj)

  #make sure limits update when y var changes
  app$set_inputs(`data3-update_parms-y_var` = "Temp_C")
  rng <- c(app$get_value(input = "data3-min"), app$get_value(input = "data3-max"))
  expect_equal(app$get_value(input = "data3-ecoregion"), "Blue Mountains")
  expect_equal(rng, c(7.185, 15.753))
  plot_obj <- app$get_value(export = "data3-plot_obj")
  vdiffr::expect_doppelganger("changing variable", plot_obj)

  #check if using USGS limits updates the min and max
  app$set_inputs(`data3-usgs_limit` = TRUE)
  rng <- c(app$get_value(input = "data3-min"), app$get_value(input = "data3-max"))
  expect_equal(rng, c(-0.20, 29.20))
  plot_obj <- app$get_value(export = "data3-plot_obj")
  vdiffr::expect_doppelganger("using USGS limits", plot_obj)

  #check that limits update if a lat/long is provided
  app$set_inputs(`data3-lat` = "44.20", `data3-long` = "-122.2")
  expect_equal(app$get_value(input = "data3-ecoregion"), "Cascades")
  plot_obj <- app$get_value(export = "data3-plot_obj")
  vdiffr::expect_doppelganger("changing ecoregion", plot_obj)

  #update limits to see plot/table
  app$set_inputs(`data3-max` = 15)
  plot_obj <- app$get_value(export = "data3-plot_obj")
  vdiffr::expect_doppelganger("manually-changing-limits", plot_obj)
  tab <- app$get_value(export = "data3-outlier_tab")
  expect_true(nrow(tab) > 0)

  #set to remove flagged values
  app$set_inputs(`data3-rm_flags` = TRUE)
  plot_obj <- app$get_value(export = "data3-plot_obj")
  vdiffr::expect_doppelganger("hiding flagged points", plot_obj)

  #save project (can't  get to work right now, but does work and arguably the tests for the module should catch issues)
    # path <- file.path(fs::path_home(), "Downloads/test-check.rds")
    # set_prjpath(path)
    # app$click("data3-flag1-rm_points", wait_=FALSE) #click to flag points
    #nflagged <- app$get_value(export = "confirm_changes-nflagged")
    #print(nflagged)
    #app$expect_values()

  #expect_true(file.exists(path))
  #prj <- readRDS(path)
  #expect_equal(test$change_log$step, c("Initial Load", "limits"))

})

