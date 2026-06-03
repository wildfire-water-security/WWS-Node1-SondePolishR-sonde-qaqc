library(shinytest2)
library(shiny)

test_that("{shinytest2} recording: checking-module3", {

  app_dir <- system.file("app", package = "SondePolishR")
  local_app_support(app_dir)
  app <- AppDriver$new(app_dir, variant = platform_variant(),
                       name = "checking-module1", height = 911, width = 1619)

#TEST 1: testing example data with no gaps/dups
  #upload files
  app$upload_file(`data1-pj_file` = file.path(test_path(), "testdata", "example-sonde-project.RDS"))

  #click to load files and create project
  app$click("data1-load_prj")

  #flip to other screen
  app$set_inputs(modules = "step-3") ##I'm shocked this worked

  #check initial table
    tab <- app$get_value(export = "data3-table")
    expect_equal(tab, NULL)
    app$wait_for_idle() #for a consistent screenshot
    app$expect_screenshot() #make sure message prints

  #switch to gap table
    app$set_inputs(`data3-table_opt` = "Gaps")
    tab <- app$get_value(export = "data3-table")
    expect_equal(tab, NULL)
    app$wait_for_idle() #for a consistent screenshot
    app$expect_screenshot() #make sure message prints

#TEST 2: testing example data with gaps and dupes
    app$upload_file(`data1-pj_file` = file.path(test_path(), "testdata", "example-sondeproj-messy.RDS"))

    #click to load files and create project
    app$click("data1-load_prj")

    #check initial table
    app$set_inputs(`data3-table_opt` = "Duplicates")
    tab <- app$get_value(export = "data3-table")
    expect_equal(nrow(tab), 2)
    #app$wait_for_idle() #for a consistent screenshot
    #app$expect_screenshot() #make sure table prints

    #switch to gap table
    app$set_inputs(`data3-table_opt` = "Gaps")
    tab <- app$get_value(export = "data3-table")
    expect_equal(nrow(tab), 1)
    #app$wait_for_idle() #for a consistent screenshot
    #app$expect_screenshot() #make sure table prints

})
