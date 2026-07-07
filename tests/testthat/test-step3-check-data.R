library(shinytest2)
library(shiny)

test_that("{shinytest2} recording: checking-module3", {

  app_dir <- system.file("app", package = "SondePolishR")
  local_app_support(app_dir)
  app <- AppDriver$new(app_dir, variant = platform_variant(),
                       name = "m3", height = 911, width = 1619)

#TEST 1: testing example data with no gaps/dups
  #upload files
  app$upload_file(`data1-pj_file` = file.path(test_path(), "testdata", "example-sonde-project.RDS"))

  #click to load files and create project
  app$click("data1-load_prj")

  #flip to other screen
  app$set_inputs(modules = "step-3") ##I'm shocked this worked
  app$wait_for_idle() #for a consistent screenshot

  #check initial table
    tab <- app$get_value(export = "data3-table")
    expect_equal(tab, NULL)
    app$expect_screenshot(name="dup_message") #make sure message prints

  #switch to gap table
    app$set_inputs(`data3-table_opt` = "Gaps")
    tab <- app$get_value(export = "data3-table")
    expect_equal(tab, NULL)
    app$expect_screenshot(name="gap_message") #make sure message prints

#TEST 2: testing example data with gaps and dupes
    app$upload_file(`data1-pj_file` = file.path(test_path(), "testdata", "example-sondeproj-messy.RDS"))

    #click to load files and create project
    app$click("data1-load_prj")
    app$wait_for_idle() #for a consistent screenshot

    #check initial table
    app$set_inputs(`data3-table_opt` = "Duplicates")
    tab <- app$get_value(export = "data3-table")
    expect_equal(nrow(tab), 2)
    app$expect_screenshot(name="dup_table") #make sure table prints

    #switch to gap table
    app$set_inputs(`data3-table_opt` = "Gaps")
    tab <- app$get_value(export = "data3-table")
    expect_equal(nrow(tab), 1)
    app$expect_screenshot(name="gap_table") #make sure table prints

#TEST 3: dealing with dupes
  app$set_inputs(`data3-table_opt` = "Duplicates")

  #select first row
  app$set_inputs(`data3-change_table_rows_selected` = 1, allow_no_input_binding_ = TRUE)

  #check for plot
    plot_obj <- app$get_value(export = "data3-plot_obj")
    expect_snapshot_value(get_plotly_snap(plot_obj), style = "json2")
    app$expect_screenshot(name = "dup1_plot_works")

  #change row, expect a different plot
    app$set_inputs(`data3-change_table_rows_selected` = 2, allow_no_input_binding_ = TRUE)
    plot_obj <- app$get_value(export = "data3-plot_obj")
    expect_snapshot_value(get_plotly_snap(plot_obj), style = "json2")
    app$expect_screenshot(name = "dup2_plot_works")

  #select option and deal with dup (don't need to test cases because those checked in non-shiny tests)
    app$set_inputs(`data3-keep_opt` = "use_mean")
    app$click("data3-apply_dup")

  #check that row is removed from table
    tab <- app$get_value(export = "data3-table")
    expect_equal(nrow(tab), 1)
    app$expect_screenshot(name="removing_rows") #make sure table prints


})
