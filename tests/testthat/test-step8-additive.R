library(shinytest2)
library(shiny)

test_that("{shinytest2} recording: checking-module8", {
  app_dir <- system.file("app", package = "SondePolishR")
  local_app_support(app_dir)
  app <- AppDriver$new(app_dir, variant = platform_variant(),
                       name = "m8", height = 911, width = 1619)
  app$upload_file(`data1-pj_file` = file.path(test_path(), "testdata", "example-sonde-project.RDS"))

  #click to load files and create project
  app$click("data1-load_prj")

  app$set_inputs(modules = "step-8") #for screenshots of what is happening

 #testing additive shift
  app$set_inputs(`data8-edit_type` = "additive")
  app$wait_for_idle()
  app$expect_values(input =c("data8-slope", "data8-int"), name="start_shift_val") #should be zero

 # #select a point for shifting (super gross but from shinytests2)
  app$set_inputs(`plotly_selected-shift_plot` = "[{\"curveNumber\":0,\"pointNumber\":2075,\"x\":\"2024-08-22 16:15\",\"y\":170.61}]", allow_no_input_binding_ = TRUE, priority_ = "event")
  app$wait_for_idle()

  #check resulting plot
    plot_obj <- app$get_value(export = "data8-plot_obj")
    expect_snapshot_value(get_plotly_snap(plot_obj), style = "json2")
    app$expect_screenshot(name = "select_single_point")
    app$expect_values(input =c("data8-slope", "data8-int"), name="shift_val")

  #select a different point (clearing seems to make shinytests mad)
    app$set_inputs(`plotly_selected-shift_plot` = "[{\"curveNumber\":0,\"pointNumber\":10418,\"x\":\"2024-11-17 14:30\",\"y\":0.08}]", allow_no_input_binding_ = TRUE, priority_ = "event")
    app$wait_for_idle()
    plot_obj <- app$get_value(export = "data8-plot_obj")
    expect_snapshot_value(get_plotly_snap(plot_obj), style = "json2")
    app$expect_screenshot(name = "select_diffsingle_point")

  #make sure the same thing works with weekly view
    app$set_inputs(`data8-date_nav-period_view` = TRUE)
    app$click("data8-date_nav-next_period")
    app$click("data8-date_nav-next_period")
    app$click("data8-date_nav-next_period")

    #reselect
    app$set_inputs(`plotly_selected-shift_plot` = "[{\"curveNumber\":0,\"pointNumber\":2075,\"x\":\"2024-08-22 16:15\",\"y\":170.61}]", allow_no_input_binding_ = TRUE, priority_ = "event")
    app$wait_for_idle()
    plot_obj <- app$get_value(export = "data8-plot_obj")
    expect_snapshot_value(get_plotly_snap(plot_obj), style = "json2")
    app$expect_screenshot(name = "select_single_point_weekly")

    #flag values
    app$click("data8-apply_limits-apply_flags")
    app$set_inputs(`data8-date_nav-period_view` = FALSE)
    app$wait_for_idle()
    plot_obj <- app$get_value(export = "data8-plot_obj")
    expect_snapshot_value(get_plotly_snap(plot_obj), style = "json2")
    app$expect_screenshot(name = "additive_after_flagging")
    tab <- app$get_value(export = "data8-changelog")

    expect_true(nrow(tab) > nrow(example_sondeproj$changelog))
    expect_equal(tab$parameter[nrow(tab)], "fDOM_QSU")
    expect_equal(tab$note[nrow(tab)], paste0("shift with slope ", 0," and intercept ", -160.445))

 #test drift correction
  #at full scale
    app$set_inputs(`data8-edit_type` = "drift")
    app$wait_for_idle(timeout = 10000)

    app$set_inputs(`data8-file` = "example-csv-data2.csv")
    app$wait_for_idle()

    app$expect_values(input =c("data8-uncorrect", "data8-correct"), name="drift_values")
    app$expect_values(input = "data8-edit_type", name="edit_type")

    plot_obj <- app$get_value(export = "data8-plot_obj")  #not showing drift correction
    expect_snapshot_value(get_plotly_snap(plot_obj), style = "json2")
    app$expect_screenshot(name = "drift_fullview")

  #at weekly scale
    app$set_inputs(`data8-date_nav-period_view` = TRUE)
    for(x in 1:12){
      app$click("data8-date_nav-next_period")
    }

    app$wait_for_idle()

    plot_obj <- app$get_value(export = "data8-plot_obj")
    expect_snapshot_value(get_plotly_snap(plot_obj), style = "json2")
    app$expect_screenshot(name = "drift_weekview")

  #flag values
    app$click("data8-apply_limits-apply_flags")
    plot_obj <- app$get_value(export = "data8-plot_obj")
    expect_snapshot_value(get_plotly_snap(plot_obj), style = "json2")
    app$expect_screenshot(name = "drift_after_flagging")

    tab <- app$get_value(export = "data8-changelog")
    expect_true(nrow(tab) > nrow(example_sondeproj$changelog))
    expect_equal(tab$parameter[nrow(tab)], "fDOM_QSU")
    expect_equal(tab$note[nrow(tab)], paste0("drift correction based on an uncorrected value of ",
                                             21.48," and corrected value of ", 17.72,
                                             " for file ", "example-csv-data2.csv"))

})

