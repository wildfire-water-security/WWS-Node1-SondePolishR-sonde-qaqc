library(shinytest2)
library(shiny)

test_that("{shinytest2} recording: checking-module5", {
  app_dir <- system.file("app", package = "SondePolishR")
  local_app_support(app_dir)
  app <- AppDriver$new(app_dir, variant = platform_variant(),
                       name = "checking-plotting-module", height = 911, width = 1619,
                       expect_values_screenshot_args = FALSE)
  app$upload_file(`data1-pj_file` = file.path(test_path(), "testdata", "example-sonde-project.RDS"))

  #click to load files and create project
  app$click("data1-load_prj")

 #testing additive shift
  #select a point for shifting (super gross but from shinytests)
    app$set_inputs(`plotly_selected-shift_plot` = "[{\"curveNumber\":0,\"pointNumber\":2128,\"x\":1724372100,\"y\":170.61}]", allow_no_input_binding_ = TRUE, priority_ = "event")

  #check resulting plot
    plot_obj <- app$get_value(export = "data5-plot_obj")
    vdiffr::expect_doppelganger("selecting a single point", plot_obj)
    app$expect_values(input =c("data5-slope", "data5-int"))

  #make sure the same thing works with weekly view
    app$set_inputs(`data5-date_nav-week_view` = TRUE)
    app$click("data5-date_nav-next_week")
    app$click("data5-date_nav-next_week")
    app$click("data5-date_nav-next_week")

    #clear selection
    app$set_inputs(`plotly_relayout-shift_plot` = "{\"selections\":[{\"xref\":\"x\",\"yref\":\"y\",\"line\":{\"width\":1,\"dash\":\"dot\"},\"type\":\"rect\",\"x0\":1724328579.869602,\"y0\":17.198382765531065,\"x1\":1724419133.4492064,\"y1\":16.18095991983968}]}", allow_no_input_binding_ = TRUE, priority_ = "event")

    #reselect
    app$set_inputs(`plotly_relayout-shift_plot` = "{\"selections\":[{\"xref\":\"x\",\"yref\":\"y\",\"line\":{\"width\":1,\"dash\":\"dot\"},\"type\":\"rect\",\"x0\":1724361936.1822484,\"y0\":172.62728456913828,\"x1\":1724379852.717518,\"y1\":164.8163587174349}]}", allow_no_input_binding_ = TRUE, priority_ = "event")

    #check plot
    plot_obj <- app$get_value(export = "data5-plot_obj")
    vdiffr::expect_doppelganger("selecting a single point, weekly view", plot_obj)

    #flag values
    app$click("data5-apply_limits-apply_flags")
    plot_obj <- app$get_value(export = "data5-plot_obj")
    vdiffr::expect_doppelganger("after values are flagged", plot_obj)
    tab <- app$get_value(export = "data5-changelog")
    expect_true(nrow(tab) > nrow(example_sondeproj$changelog))
    expect_equal(tab$parameter[nrow(tab)], "fDOM_QSU")
    expect_equal(tab$note[nrow(tab)], paste0("shift with slope ", 0," and intercept ", -160.445))

 #test drift correction (currently doesn't work, because I can't figure out how to trigger drift)
  #at full scale
    app$set_inputs(`data5-date_nav-week_view` = FALSE)

    # app$set_inputs(`data5-edit_type` = character(0))
    # app$set_inputs(`data5-edit_type` = "drift")
     #app$click(selector = "edit_type-drift.accordion-button")
    app$set_inputs("data5-edit_type" = "drift")

    app$set_inputs(`data5-file` = "example-csv-data2.csv")

    app$expect_values(input =c("data5-uncorrect", "data5-correct"))
    app$expect_values(input = "data5-edit_type")
    app$expect_values(output = "data5-edit_type")

    plot_obj <- app$get_value(export = "data5-plot_obj")  #not showing drift correction
    vdiffr::expect_doppelganger("drift correction with full view", plot_obj)

  #at weekly scale
    app$set_inputs(`data5-date_nav-week_view` = TRUE)
    for(x in 1:12){
      app$click("data5-date_nav-next_week")
    }

    plot_obj <- app$get_value(export = "data5-plot_obj")
    vdiffr::expect_doppelganger("drift correction with weekly view", plot_obj)

  #flag values
    # app$click("data5-apply_limits-apply_flags")
    # plot_obj <- app$get_value(export = "data5-plot_obj")
    # vdiffr::expect_doppelganger("after drift values are flagged", plot_obj)
    # tab <- app$get_value(export = "data5-changelog")
    # expect_true(nrow(tab) > nrow(example_sondeproj$changelog))
    # expect_equal(tab$parameter[nrow(tab)], "fDOM_QSU")
    # expect_equal(tab$note[nrow(tab)], paste0("drift correction based on an uncorrected value of ",
    #                                          21.48," and corrected value of ", 17.72,
    #                                          " for file ", "example-csv-data2.csv"))

})

