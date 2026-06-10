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

  app$set_inputs(modules = "step-5") #for screenshots of what is happening

 #testing additive shift
  app$set_inputs(`data5-edit_type` = "additive")
  app$wait_for_idle()
  app$expect_values(input =c("data5-slope", "data5-int")) #should be zero

 #select a point for shifting (super gross but from shinytests)
   app$set_inputs(`plotly_selected-shift_plot` = "[{\"curveNumber\":0,\"pointNumber\":2128,\"x\":1724372100,\"y\":170.61}]", allow_no_input_binding_ = TRUE, priority_ = "event")
   app$wait_for_idle()

  #check resulting plot
    plot_obj <- app$get_value(export = "data5-plot_obj")
    vdiffr::expect_doppelganger("selecting a single point", plot_obj)
    app$expect_values(input =c("data5-slope", "data5-int"))

  #select a different point (clearing seems to make shinytests mad)
    app$set_inputs(`plotly_selected-shift_plot` = "[{\"curveNumber\":0,\"pointNumber\":10471,\"x\":1731882600,\"y\":0.08}]", allow_no_input_binding_ = TRUE, priority_ = "event")
    app$wait_for_idle()
    plot_obj <- app$get_value(export = "data5-plot_obj")
    vdiffr::expect_doppelganger("selecting a different point works", plot_obj)

  #make sure the same thing works with weekly view
    app$set_inputs(`data5-date_nav-week_view` = TRUE)
    app$click("data5-date_nav-next_week")
    app$click("data5-date_nav-next_week")
    app$click("data5-date_nav-next_week")

    #reselect
    app$set_inputs(`plotly_selected-shift_plot` = "[{\"curveNumber\":0,\"pointNumber\":2128,\"x\":1724372100,\"y\":170.61}]", allow_no_input_binding_ = TRUE, priority_ = "event")
    app$wait_for_idle()
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

 #test drift correction
  #at full scale
    app$set_inputs(`data5-date_nav-week_view` = FALSE)
    app$set_inputs(`data5-edit_type` = "drift")
    app$wait_for_idle(timeout = 10000)

    app$set_inputs(`data5-file` = "example-csv-data2.csv")
    app$wait_for_idle()

    app$expect_values(input =c("data5-uncorrect", "data5-correct"))
    app$expect_values(input = "data5-edit_type")

    plot_obj <- app$get_value(export = "data5-plot_obj")  #not showing drift correction
    vdiffr::expect_doppelganger("drift correction with full view", plot_obj)

  #at weekly scale
    app$set_inputs(`data5-date_nav-week_view` = TRUE)
    for(x in 1:12){
      app$click("data5-date_nav-next_week")
    }

    app$wait_for_idle()

    plot_obj <- app$get_value(export = "data5-plot_obj")
    vdiffr::expect_doppelganger("drift correction with weekly view", plot_obj)

  #flag values
    app$click("data5-apply_limits-apply_flags")
    plot_obj <- app$get_value(export = "data5-plot_obj")
    vdiffr::expect_doppelganger("after drift values are flagged", plot_obj)
    tab <- app$get_value(export = "data5-changelog")
    expect_true(nrow(tab) > nrow(example_sondeproj$changelog))
    expect_equal(tab$parameter[nrow(tab)], "fDOM_QSU")
    expect_equal(tab$note[nrow(tab)], paste0("drift correction based on an uncorrected value of ",
                                             21.48," and corrected value of ", 17.72,
                                             " for file ", "example-csv-data2.csv"))

})

