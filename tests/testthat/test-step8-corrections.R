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

    app$set_inputs(`data8-drift_submod-file` = "example-csv-data2.csv")
    app$wait_for_idle()

    app$expect_values(input =c("data8-uncorrect", "data8-correct"), name="drift_values")
    app$expect_values(input = "data8-edit_type", name="edit_type")

    plot_obj <- app$get_value(export = "data8-plot_obj")  #not showing drift correction
    expect_snapshot_value(get_plotly_snap(plot_obj), style = "json2")
    app$expect_screenshot(name = "drift_fullview")

  #at weekly scale
    app$set_inputs(`data8-date_nav-period_view` = TRUE)
    for(x in 1:9){
      app$click("data8-date_nav-next_period")
    }

    app$wait_for_idle()

    plot_obj <- app$get_value(export = "data8-plot_obj")
    expect_snapshot_value(get_plotly_snap(plot_obj), style = "json2")
    app$expect_screenshot(name = "drift_weekview")

  #flag values
    app$click("data8-apply_limits-apply_flags")
    app$wait_for_idle()
    plot_obj <- app$get_value(export = "data8-plot_obj")
    expect_snapshot_value(get_plotly_snap(plot_obj), style = "json2")
    app$expect_screenshot(name = "drift_after_flagging")

    tab <- app$get_value(export = "data8-changelog")
    expect_true(nrow(tab) > nrow(example_sondeproj$changelog))
    expect_equal(tab$parameter[nrow(tab)], "fDOM_QSU")
    expect_equal(tab$note[nrow(tab)], paste0("drift correction based on an uncorrected value of ",
                                             21.48," and corrected value of ", 17.72,
                                             " for file ", "example-csv-data2.csv"))

  #tests for smoothing function
    app$set_inputs(`data8-date_nav-period_view` = FALSE)

    #initial smoothing -> select a region to smooth
    app$set_inputs(`data8-edit_type` = "smooth")
    app$wait_for_idle(timeout = 10000)
    #select data region
    app$set_inputs(`plotly_selected-shift_plot` = "[{\"curveNumber\":0,\"pointNumber\":13926,\"x\":\"2024-12-23 15:15\",\"y\":13.64},{\"curveNumber\":0,\"pointNumber\":13927,\"x\":\"2024-12-23 15:30\",\"y\":13.65},{\"curveNumber\":0,\"pointNumber\":13928,\"x\":\"2024-12-23 15:45\",\"y\":13.46},{\"curveNumber\":0,\"pointNumber\":13929,\"x\":\"2024-12-23 16:00\",\"y\":13.73},{\"curveNumber\":0,\"pointNumber\":13930,\"x\":\"2024-12-23 16:15\",\"y\":13.29},{\"curveNumber\":0,\"pointNumber\":13931,\"x\":\"2024-12-23 16:30\",\"y\":13.19},{\"curveNumber\":0,\"pointNumber\":13932,\"x\":\"2024-12-23 16:45\",\"y\":13.34},{\"curveNumber\":0,\"pointNumber\":13933,\"x\":\"2024-12-23 17:00\",\"y\":13.2},{\"curveNumber\":0,\"pointNumber\":13934,\"x\":\"2024-12-23 17:15\",\"y\":13.12},{\"curveNumber\":0,\"pointNumber\":13935,\"x\":\"2024-12-23 17:30\",\"y\":13.12},{\"curveNumber\":0,\"pointNumber\":13936,\"x\":\"2024-12-23 17:45\",\"y\":13.18},{\"curveNumber\":0,\"pointNumber\":13937,\"x\":\"2024-12-23 18:00\",\"y\":13.05},{\"curveNumber\":0,\"pointNumber\":13938,\"x\":\"2024-12-23 18:15\",\"y\":12.81},{\"curveNumber\":0,\"pointNumber\":13939,\"x\":\"2024-12-23 18:30\",\"y\":13.23},{\"curveNumber\":0,\"pointNumber\":13940,\"x\":\"2024-12-23 18:45\",\"y\":12.98},{\"curveNumber\":0,\"pointNumber\":13941,\"x\":\"2024-12-23 19:00\",\"y\":12.91},{\"curveNumber\":0,\"pointNumber\":13942,\"x\":\"2024-12-23 19:15\",\"y\":12.71},{\"curveNumber\":0,\"pointNumber\":13943,\"x\":\"2024-12-23 19:30\",\"y\":12.64},{\"curveNumber\":0,\"pointNumber\":13944,\"x\":\"2024-12-23 19:45\",\"y\":12.65},{\"curveNumber\":0,\"pointNumber\":13945,\"x\":\"2024-12-23 20:00\",\"y\":12.64},{\"curveNumber\":0,\"pointNumber\":13946,\"x\":\"2024-12-23 20:15\",\"y\":12.67},{\"curveNumber\":0,\"pointNumber\":13947,\"x\":\"2024-12-23 20:30\",\"y\":12.56},{\"curveNumber\":0,\"pointNumber\":13948,\"x\":\"2024-12-23 20:45\",\"y\":12.39},{\"curveNumber\":0,\"pointNumber\":13949,\"x\":\"2024-12-23 21:00\",\"y\":12.39},{\"curveNumber\":0,\"pointNumber\":13950,\"x\":\"2024-12-23 21:15\",\"y\":12.64},{\"curveNumber\":0,\"pointNumber\":13951,\"x\":\"2024-12-23 21:30\",\"y\":12.51},{\"curveNumber\":0,\"pointNumber\":13952,\"x\":\"2024-12-23 21:45\",\"y\":12.52},{\"curveNumber\":0,\"pointNumber\":13953,\"x\":\"2024-12-23 22:00\",\"y\":12.52},{\"curveNumber\":0,\"pointNumber\":13954,\"x\":\"2024-12-23 22:15\",\"y\":12.27},{\"curveNumber\":0,\"pointNumber\":13955,\"x\":\"2024-12-23 22:30\",\"y\":12.3},{\"curveNumber\":0,\"pointNumber\":13956,\"x\":\"2024-12-23 22:45\",\"y\":12.4},{\"curveNumber\":0,\"pointNumber\":13957,\"x\":\"2024-12-23 23:00\",\"y\":12.38},{\"curveNumber\":0,\"pointNumber\":13958,\"x\":\"2024-12-23 23:15\",\"y\":12.05},{\"curveNumber\":0,\"pointNumber\":13959,\"x\":\"2024-12-23 23:30\",\"y\":12.12},{\"curveNumber\":0,\"pointNumber\":13960,\"x\":\"2024-12-23 23:45\",\"y\":11.79},{\"curveNumber\":0,\"pointNumber\":13961,\"x\":\"2024-12-24\",\"y\":12.14},{\"curveNumber\":0,\"pointNumber\":13962,\"x\":\"2024-12-24 00:15\",\"y\":11.99},{\"curveNumber\":0,\"pointNumber\":13963,\"x\":\"2024-12-24 00:30\",\"y\":12.01},{\"curveNumber\":0,\"pointNumber\":13964,\"x\":\"2024-12-24 00:45\",\"y\":12.41},{\"curveNumber\":0,\"pointNumber\":13965,\"x\":\"2024-12-24 01:00\",\"y\":12.05},{\"curveNumber\":0,\"pointNumber\":13966,\"x\":\"2024-12-24 01:15\",\"y\":12.45},{\"curveNumber\":0,\"pointNumber\":13967,\"x\":\"2024-12-24 01:30\",\"y\":12.69},{\"curveNumber\":0,\"pointNumber\":13968,\"x\":\"2024-12-24 01:45\",\"y\":12.52},{\"curveNumber\":0,\"pointNumber\":13969,\"x\":\"2024-12-24 02:00\",\"y\":12.55},{\"curveNumber\":0,\"pointNumber\":13970,\"x\":\"2024-12-24 02:15\",\"y\":12.58},{\"curveNumber\":0,\"pointNumber\":13971,\"x\":\"2024-12-24 02:30\",\"y\":12.77},{\"curveNumber\":0,\"pointNumber\":13972,\"x\":\"2024-12-24 02:45\",\"y\":12.85},{\"curveNumber\":0,\"pointNumber\":13973,\"x\":\"2024-12-24 03:00\",\"y\":12.93},{\"curveNumber\":0,\"pointNumber\":13974,\"x\":\"2024-12-24 03:15\",\"y\":13.33},{\"curveNumber\":0,\"pointNumber\":13975,\"x\":\"2024-12-24 03:30\",\"y\":13.31},{\"curveNumber\":0,\"pointNumber\":13976,\"x\":\"2024-12-24 03:45\",\"y\":13.37},{\"curveNumber\":0,\"pointNumber\":13977,\"x\":\"2024-12-24 04:00\",\"y\":13.61},{\"curveNumber\":0,\"pointNumber\":13978,\"x\":\"2024-12-24 04:15\",\"y\":13.8},{\"curveNumber\":0,\"pointNumber\":13979,\"x\":\"2024-12-24 04:30\",\"y\":14.26},{\"curveNumber\":0,\"pointNumber\":14036,\"x\":\"2024-12-24 18:45\",\"y\":13.95},{\"curveNumber\":0,\"pointNumber\":14039,\"x\":\"2024-12-24 19:30\",\"y\":13.13},{\"curveNumber\":0,\"pointNumber\":14043,\"x\":\"2024-12-24 20:30\",\"y\":1.72},{\"curveNumber\":0,\"pointNumber\":14045,\"x\":\"2024-12-24 21:00\",\"y\":13.46},{\"curveNumber\":0,\"pointNumber\":14046,\"x\":\"2024-12-24 21:15\",\"y\":-0.84},{\"curveNumber\":0,\"pointNumber\":14047,\"x\":\"2024-12-24 21:30\",\"y\":-0.41},{\"curveNumber\":0,\"pointNumber\":14048,\"x\":\"2024-12-24 21:45\",\"y\":4.17},{\"curveNumber\":0,\"pointNumber\":14049,\"x\":\"2024-12-24 22:00\",\"y\":9.74},{\"curveNumber\":0,\"pointNumber\":14050,\"x\":\"2024-12-24 22:15\",\"y\":-1.56},{\"curveNumber\":0,\"pointNumber\":14051,\"x\":\"2024-12-24 22:30\",\"y\":0.33},{\"curveNumber\":0,\"pointNumber\":14052,\"x\":\"2024-12-24 22:45\",\"y\":-1.53},{\"curveNumber\":0,\"pointNumber\":14053,\"x\":\"2024-12-24 23:00\",\"y\":-2.56},{\"curveNumber\":0,\"pointNumber\":14054,\"x\":\"2024-12-24 23:15\",\"y\":0.67},{\"curveNumber\":0,\"pointNumber\":14055,\"x\":\"2024-12-24 23:30\",\"y\":0.62},{\"curveNumber\":0,\"pointNumber\":14056,\"x\":\"2024-12-24 23:45\",\"y\":-0.25},{\"curveNumber\":0,\"pointNumber\":14057,\"x\":\"2024-12-25\",\"y\":-2.43},{\"curveNumber\":0,\"pointNumber\":14058,\"x\":\"2024-12-25 00:15\",\"y\":-1.75},{\"curveNumber\":0,\"pointNumber\":14059,\"x\":\"2024-12-25 00:30\",\"y\":-0.29},{\"curveNumber\":0,\"pointNumber\":14060,\"x\":\"2024-12-25 00:45\",\"y\":0.94},{\"curveNumber\":0,\"pointNumber\":14061,\"x\":\"2024-12-25 01:00\",\"y\":0.62},{\"curveNumber\":0,\"pointNumber\":14062,\"x\":\"2024-12-25 01:15\",\"y\":4.41},{\"curveNumber\":0,\"pointNumber\":14063,\"x\":\"2024-12-25 01:30\",\"y\":13.09},{\"curveNumber\":0,\"pointNumber\":14064,\"x\":\"2024-12-25 01:45\",\"y\":9.43},{\"curveNumber\":0,\"pointNumber\":14065,\"x\":\"2024-12-25 02:00\",\"y\":10.05},{\"curveNumber\":0,\"pointNumber\":14066,\"x\":\"2024-12-25 02:15\",\"y\":3.82},{\"curveNumber\":0,\"pointNumber\":14067,\"x\":\"2024-12-25 02:30\",\"y\":2.75},{\"curveNumber\":0,\"pointNumber\":14068,\"x\":\"2024-12-25 02:45\",\"y\":11.68},{\"curveNumber\":0,\"pointNumber\":14069,\"x\":\"2024-12-25 03:00\",\"y\":11.97},{\"curveNumber\":0,\"pointNumber\":14070,\"x\":\"2024-12-25 03:15\",\"y\":12.48},{\"curveNumber\":0,\"pointNumber\":14071,\"x\":\"2024-12-25 03:30\",\"y\":12.07},{\"curveNumber\":0,\"pointNumber\":14072,\"x\":\"2024-12-25 03:45\",\"y\":12.21},{\"curveNumber\":0,\"pointNumber\":14073,\"x\":\"2024-12-25 04:00\",\"y\":11.86},{\"curveNumber\":0,\"pointNumber\":14074,\"x\":\"2024-12-25 04:15\",\"y\":11.5},{\"curveNumber\":0,\"pointNumber\":14075,\"x\":\"2024-12-25 04:30\",\"y\":12.63},{\"curveNumber\":0,\"pointNumber\":14076,\"x\":\"2024-12-25 04:45\",\"y\":12.17},{\"curveNumber\":0,\"pointNumber\":14077,\"x\":\"2024-12-25 05:00\",\"y\":12.5},{\"curveNumber\":0,\"pointNumber\":14078,\"x\":\"2024-12-25 05:15\",\"y\":10.75},{\"curveNumber\":0,\"pointNumber\":14079,\"x\":\"2024-12-25 05:30\",\"y\":11.68},{\"curveNumber\":0,\"pointNumber\":14080,\"x\":\"2024-12-25 05:45\",\"y\":10.01},{\"curveNumber\":0,\"pointNumber\":14081,\"x\":\"2024-12-25 06:00\",\"y\":11.29},{\"curveNumber\":0,\"pointNumber\":14082,\"x\":\"2024-12-25 06:15\",\"y\":11.99},{\"curveNumber\":0,\"pointNumber\":14083,\"x\":\"2024-12-25 06:30\",\"y\":11.76},{\"curveNumber\":0,\"pointNumber\":14084,\"x\":\"2024-12-25 06:45\",\"y\":10.79},{\"curveNumber\":0,\"pointNumber\":14085,\"x\":\"2024-12-25 07:00\",\"y\":11.6},{\"curveNumber\":0,\"pointNumber\":14086,\"x\":\"2024-12-25 07:15\",\"y\":10.49},{\"curveNumber\":0,\"pointNumber\":14087,\"x\":\"2024-12-25 07:30\",\"y\":10.87},{\"curveNumber\":0,\"pointNumber\":14088,\"x\":\"2024-12-25 07:45\",\"y\":10.54},{\"curveNumber\":0,\"pointNumber\":14089,\"x\":\"2024-12-25 08:00\",\"y\":8.8},{\"curveNumber\":0,\"pointNumber\":14090,\"x\":\"2024-12-25 08:15\",\"y\":10.52},{\"curveNumber\":0,\"pointNumber\":14091,\"x\":\"2024-12-25 08:30\",\"y\":9.01},{\"curveNumber\":0,\"pointNumber\":14092,\"x\":\"2024-12-25 08:45\",\"y\":10.77},{\"curveNumber\":0,\"pointNumber\":14093,\"x\":\"2024-12-25 09:00\",\"y\":9.48},{\"curveNumber\":0,\"pointNumber\":14094,\"x\":\"2024-12-25 09:15\",\"y\":9.87},{\"curveNumber\":0,\"pointNumber\":14095,\"x\":\"2024-12-25 09:30\",\"y\":10.57},{\"curveNumber\":0,\"pointNumber\":14096,\"x\":\"2024-12-25 09:45\",\"y\":7.59},{\"curveNumber\":0,\"pointNumber\":14097,\"x\":\"2024-12-25 10:00\",\"y\":9.4},{\"curveNumber\":0,\"pointNumber\":14098,\"x\":\"2024-12-25 10:15\",\"y\":1.55},{\"curveNumber\":0,\"pointNumber\":14099,\"x\":\"2024-12-25 10:30\",\"y\":6.91},{\"curveNumber\":0,\"pointNumber\":14100,\"x\":\"2024-12-25 10:45\",\"y\":9.09},{\"curveNumber\":0,\"pointNumber\":14101,\"x\":\"2024-12-25 11:00\",\"y\":8.85},{\"curveNumber\":0,\"pointNumber\":14102,\"x\":\"2024-12-25 11:15\",\"y\":11.68},{\"curveNumber\":0,\"pointNumber\":14103,\"x\":\"2024-12-25 11:30\",\"y\":10.19},{\"curveNumber\":0,\"pointNumber\":14104,\"x\":\"2024-12-25 11:45\",\"y\":8.25},{\"curveNumber\":0,\"pointNumber\":14105,\"x\":\"2024-12-25 12:00\",\"y\":7.6},{\"curveNumber\":0,\"pointNumber\":14106,\"x\":\"2024-12-25 12:15\",\"y\":6.69},{\"curveNumber\":0,\"pointNumber\":14107,\"x\":\"2024-12-25 12:30\",\"y\":6.93},{\"curveNumber\":0,\"pointNumber\":14108,\"x\":\"2024-12-25 12:45\",\"y\":7.33},{\"curveNumber\":0,\"pointNumber\":14109,\"x\":\"2024-12-25 13:00\",\"y\":2.84},{\"curveNumber\":0,\"pointNumber\":14110,\"x\":\"2024-12-25 13:15\",\"y\":8.16},{\"curveNumber\":0,\"pointNumber\":14111,\"x\":\"2024-12-25 13:30\",\"y\":8.63},{\"curveNumber\":0,\"pointNumber\":14112,\"x\":\"2024-12-25 13:45\",\"y\":8.48},{\"curveNumber\":0,\"pointNumber\":14113,\"x\":\"2024-12-25 14:00\",\"y\":10.15},{\"curveNumber\":0,\"pointNumber\":14114,\"x\":\"2024-12-25 14:15\",\"y\":4.38},{\"curveNumber\":0,\"pointNumber\":14115,\"x\":\"2024-12-25 14:30\",\"y\":2.73},{\"curveNumber\":0,\"pointNumber\":14116,\"x\":\"2024-12-25 14:45\",\"y\":7.28},{\"curveNumber\":0,\"pointNumber\":14117,\"x\":\"2024-12-25 15:00\",\"y\":7.19},{\"curveNumber\":0,\"pointNumber\":14118,\"x\":\"2024-12-25 15:15\",\"y\":7.09},{\"curveNumber\":0,\"pointNumber\":14119,\"x\":\"2024-12-25 15:30\",\"y\":7.22},{\"curveNumber\":0,\"pointNumber\":14120,\"x\":\"2024-12-25 15:45\",\"y\":9.35},{\"curveNumber\":0,\"pointNumber\":14121,\"x\":\"2024-12-25 16:00\",\"y\":11.1},{\"curveNumber\":0,\"pointNumber\":14122,\"x\":\"2024-12-25 16:15\",\"y\":4.46},{\"curveNumber\":0,\"pointNumber\":14123,\"x\":\"2024-12-25 16:30\",\"y\":12.1},{\"curveNumber\":0,\"pointNumber\":14124,\"x\":\"2024-12-25 16:45\",\"y\":-1.19},{\"curveNumber\":0,\"pointNumber\":14125,\"x\":\"2024-12-25 17:00\",\"y\":-1.75},{\"curveNumber\":0,\"pointNumber\":14126,\"x\":\"2024-12-25 17:15\",\"y\":-1.7},{\"curveNumber\":0,\"pointNumber\":14127,\"x\":\"2024-12-25 17:30\",\"y\":-0.93},{\"curveNumber\":0,\"pointNumber\":14128,\"x\":\"2024-12-25 17:45\",\"y\":1.93},{\"curveNumber\":0,\"pointNumber\":14129,\"x\":\"2024-12-25 18:00\",\"y\":1.63},{\"curveNumber\":0,\"pointNumber\":14130,\"x\":\"2024-12-25 18:15\",\"y\":1.54},{\"curveNumber\":0,\"pointNumber\":14131,\"x\":\"2024-12-25 18:30\",\"y\":1.51},{\"curveNumber\":0,\"pointNumber\":14132,\"x\":\"2024-12-25 18:45\",\"y\":4.1},{\"curveNumber\":0,\"pointNumber\":14133,\"x\":\"2024-12-25 19:00\",\"y\":3.1},{\"curveNumber\":0,\"pointNumber\":14134,\"x\":\"2024-12-25 19:15\",\"y\":3.18},{\"curveNumber\":0,\"pointNumber\":14135,\"x\":\"2024-12-25 19:30\",\"y\":6.6},{\"curveNumber\":0,\"pointNumber\":14136,\"x\":\"2024-12-25 19:45\",\"y\":7.74},{\"curveNumber\":0,\"pointNumber\":14137,\"x\":\"2024-12-25 20:00\",\"y\":3.33},{\"curveNumber\":0,\"pointNumber\":14138,\"x\":\"2024-12-25 20:15\",\"y\":3.34},{\"curveNumber\":0,\"pointNumber\":14139,\"x\":\"2024-12-25 20:30\",\"y\":10.9},{\"curveNumber\":0,\"pointNumber\":14140,\"x\":\"2024-12-25 20:45\",\"y\":-0.34},{\"curveNumber\":0,\"pointNumber\":14141,\"x\":\"2024-12-25 21:00\",\"y\":4.47},{\"curveNumber\":0,\"pointNumber\":14142,\"x\":\"2024-12-25 21:15\",\"y\":2.16},{\"curveNumber\":0,\"pointNumber\":14143,\"x\":\"2024-12-25 21:30\",\"y\":8.61},{\"curveNumber\":0,\"pointNumber\":14144,\"x\":\"2024-12-25 21:45\",\"y\":8.05},{\"curveNumber\":0,\"pointNumber\":14145,\"x\":\"2024-12-25 22:00\",\"y\":-1.48},{\"curveNumber\":0,\"pointNumber\":14146,\"x\":\"2024-12-25 22:15\",\"y\":2.95},{\"curveNumber\":0,\"pointNumber\":14147,\"x\":\"2024-12-25 22:30\",\"y\":3.46},{\"curveNumber\":0,\"pointNumber\":14148,\"x\":\"2024-12-25 22:45\",\"y\":1.74},{\"curveNumber\":0,\"pointNumber\":14149,\"x\":\"2024-12-25 23:00\",\"y\":3.97},{\"curveNumber\":0,\"pointNumber\":14150,\"x\":\"2024-12-25 23:15\",\"y\":4.04},{\"curveNumber\":0,\"pointNumber\":14151,\"x\":\"2024-12-25 23:30\",\"y\":2.5},{\"curveNumber\":0,\"pointNumber\":14152,\"x\":\"2024-12-25 23:45\",\"y\":7.82},{\"curveNumber\":0,\"pointNumber\":14153,\"x\":\"2024-12-26\",\"y\":1.45},{\"curveNumber\":0,\"pointNumber\":14154,\"x\":\"2024-12-26 00:15\",\"y\":7.12},{\"curveNumber\":0,\"pointNumber\":14155,\"x\":\"2024-12-26 00:30\",\"y\":7.38},{\"curveNumber\":0,\"pointNumber\":14156,\"x\":\"2024-12-26 00:45\",\"y\":9.48},{\"curveNumber\":0,\"pointNumber\":14157,\"x\":\"2024-12-26 01:00\",\"y\":-2.44},{\"curveNumber\":0,\"pointNumber\":14158,\"x\":\"2024-12-26 01:15\",\"y\":0.75},{\"curveNumber\":0,\"pointNumber\":14159,\"x\":\"2024-12-26 01:30\",\"y\":0.06},{\"curveNumber\":0,\"pointNumber\":14160,\"x\":\"2024-12-26 01:45\",\"y\":9.19},{\"curveNumber\":0,\"pointNumber\":14161,\"x\":\"2024-12-26 02:00\",\"y\":-1.56},{\"curveNumber\":0,\"pointNumber\":14162,\"x\":\"2024-12-26 02:15\",\"y\":-2.45},{\"curveNumber\":0,\"pointNumber\":14163,\"x\":\"2024-12-26 02:30\",\"y\":-0.73},{\"curveNumber\":0,\"pointNumber\":14164,\"x\":\"2024-12-26 02:45\",\"y\":5.11},{\"curveNumber\":0,\"pointNumber\":14165,\"x\":\"2024-12-26 03:00\",\"y\":-1.49},{\"curveNumber\":0,\"pointNumber\":14166,\"x\":\"2024-12-26 03:15\",\"y\":9.77},{\"curveNumber\":0,\"pointNumber\":14229,\"x\":\"2024-12-26 19:15\",\"y\":14.41},{\"curveNumber\":0,\"pointNumber\":14230,\"x\":\"2024-12-26 19:30\",\"y\":14.35},{\"curveNumber\":0,\"pointNumber\":14231,\"x\":\"2024-12-26 19:45\",\"y\":14.22},{\"curveNumber\":0,\"pointNumber\":14232,\"x\":\"2024-12-26 20:00\",\"y\":14.15},{\"curveNumber\":0,\"pointNumber\":14233,\"x\":\"2024-12-26 20:15\",\"y\":14.03},{\"curveNumber\":0,\"pointNumber\":14234,\"x\":\"2024-12-26 20:30\",\"y\":13.98},{\"curveNumber\":0,\"pointNumber\":14235,\"x\":\"2024-12-26 20:45\",\"y\":13.88},{\"curveNumber\":0,\"pointNumber\":14236,\"x\":\"2024-12-26 21:00\",\"y\":13.81},{\"curveNumber\":0,\"pointNumber\":14237,\"x\":\"2024-12-26 21:15\",\"y\":13.78},{\"curveNumber\":0,\"pointNumber\":14238,\"x\":\"2024-12-26 21:30\",\"y\":13.7},{\"curveNumber\":0,\"pointNumber\":14239,\"x\":\"2024-12-26 21:45\",\"y\":13.71},{\"curveNumber\":0,\"pointNumber\":14240,\"x\":\"2024-12-26 22:00\",\"y\":13.7},{\"curveNumber\":0,\"pointNumber\":14241,\"x\":\"2024-12-26 22:15\",\"y\":13.62},{\"curveNumber\":0,\"pointNumber\":14242,\"x\":\"2024-12-26 22:30\",\"y\":13.64},{\"curveNumber\":0,\"pointNumber\":14243,\"x\":\"2024-12-26 22:45\",\"y\":13.63},{\"curveNumber\":0,\"pointNumber\":14244,\"x\":\"2024-12-26 23:00\",\"y\":13.58},{\"curveNumber\":0,\"pointNumber\":14245,\"x\":\"2024-12-26 23:15\",\"y\":13.61},{\"curveNumber\":0,\"pointNumber\":14246,\"x\":\"2024-12-26 23:30\",\"y\":13.54},{\"curveNumber\":0,\"pointNumber\":14247,\"x\":\"2024-12-26 23:45\",\"y\":13.55},{\"curveNumber\":0,\"pointNumber\":14248,\"x\":\"2024-12-27\",\"y\":13.59},{\"curveNumber\":0,\"pointNumber\":14249,\"x\":\"2024-12-27 00:15\",\"y\":13.64},{\"curveNumber\":0,\"pointNumber\":14250,\"x\":\"2024-12-27 00:30\",\"y\":13.63},{\"curveNumber\":0,\"pointNumber\":14251,\"x\":\"2024-12-27 00:45\",\"y\":13.66},{\"curveNumber\":0,\"pointNumber\":14252,\"x\":\"2024-12-27 01:00\",\"y\":13.61},{\"curveNumber\":0,\"pointNumber\":14253,\"x\":\"2024-12-27 01:15\",\"y\":13.61},{\"curveNumber\":0,\"pointNumber\":14254,\"x\":\"2024-12-27 01:30\",\"y\":13.58},{\"curveNumber\":0,\"pointNumber\":14255,\"x\":\"2024-12-27 01:45\",\"y\":13.6},{\"curveNumber\":0,\"pointNumber\":14256,\"x\":\"2024-12-27 02:00\",\"y\":13.61},{\"curveNumber\":0,\"pointNumber\":14257,\"x\":\"2024-12-27 02:15\",\"y\":13.6},{\"curveNumber\":0,\"pointNumber\":14258,\"x\":\"2024-12-27 02:30\",\"y\":13.59},{\"curveNumber\":0,\"pointNumber\":14259,\"x\":\"2024-12-27 02:45\",\"y\":13.59},{\"curveNumber\":0,\"pointNumber\":14260,\"x\":\"2024-12-27 03:00\",\"y\":13.54},{\"curveNumber\":0,\"pointNumber\":14261,\"x\":\"2024-12-27 03:15\",\"y\":13.55},{\"curveNumber\":0,\"pointNumber\":14262,\"x\":\"2024-12-27 03:30\",\"y\":13.47},{\"curveNumber\":0,\"pointNumber\":14263,\"x\":\"2024-12-27 03:45\",\"y\":13.32},{\"curveNumber\":0,\"pointNumber\":14264,\"x\":\"2024-12-27 04:00\",\"y\":13.02},{\"curveNumber\":0,\"pointNumber\":14265,\"x\":\"2024-12-27 04:15\",\"y\":12.97},{\"curveNumber\":0,\"pointNumber\":14266,\"x\":\"2024-12-27 04:30\",\"y\":13.09},{\"curveNumber\":0,\"pointNumber\":14267,\"x\":\"2024-12-27 04:45\",\"y\":13.18},{\"curveNumber\":0,\"pointNumber\":14268,\"x\":\"2024-12-27 05:00\",\"y\":13.18},{\"curveNumber\":0,\"pointNumber\":14269,\"x\":\"2024-12-27 05:15\",\"y\":12.92},{\"curveNumber\":0,\"pointNumber\":14270,\"x\":\"2024-12-27 05:30\",\"y\":12.32},{\"curveNumber\":0,\"pointNumber\":14271,\"x\":\"2024-12-27 05:45\",\"y\":11.89},{\"curveNumber\":0,\"pointNumber\":14272,\"x\":\"2024-12-27 06:00\",\"y\":12.24},{\"curveNumber\":0,\"pointNumber\":14273,\"x\":\"2024-12-27 06:15\",\"y\":12.49},{\"curveNumber\":0,\"pointNumber\":14274,\"x\":\"2024-12-27 06:30\",\"y\":12.35},{\"curveNumber\":0,\"pointNumber\":14275,\"x\":\"2024-12-27 06:45\",\"y\":12.16},{\"curveNumber\":0,\"pointNumber\":14276,\"x\":\"2024-12-27 07:00\",\"y\":12.4},{\"curveNumber\":0,\"pointNumber\":14277,\"x\":\"2024-12-27 07:15\",\"y\":12.91},{\"curveNumber\":0,\"pointNumber\":14278,\"x\":\"2024-12-27 07:30\",\"y\":13.09},{\"curveNumber\":0,\"pointNumber\":14279,\"x\":\"2024-12-27 07:45\",\"y\":13.19},{\"curveNumber\":0,\"pointNumber\":14280,\"x\":\"2024-12-27 08:00\",\"y\":13.33},{\"curveNumber\":0,\"pointNumber\":14281,\"x\":\"2024-12-27 08:15\",\"y\":13.43},{\"curveNumber\":0,\"pointNumber\":14282,\"x\":\"2024-12-27 08:30\",\"y\":13.51},{\"curveNumber\":0,\"pointNumber\":14283,\"x\":\"2024-12-27 08:45\",\"y\":13.55},{\"curveNumber\":0,\"pointNumber\":14284,\"x\":\"2024-12-27 09:00\",\"y\":13.68},{\"curveNumber\":0,\"pointNumber\":14285,\"x\":\"2024-12-27 09:15\",\"y\":13.69},{\"curveNumber\":0,\"pointNumber\":14286,\"x\":\"2024-12-27 09:30\",\"y\":13.71},{\"curveNumber\":0,\"pointNumber\":14287,\"x\":\"2024-12-27 10:00\",\"y\":13.85},{\"curveNumber\":0,\"pointNumber\":14288,\"x\":\"2024-12-27 10:15\",\"y\":13.88},{\"curveNumber\":0,\"pointNumber\":14289,\"x\":\"2024-12-27 10:30\",\"y\":13.79},{\"curveNumber\":0,\"pointNumber\":14290,\"x\":\"2024-12-27 10:45\",\"y\":13.82},{\"curveNumber\":0,\"pointNumber\":14291,\"x\":\"2024-12-27 11:00\",\"y\":13.79},{\"curveNumber\":0,\"pointNumber\":14292,\"x\":\"2024-12-27 11:15\",\"y\":13.84},{\"curveNumber\":0,\"pointNumber\":14293,\"x\":\"2024-12-27 11:30\",\"y\":13.79},{\"curveNumber\":0,\"pointNumber\":14294,\"x\":\"2024-12-27 11:45\",\"y\":13.79},{\"curveNumber\":0,\"pointNumber\":14295,\"x\":\"2024-12-27 12:00\",\"y\":13.8},{\"curveNumber\":0,\"pointNumber\":14296,\"x\":\"2024-12-27 12:15\",\"y\":13.66},{\"curveNumber\":0,\"pointNumber\":14297,\"x\":\"2024-12-27 12:30\",\"y\":13.73},{\"curveNumber\":0,\"pointNumber\":14298,\"x\":\"2024-12-27 12:45\",\"y\":13.68},{\"curveNumber\":0,\"pointNumber\":14299,\"x\":\"2024-12-27 13:00\",\"y\":13.61},{\"curveNumber\":0,\"pointNumber\":14300,\"x\":\"2024-12-27 13:15\",\"y\":13.53},{\"curveNumber\":0,\"pointNumber\":14301,\"x\":\"2024-12-27 13:30\",\"y\":13.46},{\"curveNumber\":0,\"pointNumber\":14302,\"x\":\"2024-12-27 13:45\",\"y\":13.43},{\"curveNumber\":0,\"pointNumber\":14303,\"x\":\"2024-12-27 14:00\",\"y\":13.38},{\"curveNumber\":0,\"pointNumber\":14304,\"x\":\"2024-12-27 14:15\",\"y\":13.3},{\"curveNumber\":0,\"pointNumber\":14305,\"x\":\"2024-12-27 14:30\",\"y\":13.28},{\"curveNumber\":0,\"pointNumber\":14306,\"x\":\"2024-12-27 14:45\",\"y\":13.15},{\"curveNumber\":0,\"pointNumber\":14307,\"x\":\"2024-12-27 15:00\",\"y\":13.04},{\"curveNumber\":0,\"pointNumber\":14308,\"x\":\"2024-12-27 15:15\",\"y\":13.03},{\"curveNumber\":0,\"pointNumber\":14309,\"x\":\"2024-12-27 15:30\",\"y\":12.93},{\"curveNumber\":0,\"pointNumber\":14310,\"x\":\"2024-12-27 15:45\",\"y\":12.86},{\"curveNumber\":0,\"pointNumber\":14311,\"x\":\"2024-12-27 16:00\",\"y\":12.78},{\"curveNumber\":0,\"pointNumber\":14312,\"x\":\"2024-12-27 16:15\",\"y\":12.74}]", allow_no_input_binding_ = TRUE, priority_ = "event")
    plot_obj <- app$get_value(export = "data8-plot_obj")
    expect_snapshot_value(get_plotly_snap(plot_obj), style = "json2")
    app$expect_screenshot(name = "initial_smoothing")

    #change method
    app$set_inputs(`data8-smooth_submod-method` = "kalman")
    plot_obj <- app$get_value(export = "data8-plot_obj")
    expect_snapshot_value(get_plotly_snap(plot_obj), style = "json2")
    app$expect_screenshot(name = "change_smooth_mtd")

    #change k
    app$set_inputs(`data8-smooth_submod-smooth_fact` = 55)
    plot_obj <- app$get_value(export = "data8-plot_obj")
    expect_snapshot_value(get_plotly_snap(plot_obj), style = "json2")
    app$expect_screenshot(name = "change_smooth_k")

    #flag data
    app$click("data8-apply_limits-apply_flags")
    app$wait_for_idle()
    plot_obj <- app$get_value(export = "data8-plot_obj")
    expect_snapshot_value(get_plotly_snap(plot_obj), style = "json2")
    app$expect_screenshot(name = "smooth_after_flagging")

    tab <- app$get_value(export = "data8-changelog")
    expect_true(nrow(tab) > nrow(example_sondeproj$changelog))
    expect_equal(tab$parameter[nrow(tab)], "fDOM_QSU")
    expect_equal(tab$note[nrow(tab)], paste0("smoothing correction using ", "Kalman Filter",
                                             " using a smoothing factor of ", 55))

})

