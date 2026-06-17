library(shinytest2)
library(shiny)

test_that("{shinytest2} recording: checking-module8", {
  app_dir <- system.file("app", package = "SondePolishR")
  local_app_support(app_dir)
  app <- AppDriver$new(app_dir, variant = platform_variant(),
                       name = "checking-plotting-module", height = 911, width = 1619,
                       expect_values_screenshot_args = FALSE)
  app$upload_file(`data1-pj_file` = file.path(test_path(), "testdata", "example-sonde-project.RDS"))

  #click to load files and create project
  app$click("data1-load_prj")

  app$set_inputs(modules = "step-8")
  app$wait_for_idle()

  #check initial plot
    plot_obj <- app$get_value(export = "data8-plot_obj")
    vdiffr::expect_doppelganger("intial fdom plot", plot_obj)

  #test changing method
    app$set_inputs(`data8-method` = "1p_exponential")
    plot_obj <- app$get_value(export = "data8-plot_obj")
    vdiffr::expect_doppelganger("switching method", plot_obj)

  #test changing parameter
    app$set_inputs(`data8-a` = -0.004)
    plot_obj <- app$get_value(export = "data8-plot_obj")
    vdiffr::expect_doppelganger("changing parameter value", plot_obj)

  #make sure source and things show up correctly
    app$expect_screenshot() #make sure message prints

  #test flagging points
    app$click("data8-apply_limits-apply_flags")
    plot_obj <- app$get_value(export = "data8-plot_obj")
    vdiffr::expect_doppelganger("after values are flagged", plot_obj)
    tab <- app$get_value(export = "data8-changelog")
    expect_true(nrow(tab) > nrow(example_sondeproj$changelog))
    expect_equal(tab$parameter[nrow(tab)], "fDOM_QSU")
    expect_equal(tab$note[nrow(tab)],  "fDOM corrected for temperature (\U03C1 = -0.011) and turbidity using the Exponential (1-parameter) method (a = -0.004)")

})

