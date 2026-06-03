library(shinytest2)
library(shiny)

test_that("{shinytest2} recording: checking-module4", {
  app_dir <- system.file("app", package = "SondePolishR")
  local_app_support(app_dir)
  app <- AppDriver$new(app_dir, variant = platform_variant(),
                       name = "checking-plotting-module", height = 911, width = 1619,
                       expect_values_screenshot_args = FALSE)
  app$upload_file(`data1-pj_file` = file.path(test_path(), "testdata", "example-sonde-project.RDS"))

  #click to load files and create project
  app$click("data1-load_prj")

  #check range when df is first added
    rng <- c(app$get_value(input = "data4-min"), app$get_value(input = "data4-max"))
    expect_equal(rng, c(0,300))

  #check initial plot
    plot_obj <- app$get_value(export = "data4-plot_obj")
    vdiffr::expect_doppelganger("intial limit plot", plot_obj)

  #make sure limits update when y var changes
    app$set_inputs(`data4-update_parms-y_var` = "Temp_C")
    rng <- c(app$get_value(input = "data4-min"), app$get_value(input = "data4-max"))
    expect_equal(rng, c(-5,50))

  #update limits to see plot/table
    app$set_inputs(`data4-max` = 15)
    plot_obj <- app$get_value(export = "data4-plot_obj")
    vdiffr::expect_doppelganger("manually-changing-limits", plot_obj)

  #hide flagged values
    app$set_inputs(`data4-rm_flags` = TRUE)
    plot_obj <- app$get_value(export = "data4-plot_obj")
    vdiffr::expect_doppelganger("hiding flagged points", plot_obj)

  #flag values
    app$click("data4-apply_limits-apply_flags")
    plot_obj <- app$get_value(export = "data4-plot_obj")
    vdiffr::expect_doppelganger("after values are flagged", plot_obj)
    tab <- app$get_value(export = "data4-changelog")
    expect_true(nrow(tab) > nrow(example_sondeproj$changelog))
    expect_equal(tab$parameter[nrow(tab)], "Temp_C")
    expect_equal(tab$note[nrow(tab)], paste0("Data removed based on absolute limits of ", -5, " and ", 15))

})

