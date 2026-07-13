library(shinytest2)
library(shiny)

test_that("{shinytest2} recording: checking-module9", {
  app_dir <- system.file("app", package = "SondePolishR")
  local_app_support(app_dir)
  app <- AppDriver$new(app_dir, variant = platform_variant(),
                       name = "m9", height = 911, width = 1619,
                       expect_values_screenshot_args = FALSE)
  app$upload_file(`data1-pj_file` = file.path(test_path(), "testdata", "example-sonde-project.RDS"))

  #click to load files and create project
  app$click("data1-load_prj")

  app$set_inputs(modules = "step-9")
  app$wait_for_idle()

  #check initial plot (temp)
    plot_obj <- app$get_value(export = "data9-plot_obj")
    expect_snapshot_value(get_plotly_snap(plot_obj), style = "json2")
    app$expect_screenshot(name = "initial_plot")
    fdom <- app$get_value(export = "data9-fdom_val")
    corr_fdom <- app$get_value(export = "data9-corr_fdom_val")
    expect_true(any(na.omit(fdom) != na.omit(corr_fdom))) #shouldn't be equal

  #test changing parameter
    app$set_inputs(`data9-rho` = -0.004)
    plot_obj <- app$get_value(export = "data9-plot_obj")
    expect_snapshot_value(get_plotly_snap(plot_obj), style = "json2")
    app$expect_screenshot(name = "change_parm")

  #test changing method
    app$set_inputs(`data9-method` = "1p_exponential", timeout_ = 100000)
    plot_obj <- app$get_value(export = "data9-plot_obj")
    expect_snapshot_value(get_plotly_snap(plot_obj), style = "json2")
    app$expect_screenshot(name = "switching_method_wocorr")
    fdom <- app$get_value(export = "data9-fdom_val")
    corr_fdom <- app$get_value(export = "data9-corr_fdom_val")
    expect_true(all(na.omit(fdom) == na.omit(corr_fdom))) #shouldn't apply any corrections because no temp correction

  #test flagging points
    app$set_inputs(`data9-method` = "temperature")
    app$wait_for_idle()
    app$click("data9-apply_limits-apply_flags")
    plot_obj <- app$get_value(export = "data9-plot_obj")
    expect_snapshot_value(get_plotly_snap(plot_obj), style = "json2")
    app$expect_screenshot(name = "after_tempflagging")

    tab <- app$get_value(export = "data9-changelog")
    expect_true(nrow(tab) > nrow(example_sondeproj$changelog))
    expect_equal(tab$parameter[nrow(tab)], "fDOM_QSU")
    expect_equal(tab$note[nrow(tab)],  "fDOM corrected for temperature (\U03C1 = -0.011)")
    fdom <- app$get_value(export = "data9-fdom_val")
    corr_fdom <- app$get_value(export = "data9-corr_fdom_val")
    expect_true(all(na.omit(fdom) == na.omit(corr_fdom))) #shouldn't apply any corrections because already temp corrected after flagging

  #test changing method
    app$set_inputs(`data9-method` = "1p_exponential")
    plot_obj <- app$get_value(export = "data9-plot_obj")
    expect_snapshot_value(get_plotly_snap(plot_obj), style = "json2")
    app$expect_screenshot(name = "switching_method_wcorr")
    fdom <- app$get_value(export = "data9-fdom_val")
    corr_fdom <- app$get_value(export = "data9-corr_fdom_val")
    expect_true(any(na.omit(fdom) != na.omit(corr_fdom))) #now expect correction to be applied

  #test flagging points
    app$click("data9-apply_limits-apply_flags")
    plot_obj <- app$get_value(export = "data9-plot_obj")
    expect_snapshot_value(get_plotly_snap(plot_obj), style = "json2")
    app$expect_screenshot(name = "after_turbflagging")

    tab <- app$get_value(export = "data9-changelog")
    expect_true(nrow(tab) > nrow(example_sondeproj$changelog))
    expect_equal(tab$parameter[nrow(tab)], "fDOM_QSU")
    expect_equal(tab$note[nrow(tab)],  "fDOM corrected for turbidity (Exponential (1-parameter)) (a = -0.003)")
    fdom <- app$get_value(export = "data9-fdom_val")
    corr_fdom <- app$get_value(export = "data9-corr_fdom_val")
    expect_true(all(na.omit(fdom) == na.omit(corr_fdom))) #shouldn't apply any corrections because already turb corrected after flagging

})

