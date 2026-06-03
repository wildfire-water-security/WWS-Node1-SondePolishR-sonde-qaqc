library(shinytest2)
library(shiny)

test_that("{shinytest2} recording: checking-module6", {
  app_dir <- system.file("app", package = "SondePolishR")
  local_app_support(app_dir)
  app <- AppDriver$new(app_dir, variant = platform_variant(),
                       name = "checking-plotting-module", height = 911, width = 1619,
                       expect_values_screenshot_args = FALSE)
  app$upload_file(`data1-pj_file` = file.path(test_path(), "testdata", "example-sonde-project.RDS"))

  #click to load files and create project
  app$click("data1-load_prj")

  #check initial plot
  plot_obj <- app$get_value(export = "data6-plot_obj")
  vdiffr::expect_doppelganger("intial outlier plot", plot_obj)

  #test changing method
  app$set_inputs(`data6-filter_type` = "rel_change")
  plot_obj <- app$get_value(export = "data6-plot_obj")
  vdiffr::expect_doppelganger("switching to rel_change", plot_obj)
  app$set_inputs(`data6-filter_type` = "hampel")

  #test changing t
  app$set_inputs(`data6-t` = 10)
  plot_obj <- app$get_value(export = "data6-plot_obj")
  vdiffr::expect_doppelganger("changing t-value", plot_obj)
  app$set_inputs(`data6-t` = 2)

  #test changing k
  app$set_inputs(`data6-k` = 100)
  plot_obj <- app$get_value(export = "data6-plot_obj")
  vdiffr::expect_doppelganger("changing k-value", plot_obj)
  app$set_inputs(`data6-k` = 5)

  #test adding points manually
  app$set_inputs(`plotly_relayout-outlier_plot` = "{\"selections\":[{\"xref\":\"x\",\"yref\":\"y\",\"line\":{\"width\":1,\"dash\":\"dot\"},\"type\":\"rect\",\"x0\":1734824201.811106,\"y0\":4.604372018648467,\"x1\":1735493003.8557155,\"y1\":-5.195602257878866}]}", allow_no_input_binding_ = TRUE, priority_ = "event")
  plot_obj <- app$get_value(export = "data6-plot_obj")
  vdiffr::expect_doppelganger("adding points manually", plot_obj)

  #test removing points manually
  app$set_inputs(`data6-selection_mode` = "remove")
  app$set_inputs(`plotly_relayout-outlier_plot` = "{\"selections\":[{\"xref\":\"x\",\"yref\":\"y\",\"line\":{\"width\":1,\"dash\":\"dot\"},\"type\":\"rect\",\"x0\":1724361936.1822484,\"y0\":172.62728456913828,\"x1\":1724379852.717518,\"y1\":164.8163587174349}]}", allow_no_input_binding_ = TRUE, priority_ = "event")
  plot_obj <- app$get_value(export = "data6-plot_obj")
  vdiffr::expect_doppelganger("removing points manually", plot_obj)

  #test doing the same set of points twice
  app$set_inputs(`data6-selection_mode` = "add")
  app$set_inputs(`plotly_relayout-outlier_plot` = "{\"selections\":[{\"xref\":\"x\",\"yref\":\"y\",\"line\":{\"width\":1,\"dash\":\"dot\"},\"type\":\"rect\",\"x0\":1734824201.811106,\"y0\":4.604372018648467,\"x1\":1735493003.8557155,\"y1\":-5.195602257878866}]}", allow_no_input_binding_ = TRUE, priority_ = "event")
  app$set_inputs(`plotly_relayout-outlier_plot` = "{\"selections\":[{\"xref\":\"x\",\"yref\":\"y\",\"line\":{\"width\":1,\"dash\":\"dot\"},\"type\":\"rect\",\"x0\":1734824201.811106,\"y0\":4.604372018648467,\"x1\":1735493003.8557155,\"y1\":-5.195602257878866}]}", allow_no_input_binding_ = TRUE, priority_ = "event")
  plot_obj <- app$get_value(export = "data6-plot_obj")
  vdiffr::expect_doppelganger("adding points manually (twice)", plot_obj)

  #test removing flags
  app$set_inputs(`data6-rm_flags` = TRUE)
  plot_obj <- app$get_value(export = "data6-plot_obj")
  vdiffr::expect_doppelganger("hiding flagged points", plot_obj)

  #test removing point while flags removed
  app$set_inputs(`data6-selection_mode` = "remove")
  app$set_inputs(`plotly_relayout-outlier_plot` = "{\"selections\":[{\"xref\":\"x\",\"yref\":\"y\",\"line\":{\"width\":1,\"dash\":\"dot\"},\"type\":\"rect\",\"x0\":1734824201.811106,\"y0\":4.604372018648467,\"x1\":1735493003.8557155,\"y1\":-5.195602257878866}]}", allow_no_input_binding_ = TRUE, priority_ = "event")
  plot_obj <- app$get_value(export = "data6-plot_obj")
  vdiffr::expect_doppelganger("removing points with hidden points", plot_obj)


  #test flagging points
  app$click("data6-apply_limits-apply_flags")
  plot_obj <- app$get_value(export = "data6-plot_obj")
  vdiffr::expect_doppelganger("after values are flagged", plot_obj)
  tab <- app$get_value(export = "data6-changelog")
  expect_true(nrow(tab) > nrow(example_sondeproj$changelog))
  expect_equal(tab$parameter[nrow(tab)], "fDOM_QSU")
  expect_equal(tab$note[nrow(tab)],  paste0("Data removed based on ", "Hampel Filter",
                                            " method with a window size of ", 5, " and threshold of ",
                                            2, " paired with manual outlier detection."))

})

