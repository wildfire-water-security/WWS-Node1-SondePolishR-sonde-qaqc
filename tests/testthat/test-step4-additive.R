library(shinytest2)
library(shiny)
library(SondePolishR)

test_that("{shinytest2} recording: checking-module4", {
  #initial app set up
  app_dir <- system.file("app", package = "SondePolishR")
  local_app_support(app_dir)
  app <- AppDriver$new(app_dir, variant = platform_variant(),
                       name = "checking-module4", height = 911, width = 1619)
  app$upload_file(`data1-file` = file.path(test_path(), "testdata/example-sonde-project.RDS"))

  #check status when df is first added
  rng <- c(app$get_value(input = "data4-int_val"), app$get_value(input = "data4-slope_val"))
  expect_equal(rng, c(0,0))
  plot_obj <- app$get_value(export = "data4-plot_obj")
  vdiffr::expect_doppelganger("intial additive plot", plot_obj)

  #select some points (super long but needed)
  app$set_inputs(`plotly_selected-shift_plot` = "[{\"curveNumber\":0,\"pointNumber\":1590,\"x\":1715250600,\"y\":19.1},{\"curveNumber\":0,\"pointNumber\":1591,\"x\":1715251500,\"y\":19},{\"curveNumber\":0,\"pointNumber\":1592,\"x\":1715252400,\"y\":18.9},{\"curveNumber\":0,\"pointNumber\":1593,\"x\":1715253300,\"y\":18.8},{\"curveNumber\":0,\"pointNumber\":1594,\"x\":1715254200,\"y\":18.7},{\"curveNumber\":0,\"pointNumber\":1595,\"x\":1715255100,\"y\":18.7},{\"curveNumber\":0,\"pointNumber\":1596,\"x\":1715256000,\"y\":18.7},{\"curveNumber\":0,\"pointNumber\":1597,\"x\":1715256900,\"y\":18.6},{\"curveNumber\":0,\"pointNumber\":1598,\"x\":1715257800,\"y\":18.6},{\"curveNumber\":0,\"pointNumber\":1599,\"x\":1715258700,\"y\":18.6},{\"curveNumber\":0,\"pointNumber\":1600,\"x\":1715259600,\"y\":18.5},{\"curveNumber\":0,\"pointNumber\":1601,\"x\":1715260500,\"y\":18.5},{\"curveNumber\":0,\"pointNumber\":1602,\"x\":1715261400,\"y\":18.5},{\"curveNumber\":0,\"pointNumber\":1603,\"x\":1715262300,\"y\":18.4},{\"curveNumber\":0,\"pointNumber\":1604,\"x\":1715263200,\"y\":18.4},{\"curveNumber\":0,\"pointNumber\":1605,\"x\":1715264100,\"y\":18.4},{\"curveNumber\":0,\"pointNumber\":1606,\"x\":1715265000,\"y\":18.4},{\"curveNumber\":0,\"pointNumber\":1607,\"x\":1715265900,\"y\":18.4},{\"curveNumber\":0,\"pointNumber\":1608,\"x\":1715266800,\"y\":18.4},{\"curveNumber\":0,\"pointNumber\":1609,\"x\":1715267700,\"y\":18.4},{\"curveNumber\":0,\"pointNumber\":1610,\"x\":1715268600,\"y\":18.3},{\"curveNumber\":0,\"pointNumber\":1611,\"x\":1715269500,\"y\":18.3},{\"curveNumber\":0,\"pointNumber\":1612,\"x\":1715270400,\"y\":18.3},{\"curveNumber\":0,\"pointNumber\":1613,\"x\":1715271300,\"y\":18.3},{\"curveNumber\":0,\"pointNumber\":1614,\"x\":1715272200,\"y\":18.4},{\"curveNumber\":0,\"pointNumber\":1615,\"x\":1715273100,\"y\":18.4},{\"curveNumber\":0,\"pointNumber\":1616,\"x\":1715274000,\"y\":18.5},{\"curveNumber\":0,\"pointNumber\":1617,\"x\":1715274900,\"y\":18.5},{\"curveNumber\":0,\"pointNumber\":1618,\"x\":1715275800,\"y\":18.6},{\"curveNumber\":0,\"pointNumber\":1619,\"x\":1715276700,\"y\":18.6},{\"curveNumber\":0,\"pointNumber\":1620,\"x\":1715277600,\"y\":18.7},{\"curveNumber\":0,\"pointNumber\":1621,\"x\":1715278500,\"y\":18.8},{\"curveNumber\":0,\"pointNumber\":1622,\"x\":1715279400,\"y\":18.9},{\"curveNumber\":0,\"pointNumber\":1623,\"x\":1715280300,\"y\":19},{\"curveNumber\":0,\"pointNumber\":1624,\"x\":1715281200,\"y\":19.1},{\"curveNumber\":0,\"pointNumber\":1625,\"x\":1715282100,\"y\":19.2},{\"curveNumber\":0,\"pointNumber\":1626,\"x\":1715283000,\"y\":19.3},{\"curveNumber\":0,\"pointNumber\":1627,\"x\":1715283900,\"y\":19.4},{\"curveNumber\":0,\"pointNumber\":1628,\"x\":1715284800,\"y\":19.5},{\"curveNumber\":0,\"pointNumber\":1629,\"x\":1715285700,\"y\":19.5}]", allow_no_input_binding_ = TRUE, priority_ = "event")
  rng <- c(app$get_value(input = "data4-int_val"), app$get_value(input = "data4-slope_val"))
  expect_equal(rng, c(7.4,0.02))
  plot_obj <- app$get_value(export = "data4-plot_obj")
  vdiffr::expect_doppelganger("selecting data to shift plot", plot_obj)

  #test changing y var
  app$set_inputs(`data4-update_parms-y_var` = "nLF_Cond_uS_cm")
  rng <- c(app$get_value(input = "data4-int_val"), app$get_value(input = "data4-slope_val"))
  expect_equal(rng, c(0,0))
  plot_obj <- app$get_value(export = "data4-plot_obj")
  vdiffr::expect_doppelganger("changing yvar plot", plot_obj)


})

