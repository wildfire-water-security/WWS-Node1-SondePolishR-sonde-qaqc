library(shinytest2)
library(shiny)
library(SondePolishR)

test_that("{shinytest2} recording: checking-module1", {
  #clear any existing data
  clear_log()
  clear_data()
  clear_prjpath()

  app_dir <- system.file("app", package = "SondePolishR")
  local_app_support(app_dir)
  app <- AppDriver$new(app_dir, variant = platform_variant(),
                       name = "checking-module1", height = 911, width = 1619)

  #load existing file
  app$upload_file(`data1-file` = file.path(test_path(), "testdata/example-sonde-project.RDS"))
  app$expect_values(export = "data1-type")

  #make sure data looks right
  data_head <- app$get_value(export="data")
  expect_s3_class(data_head, "data.frame")

  #make sure path gets saved
  app$expect_values(export = "prj_path")

  #make sure overwriting works
  app$set_inputs(`data1-overwrite` = TRUE)
  app$expect_values(export = "prj_path")

  #make sure log is loaded
  vals <- app$get_values()
  expect_equal(vals$export$log$value$step, c("Initial Load", "test step", "test step2"))

#load new file
  app$upload_file(`data1-file` = file.path(test_path(), "testdata/sonde-example.csv"), timeout_ = 6)
  app$expect_values(export = "data1-type")

  #make sure data looks right
  data_head <- app$get_value(export="data")
  expect_s3_class(data_head, "data.frame")

  #make sure path gets saved
  app$expect_values(export = "prj_path")

  #make sure log is loaded
  #app$expect_values()
  vals <- app$get_values()
  expect_equal(vals$export$log$value$step, "Initial Load")

  #set a save path and make sure prjpath gets updated [can't currently get to work]
  # app$click("data1-save_file", wait_ = FALSE)
  # app$set_inputs(
  #   `data1-save_file-modal` = list(root = "wd",path = character(0)),
  #   allow_no_input_binding_ = TRUE, wait_ = FALSE)
  # app$click("data1-save_file", wait_ = FALSE)
  # test <- app$get_values(output = "data1-path_text_box")
  # print(test)

})
