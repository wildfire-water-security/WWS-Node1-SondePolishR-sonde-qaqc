

test_that("{shinytest2} recording: checking-module1", {
  library(shinytest2)
  library(shiny)
  app_dir <- system.file("app", package = "SondePolishR")
  local_app_support(app_dir)
  app <- AppDriver$new(app_dir, variant = platform_variant(),
                       name = "m1", height = 911, width = 1619)

  #upload files
  app$upload_file(`data1-csv_files` = file.path(test_path(), "testdata", c("example-csv-data1.csv", "example-csv-data2.csv")))

  #set site name
  app$set_inputs(`data1-site` = "TEST")
  app$set_inputs(`data1-tz` = "Etc/GMT+4")

  #click to load files and create project
  app$click("data1-load_prj", timeout_=20000)

  #upload user precip files
  app$set_inputs(`data1-precip_source` = "upload")
  app$upload_file(`data1-precip_file` = file.path(test_path(), "testdata", "example-precip.csv"))
  app$click("data1-load_precip")

  #update unbound `input` value
  app$wait_for_idle()
  proj <- app$get_value(export = "data1-proj")
  precip <- proj$precip
  expect_s3_class(proj, "sondeproj")
  expect_true(is.data.frame(precip))
  expect_equal(ncol(precip), 2)
  expect_equal(proj$meta$site, "TEST")
  expect_equal(proj$meta$tz, "Etc/GMT+4")

})
