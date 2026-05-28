

test_that("{shinytest2} recording: checking-module1", {
  library(shinytest2)
  library(shiny)
  app_dir <- system.file("app", package = "SondePolishR")
  local_app_support(app_dir)
  app <- AppDriver$new(app_dir, variant = platform_variant(),
                       name = "checking-module1", height = 911, width = 1619)

#TEST 1: testing "normal"
  #upload files
    app$upload_file(`data1-csv_files` = file.path(test_path(), "testdata", "example-csv-data3.csv"))
    app$upload_file(`data1-pj_file` = file.path(test_path(), "testdata", "example-sonde-project.RDS"))
    app$upload_file(`data1-ff_file` = file.path(test_path(), "testdata", "example-fieldform.csv"))
    app$upload_file(`data1-cc_file` = file.path(test_path(), "testdata", "example-calcheck.csv"))

  #click to load files and create project
    app$click("data1-load_prj")

  #update unbound `input` value
    proj <- app$get_value(export = "data1-proj")

    #make sure things look as expected
      expect_s3_class(proj, "sondeproj")
      expect_s3_class(proj$data, "data.frame")
      expect_type(proj$flags, "list")
      expect_s3_class(proj$fieldform, "data.frame")
      expect_s3_class(proj$calcheck, "data.frame")
      expect_type(proj$diffs, "list")
      expect_s3_class(proj$changelog, "data.frame")

    #make sure data merged
      expect_equal(nrow(proj$data), 14528) #expect 8071 + csv-data3 rows
      expect_equal(nrow(proj$changelog), 6)
      expect_equal(length(proj$diffs), 5)

# TEST 2: testing with only csv files
    #reset button to reset files
      app$click("data1-reset")

    #upload files
      app$upload_file(`data1-csv_files` = file.path(test_path(), "testdata", c("example-csv-data1.csv", "example-csv-data2.csv")))
      app$upload_file(`data1-ff_file` = file.path(test_path(), "testdata", "example-fieldform.csv"))
      app$upload_file(`data1-cc_file` = file.path(test_path(), "testdata", "example-calcheck.csv"))

    #click to load files and create project
      app$click("data1-load_prj")

    #update unbound `input` value
      proj <- app$get_value(export = "data1-proj")

    #make sure things look as expected
      expect_s3_class(proj, "sondeproj")
      expect_s3_class(proj$data, "data.frame")
      expect_type(proj$flags, "list")
      expect_s3_class(proj$fieldform, "data.frame")
      expect_s3_class(proj$calcheck, "data.frame")
      expect_type(proj$diffs, "list")
      expect_s3_class(proj$changelog, "data.frame")

    #make sure data merged
      expect_equal(nrow(proj$data), 8071) #expect csv1 + csv2 rows
      expect_equal(nrow(proj$changelog), 1)
      expect_equal(length(proj$diffs), 0)

 # TEST 3: testing with only project file
    #reset button to reset files
      app$click("data1-reset")

    #upload files
      app$upload_file(`data1-pj_file` = file.path(test_path(), "testdata", "example-sonde-project.RDS"))
      app$upload_file(`data1-ff_file` = file.path(test_path(), "testdata", "example-fieldform.csv"))
      app$upload_file(`data1-cc_file` = file.path(test_path(), "testdata", "example-calcheck.csv"))

    #click to load files and create project
      app$click("data1-load_prj")

    #update unbound `input` value
      proj <- app$get_value(export = "data1-proj")

    #make sure things look as expected
      expect_s3_class(proj, "sondeproj")
      expect_s3_class(proj$data, "data.frame")
      expect_type(proj$flags, "list")
      expect_s3_class(proj$fieldform, "data.frame")
      expect_s3_class(proj$calcheck, "data.frame")
      expect_type(proj$diffs, "list")
      expect_s3_class(proj$changelog, "data.frame")

    #make sure data merged
      expect_equal(nrow(proj$data), 8071) #expect csv1 + csv2 rows
      expect_equal(nrow(proj$changelog), 5)
      expect_equal(length(proj$diffs), 4)

})
