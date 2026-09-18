test_that("project is loaded and merged correctly", {
  #TEST 1: testing "normal"
    csv_files <- "example-csv-data3.csv"
    csv_path <- file.path(test_path(), "testdata", csv_files)
    prj_path <- file.path(test_path(), "testdata", "example-sonde-project.RDS")
    ff_path <- file.path(test_path(), "testdata", "example-fieldform.csv")
    cc_path <- file.path(test_path(), "testdata", "example-calcheck.csv")

    proj <- load_project(csv_path, csv_files, prj_path,ff_path, cc_path, site="test")

      #make sure things look as expected
      expect_s3_class(proj, "sondeproj")
      expect_s3_class(proj$data, "data.frame")
      expect_s3_class(proj$fieldform, "data.frame")
      expect_s3_class(proj$calcheck, "data.frame")
      expect_type(proj$diffs, "list")
      expect_s3_class(proj$changelog, "data.frame")
      expect_equal(proj$meta, list(site="FAL", tz="Etc/GMT+8", coords=c(43.96, -122.63),
                                   pkg_version = packageVersion("SondePolishR"))) #should maintain from example

      #make sure data merged
      expect_equal(nrow(proj$data), 14539)
      expect_equal(length(get_parms(proj$data, flags=TRUE)),12)
      expect_equal(nrow(proj$changelog), 6) #no longer merges in since we have all the data in the original project
      expect_equal(length(proj$diffs), 5)

  # TEST 2: testing with only csv files
      csv_files <- c("example-csv-data1.csv", "example-csv-data2.csv")
      csv_path <- file.path(test_path(), "testdata", csv_files)
      prj_path <- NULL
      ff_path <- file.path(test_path(), "testdata", "example-fieldform.csv")
      cc_path <- file.path(test_path(), "testdata", "example-calcheck.csv")

      proj <- load_project(csv_path, csv_files, prj_path,ff_path, cc_path, site="test", username="Smith")

      #make sure things look as expected
      expect_s3_class(proj, "sondeproj")
      expect_s3_class(proj$data, "data.frame")
      expect_s3_class(proj$fieldform, "data.frame")
      expect_s3_class(proj$calcheck, "data.frame")
      expect_type(proj$diffs, "list")
      expect_s3_class(proj$changelog, "data.frame")
      expect_equal(proj$meta, list(site="test", tz="Etc/GMT+8", coords=c(NA, NA),
                                   pkg_version = packageVersion("SondePolishR"))) #should write new

      #make sure data merged
      expect_equal(nrow(proj$data), 8072) #expect csv1 + csv2 rows
      expect_equal(length(get_parms(proj$data, flags=TRUE)),12)
      expect_equal(nrow(proj$changelog), 2)
      expect_equal(length(proj$diffs), 1)

  # TEST 3: testing with only project file
      csv_files <- NULL
      csv_path <- NULL
      prj_path <- file.path(test_path(), "testdata", "example-sonde-project.RDS")
      ff_path <- file.path(test_path(), "testdata", "example-fieldform.csv")
      cc_path <- file.path(test_path(), "testdata", "example-calcheck.csv")

      proj <- load_project(csv_path, csv_files, prj_path,ff_path, cc_path, username="Smith")

      #make sure things look as expected
      expect_s3_class(proj, "sondeproj")
      expect_s3_class(proj$data, "data.frame")
      expect_s3_class(proj$fieldform, "data.frame")
      expect_s3_class(proj$calcheck, "data.frame")
      expect_type(proj$diffs, "list")
      expect_s3_class(proj$changelog, "data.frame")

      #make sure data merged
      expect_equal(nrow(proj$data), 14539) #expect csv1 + csv2 +csv3 rows
      expect_equal(length(get_parms(proj$data, flags=TRUE)),12)
      expect_equal(nrow(proj$changelog), 6)
      expect_equal(length(proj$diffs), 5)

  #TEST 4: make sure it doesn't fail when metadata isn't specified
      csv_files <- c("example-csv-data1.csv", "example-csv-data2.csv")
      csv_path <- file.path(test_path(), "testdata", csv_files)
      prj_path <- NULL
      ff_path <- NULL
      cc_path <- NULL

      proj <- load_project(csv_path, csv_files, prj_path,ff_path, cc_path, username="Smith")

      #make sure things look as expected
      expect_s3_class(proj, "sondeproj")
      expect_s3_class(proj$data, "data.frame")
      expect_true(is.null(proj$fieldform))
      expect_true(is.null(proj$calcheck))
      expect_type(proj$diffs, "list")
      expect_s3_class(proj$changelog, "data.frame")


  #TEST 5: test with an existing project that requires a merge
      proj1 <- load_project(csv_path = test_path("testdata", "example-csv-data1.csv"), csv_files = paste0("example-csv-data", 1, ".csv"),
                            tz = "Etc/GMT+8", site="FAL", username="Smith")
      temp_rds <- tempfile(fileext=".rds")
      saveRDS(proj1, temp_rds)

      #add a new file
      proj <- load_project(csv_path = test_path("testdata", "example-csv-data2.csv"), csv_files = paste0("example-csv-data", 2, ".csv"),
                            tz = "Etc/GMT+8", site="FAL", prj_path=temp_rds, username="Smith")

      #make sure things look as expected
      expect_s3_class(proj, "sondeproj")
      expect_s3_class(proj$data, "data.frame")
      expect_type(proj$diffs, "list")
      expect_s3_class(proj$changelog, "data.frame")

      #make sure data merged
      expect_equal(nrow(proj$data), 8072) #expect csv1 + csv2 rows
      expect_equal(length(get_parms(proj$data, flags=TRUE)),12)
      expect_equal(nrow(proj$changelog), 3)
      expect_equal(length(proj$diffs), 2)
      expect_equal(proj$changelog$step[2], "adding new data") #since we're already adding data we'll wrap up the filling rows in here

})
