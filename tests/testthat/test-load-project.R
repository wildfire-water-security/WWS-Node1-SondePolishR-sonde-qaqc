test_that("project is loaded and merged correctly", {
  #TEST 1: testing "normal"
    csv_files <- "example-csv-data3.csv"
    csv_path <- file.path(test_path(), "testdata", csv_files)
    prj_path <- file.path(test_path(), "testdata", "example-sonde-project.RDS")
    ff_path <- file.path(test_path(), "testdata", "example-fieldform.csv")
    cc_path <- file.path(test_path(), "testdata", "example-calcheck.csv")

    proj <- load_project(csv_path, csv_files, prj_path,ff_path, cc_path)

      #make sure things look as expected
      expect_s3_class(proj, "sondeproj")
      expect_s3_class(proj$data, "data.frame")
      expect_type(proj$flags, "list")
      expect_s3_class(proj$fieldform, "data.frame")
      expect_s3_class(proj$calcheck, "data.frame")
      expect_type(proj$diffs, "list")
      expect_s3_class(proj$changelog, "data.frame")

      #make sure data merged
      expect_equal(nrow(proj$data), 14528)
      expect_equal(nrow(proj$changelog), 5) #no longer merges in since we have all the data in the original project
      expect_equal(length(proj$diffs), 4)

  # TEST 2: testing with only csv files
      csv_files <- c("example-csv-data1.csv", "example-csv-data2.csv")
      csv_path <- file.path(test_path(), "testdata", csv_files)
      prj_path <- NULL
      ff_path <- file.path(test_path(), "testdata", "example-fieldform.csv")
      cc_path <- file.path(test_path(), "testdata", "example-calcheck.csv")

      proj <- load_project(csv_path, csv_files, prj_path,ff_path, cc_path)

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
      csv_files <- NULL
      csv_path <- NULL
      prj_path <- file.path(test_path(), "testdata", "example-sonde-project.RDS")
      ff_path <- file.path(test_path(), "testdata", "example-fieldform.csv")
      cc_path <- file.path(test_path(), "testdata", "example-calcheck.csv")

      proj <- load_project(csv_path, csv_files, prj_path,ff_path, cc_path)

      #make sure things look as expected
      expect_s3_class(proj, "sondeproj")
      expect_s3_class(proj$data, "data.frame")
      expect_type(proj$flags, "list")
      expect_s3_class(proj$fieldform, "data.frame")
      expect_s3_class(proj$calcheck, "data.frame")
      expect_type(proj$diffs, "list")
      expect_s3_class(proj$changelog, "data.frame")

      #make sure data merged
      expect_equal(nrow(proj$data), 14528) #expect csv1 + csv2 +csv3 rows
      expect_equal(nrow(proj$changelog), 5)
      expect_equal(length(proj$diffs), 4)

  #TEST 4: make sure it doesn't fail when metadata isn't specified
      csv_files <- c("example-csv-data1.csv", "example-csv-data2.csv")
      csv_path <- file.path(test_path(), "testdata", csv_files)
      prj_path <- NULL
      ff_path <- NULL
      cc_path <- NULL

      proj <- load_project(csv_path, csv_files, prj_path,ff_path, cc_path)

      #make sure things look as expected
      expect_s3_class(proj, "sondeproj")
      expect_s3_class(proj$data, "data.frame")
      expect_type(proj$flags, "list")
      expect_true(is.null(proj$fieldform))
      expect_true(is.null(proj$calcheck))
      expect_type(proj$diffs, "list")
      expect_s3_class(proj$changelog, "data.frame")
})
