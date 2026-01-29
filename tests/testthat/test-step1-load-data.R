library(shiny)

test_that("module sever 1 works loading a csv", {

  testServer(load_data_server, {
    #test loading a csv
    session$setInputs(file = data.frame(name = "sonde-example.csv", size=1,type="csv",
                                        datapath=file.path(testthat::test_path(),
                                                           "testdata/sonde-example.csv")),
                      tz = "Etc/GMT+8")

    #expect that df should be loaded
      expect_s3_class(df(), "data.frame")
      expect_equal(type(), "csv")
      expect_equal(prj_path(), NULL) #no save path set


    #set the save location
      #shinyFiles you pass root something that is in roots and it returns the path
      session$setInputs(save_file = list(root = 'C Drive',
                                         path = character(0)))

      #now should be what we set it to
      expect_equal(prj_path(), "C://sonde-example.RDS")


    #expect output should be a list saved as a reactive
      output <- session$returned()
      expect_true(inherits(output,"data.frame"))

  })
})


test_that("module sever 1 works loading an existing project", {

  testServer(load_data_server, {
    #test loading a csv
    session$setInputs(file = data.frame(name = "example-sonde-project.RDS", size=1,type="RDS",
                                        datapath=file.path(testthat::test_path(),
                                                           "testdata/example-sonde-project.RDS")),
                      tz = "Etc/GMT+8")


    #expect that df should be loaded
    expect_s3_class(df(), "data.frame")
    expect_equal(type(), "RDS")
    expect_equal(prj_path(), "inst/extdata/example-sonde-project.RDS") #should pull path from the project


    #expect output should be a list saved as a reactive
    output <- session$returned()
    expect_true(inherits(output,"data.frame"))

  })
})

