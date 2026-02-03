test_that("module sever 2 works", {
  #read in project (would be module 1 task)
  data_prj <- read_project(file.path(test_path(), "testdata", "example-sonde-project.RDS"))[[1]]

  testServer(explore_data_server, {

   #make sure data is created
    expect_equal(data(), raw_sonde)
    session$flushReact() #needed here to run reactives

  #make sure plot_data gets written
    expect_true(is.data.frame(plot_data()))

  #make sure log is gotten
    expect_true(inherits(output$log_table, "json"))

    #can't easily check plot because it needs y_var

  #check dates
    #absolute min and max of df
    expect_equal(date_bounds(), list(min=as.Date(min(raw_sonde$Date_MM_DD_YYYY)), max= as.Date(max(raw_sonde$Date_MM_DD_YYYY))))


  #could use some shinytest2 to check flipping week

  },
  #these are passed to the module
  args = list(
    data = reactive({data_prj}) #expected that data is a reactive value so need to pass it that
  ))
})
