test_that("flags are added", {
  #flag columns are added if missing
    data <- add_flags(raw_sonde)
    expect_equal(sum(grepl("_flag", colnames(data))), 14)

  #try if some are added
    data <- raw_sonde
    data$Cond_uS_cm_flag <- NA
    data <- add_flags(data)
    expect_equal(sum(grepl("_flag", colnames(data))), 14)


  #add new flags
    data <- add_flags(raw_sonde, "fDOM_QSU", "test_flag", c(1,2,3))
    expect_equal(data$fDOM_QSU_flag[[1]], c(test_flag = TRUE))
    expect_equal(data$fDOM_QSU_flag[[4]], c(test_flag = FALSE))

    #add to existing flags
      data <- add_flags(data, "fDOM_QSU", "test_flag2", c(1,2))
      expect_equal(data$fDOM_QSU_flag[[1]], c(test_flag = TRUE, test_flag2 = TRUE))
      expect_equal(data$fDOM_QSU_flag[[3]], c(test_flag = TRUE, test_flag2 = FALSE))
      expect_equal(data$fDOM_QSU_flag[[4]], c(test_flag = FALSE, test_flag2 = FALSE))

  #ensure flags rewrite
    data <- add_flags(data, "fDOM_QSU", "test_flag", c(4))
    expect_equal(data$fDOM_QSU_flag[[1]], c(test_flag = TRUE,test_flag2 = TRUE))
    expect_equal(data$fDOM_QSU_flag[[4]], c(test_flag = TRUE, test_flag2 = FALSE))
})

test_that("saving flags works",{
  prj_path <- file.path(withr::local_tempdir(), "test_prj.RDS")

  SondePolishR::clear_data()
  SondePolishR::clear_log()
  SondePolishR::clear_prjpath()

  write_data(raw_sonde, "raw")
  write_log("All", "Initial Load", n = 0, version = "raw")
  set_prjpath(prj_path)

  data <- flag_data(raw_sonde, "fDOM_QSU", "test_flag", 1:4)

  #ensure file is saved
  expect_true(file.exists(prj_path))

  #ensure log is written
  expect_equal(nrow(get_log()), 2)
  expect_equal(get_log()$step, c("Initial Load", "test_flag"))

  #ensure data ver is saved
  expect_equal(names(get_data()), c("raw", "0ba6c8cf7d0fc4bc4d13c06aedd9c1bc"))

  #ensure no new version is saved if same changes are made
    data <- flag_data(data, "fDOM_QSU", "test_flag", 1:4, prj_path)

    #ensure log is written
    expect_equal(nrow(get_log()), 2)
    expect_equal(get_log()$step, c("Initial Load", "test_flag"))

    #ensure data ver is saved
    expect_equal(names(get_data()), c("raw", "0ba6c8cf7d0fc4bc4d13c06aedd9c1bc"))


})
