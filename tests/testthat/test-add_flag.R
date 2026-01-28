test_that("flags are added", {
  #flag columns are added if missing
    df <- add_flags(raw_sonde)
    expect_equal(sum(grepl("_flag", colnames(df))), 14)

  #try if some are added
    df <- raw_sonde
    df$Cond_uS_cm_flag <- NA
    df <- add_flags(df)
    expect_equal(sum(grepl("_flag", colnames(df))), 14)


  #add new flags
    df <- add_flags(raw_sonde, "fDOM_QSU", "test_flag", c(1,2,3))
    expect_equal(df$fDOM_QSU_flag[[1]], c(test_flag = TRUE))
    expect_equal(df$fDOM_QSU_flag[[4]], c(test_flag = FALSE))

    #add to existing flags
      df <- add_flags(df, "fDOM_QSU", "test_flag2", c(1,2))
      expect_equal(df$fDOM_QSU_flag[[1]], c(test_flag = TRUE, test_flag2 = TRUE))
      expect_equal(df$fDOM_QSU_flag[[3]], c(test_flag = TRUE, test_flag2 = FALSE))
      expect_equal(df$fDOM_QSU_flag[[4]], c(test_flag = FALSE, test_flag2 = FALSE))

  #ensure flags rewrite
    df <- add_flags(df, "fDOM_QSU", "test_flag", c(4))
    expect_equal(df$fDOM_QSU_flag[[1]], c(test_flag = TRUE,test_flag2 = TRUE))
    expect_equal(df$fDOM_QSU_flag[[4]], c(test_flag = TRUE, test_flag2 = FALSE))
})

test_that("saving flags works",{
  prj_path <- file.path(withr::local_tempdir(), "test_prj.RDS")

  clear_data()
  clear_log()

  write_data(raw_sonde, "raw")

  df <- flag_data(raw_sonde, "fDOM_QSU", "test_flag", 1:4, prj_path)

  #ensure file is saved
  expect_true(file.exists(prj_path))

  #ensure log is written
  expect_equal(nrow(get_log()), 1)
  expect_equal(get_log()$step, "test_flag")

  #ensure data ver is saved
  expect_equal(names(get_data()), c("raw", "824f400c52499aa98d40f5efe0623169"))

  #ensure no new version is saved if same changes are made
    df <- flag_data(df, "fDOM_QSU", "test_flag", 1:4, prj_path)

    #ensure log is written
    expect_equal(nrow(get_log()), 1)
    expect_equal(get_log()$step, "test_flag")

    #ensure data ver is saved
    expect_equal(names(get_data()), c("raw", "824f400c52499aa98d40f5efe0623169"))


})
