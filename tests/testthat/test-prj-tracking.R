test_that("log can be read, written, and cleared", {

  #write to the log
    log <- write_log(NULL, "Cond_S_cm", "physical limits", 5, "test",  "diff1")
    expect_equal(nrow(log), 1)
    expect_equal(log$parameter, "Cond_S_cm")
    expect_equal(log$step,  "physical limits")
    expect_equal(log$n_changed, 5)
    expect_equal(log$diff_name, "diff1")
    expect_equal(log$note, "test")

  #store in project
    proj <- example_sondeproj
    proj$changelog <- log

  #write another line
    #return log only
    log <- write_log(proj, "Cond_S_cm", "physical limits", 2, "test", "diff2")
    expect_equal(nrow(log), 2)

    #return within project
    proj <- write_log(proj, "Cond_S_cm", "physical limits", 2, "test", "diff2", return="sondeproj")
    expect_equal(nrow(proj$changelog), 2)

})
