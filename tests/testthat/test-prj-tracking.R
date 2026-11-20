test_that("log can be read, written, and cleared", {

  #write to the log
    log <- write_log(NULL, "Cond_S_cm", "physical limits", 5, "test",  "diff1")
    expect_equal(nrow(log), 1)
    expect_equal(log$parameter, "Cond_S_cm")
    expect_equal(log$step,  "physical limits")
    expect_equal(log$n_changed, 5)
    expect_equal(log$diff_name, "diff1")
    expect_equal(log$analyst_note, "test")

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

test_that("changelog review details work", {
  #test adding review info
  proj <- update_log(example_sondeproj, "dd2", "approved", reviewer="J. Smith", note = "change looks fine")
  log <- proj$changelog
  expect_equal(sum(log$review_status == "approved"), 2)
  expect_true(inherits(log$review_date, "POSIXct"))
  row <- log$diff_name == "dd2"
  expect_equal(log$reviewer[row], "J. Smith")
  expect_equal(log$review_note[row], "change looks fine")

  #test adding analyst reply
  proj <- update_log(example_sondeproj, "dd2", "justified", analyst="smith #2", note = "you're wrong")
  log <- proj$changelog
  expect_equal(sum(log$review_status == "justified"), 1)
  expect_true(inherits(log$review_date, "POSIXct"))
  row <- log$diff_name == "dd2"
  expect_equal(log$analyst[row],"smith/smith #2")
  expect_equal(log$analyst_note[row], "you're wrong")

  #test with a single same analyst
  proj <- update_log(example_sondeproj, "dd2", "justified", analyst="smith", note = "you're wrong")
  log <- proj$changelog
  expect_equal(log$analyst[row],"smith")

})
