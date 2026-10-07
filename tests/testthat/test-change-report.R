test_that("records are generated", {
  #test report header
  header <- create_report_header("smith", "FAL")

  #make sure all are markdown
  expect_true(all(grepl("#'", header)))
  expect_equal(sum(grepl("---", header)), 2)

  #test each change is generated
    dir <- withr::local_tempdir()
    path <- file.path(dir, "sondepolishR-report.R")
    proj <- example_sondeproj
    log <- proj$changelog[-1,] #remove first row that's just loading data
    old <- get_raw_data(proj) #get data before change

    text <- c() #spot to put the text generated
    for(x in 1:nrow(log)){
      info <- log[x,] #get info for report
      dd <- proj$diffs[[log$diff_name[x]]]
      new <- apply_diff(old, dd,
                        id = c("DateTime_rd", "DupNum"))
      text <- c(text, create_report_record(dir, old, new, info, dd))
      old <- new #replace old data with the revised data
    }

    expect_equal(length(text), 11 * nrow(log)) #should have 11 rows for each
    expect_true(all(grepl("#'", text))) #should all be markdown format
    expect_equal(sum(grepl("!\\[\\].+png", text)), nrow(log)) #make sure we have the right number of images shown

    #check plots are generated
    expect_equal(length(list.files(dir, pattern="change-dd[0-9]{1,}.png")), nrow(log))

})

test_that("report is generate and looks correct", {
  report_path <- tempfile(fileext = ".pdf")
  suppressMessages(generate_report(example_sondeproj, "smith", report_path))

  expect_true(file.exists(report_path))
  proj <- example_sondeproj
  log <- proj$changelog[-1,]

  #if pdftools installed use to do more testing
  if(requireNamespace("pdftools", quietly = TRUE)){
    meta <- pdftools::pdf_info(report_path)
    expect_equal(meta$page, nrow(log) + 1) #ensure we have right pages
  }

  #remove testing report
  unlink(report_path)
})
