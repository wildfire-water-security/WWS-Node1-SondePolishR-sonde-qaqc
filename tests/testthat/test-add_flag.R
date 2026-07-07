test_that("flags are added", {
  #check that if we add flags it turns a df with the same rows/columns but NA (as character)
    proj <- example_sondeproj
    proj$flags <- NULL
    newproj <- add_flags(proj, example_data)
    data <- newproj$flags$flag_rm
    expect_equal(sum(grepl("_flag", colnames(data))), 0)
    expect_equal(colnames(data)[1:4], c("Index", "DupNum", "DateTime", "DateTime_rd"))
    expect_true(all(sapply(data, class)[-c(1:4)] == "character"))
    expect_true(all(sapply(data, function(x){sum(is.na(x))})[-c(1:4)] == nrow(data)))

})
