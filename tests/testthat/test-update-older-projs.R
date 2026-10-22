test_that("oow periods get split up", {
  #set up creating the OOW entry
  flag_oow <- function(proj){
    #create a project with a merged OOW period
    data <- proj$data

    #get OOW periods
    oow <- get_oow(proj$fieldform, tz=proj$meta$tz,interval=get_interval(data))

    #remove those periods from data
    rm_index <- data %>%
      rowwise() %>%
      filter(any(.data$DateTime_rd >= oow$start & .data$DateTime_rd <= oow$end)) %>% pull(.data$Index) %>% unique()
    setna <- data$Index %in% rm_index

    data_filter <- data %>% mutate(filter = setna) %>%
      mutate(across(-("DateTime_rd":"Battery_V"), ~ if_else(filter, NA, .x))) %>%
      select(-"filter")

    #flag these changes were made
    edit <- list(
      data = data_filter,
      rows = rm_index,
      y_var = "all",
      step = "removing oow",
      note = paste0("OOW periods removed based on information from the field form."),
      flag = "RM04",
      changetype = "flag_rm"
    )

    #log edits
    proj <- apply_edit(proj, edit, "Smith")
    return(proj)
  }

  proj <- flag_oow(example_sondeproj)

  #test the update
    proj_update <- split_oow(proj)
    changelog <- proj_update$changelog
    diffs <- proj_update$diffs
    expect_equal(nrow(changelog), 12)
    expect_equal(length(diffs), 11)
    expect_true(!any(changelog$step == "removing oow" & changelog$parameter == "all"))

 #test when it's not at the end
  #remove OOW
   proj <- flag_oow(example_sondeproj)

  #add more changes
  set.seed(9)
  for(x in 1:5){
    edit <- list(data = proj$data,rows = sample(1:nrow(proj$data), 5),
                 y_var = sample(get_parms(proj$data),1), step = "testing",
                 note = paste0("note here"),
                 flag = "TEST01",
                 changetype = "flag_rm")

    #log edits
    proj <- apply_edit(proj, edit, "Smith")
  }

  #test the update
    proj_update <- split_oow(proj)
    changelog <- proj_update$changelog
    diffs <- proj_update$diffs
    expect_equal(nrow(changelog), 17)
    expect_equal(length(diffs), 16)
    expect_true(!any(changelog$step == "removing oow" & changelog$parameter == "all"))

 #test when more than one OOW
    #remove OOW
    proj <- flag_oow(example_sondeproj)

    #add more changes
    set.seed(9)
    for(x in 1:5){
      edit <- list(data = proj$data,rows = sample(1:nrow(proj$data), 5),
                   y_var = sample(get_parms(proj$data),1), step = "testing",
                   note = paste0("note here"),
                   flag = "TEST01",
                   changetype = "flag_rm")

      #log edits
      proj <- apply_edit(proj, edit, "Smith")
    }

    proj <- flag_oow(proj)

    #test the update
    proj_update <- split_oow(proj)
    changelog <- proj_update$changelog
    diffs <- proj_update$diffs
    expect_equal(nrow(changelog), 23)
    expect_equal(length(diffs), 22)
    expect_true(!any(changelog$step == "removing oow" & changelog$parameter == "all"))

})
