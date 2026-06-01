## this module will detect gaps and overlaps in the data (dups), it will try to guess at the cause
  #but display the table and allow the user to edit. This will be saved with the sondeproject.

  #if a sondeproj is loaded that has these, it will copy over user explanations from the old version

#it will also have a button to remove the OOW periods (allow option to set remove_period as FALSE??, edit removal times?)

#inputs in the module
  proj <- example_sondeproj #the project that's getting passed around the app
  y_var <- "fDOM_QSU"

#checking for duplicated data -----
  #triggered when data is loaded (keep static to only when data is uploaded I think, data_ver observer)
  #add fake duplicates because example data doesn't have any
  data <- proj$data
  data <- rbind(data, data[1:14,]) #single file dup
  data <- rbind(data, data[251:264,] %>% mutate(FileName = "dupfile2.csv"))

  dup_check <- identify_dups(data)

  #put in the project (merge user notes to preserve)
   if(!is.null(proj$duplicates)){
     old_dups <- proj$duplicates

     merge_dups <- dup_check %>% select(-"user_note") %>% left_join(old_dups %>% select("start", "end", "user_note"), join_by("start", "end"))
   }else{
     merge_dups <- dup_check
   }

    proj$duplicates <- merge_dups

  #view and let user edit

#checking for missing data ------

  #set a larger gap for testing
  data <- data[-(500:580),]

  missing <- identify_gaps(data)

  #put in the project (merge user notes to preserve)
  if(!is.null(proj$data_gaps)){
    old_gap <- proj$data_gaps

    merge_gap <- missing %>% select(-"user_note") %>% left_join(old_gap %>% select("start", "end", "user_note"), join_by("start", "end"))
  }else{
    merge_gap <- missing
  }

  proj$data_gaps <- merge_gap

  #view and let user edit

#removing OOW periods -----
  #UI input
  rm_OOW <- TRUE

#if button to rm oow, remove periods and add flags
  #req(sondeproj$fieldform)

  #get OOW periods
  oow <- get_oow(proj$fieldform)

  #remove those periods from data
  setna <- data$DateTime_rd >= oow$start & data$DateTime_rd <= oow$end

  data_filter <- data %>% mutate(filter = setna) %>%
    mutate(across(-("Index":"Battery_V"), ~ if_else(filter, NA, .x))) %>%
    select(-"filter")

  #flag these changes were made
  edit <- list(
    data = data_filter,
    rows = setna,
    y_var = "all",
    step = "removing oow",
    note = paste0("OOW periods removed based on information from the field form."),
    flag = "RM04",
    changetype = "flag_rm"
  )

  #flags and updates data
  #apply_edit_server("apply_limits", sondeproj, edit)
