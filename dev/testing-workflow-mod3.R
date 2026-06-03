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

#allowing user to deal with duplicates -----
  #getting messy data to test workflow
  proj <- readRDS(file.path(test_path(), "testdata", "example-sondeproj-messy.RDS"))
  data <- proj$data
  data$fDOM_QSU[data$FileName == "dupfile2.csv"] <- data$fDOM_QSU[data$FileName == "dupfile2.csv"] * 1.1


  #identify duplicates
  dups <- identify_dups(data)

  row <- 2
  y_var <- "fDOM_QSU"
  #for each row user can choose to keep v1, v2, or average values
    #show plot of that duplicated region to help determine which to keep
    plot_dat <- data %>% filter(.data$DateTime_rd >= dups$start[row] & .data$DateTime_rd <= dups$end[row]) %>%
      rename("duplicate_id" = "FileName")

  if(dups$duptype[row] == "same file"){
    plot_dat <- plot_dat %>% group_by(.data$DateTime_rd) %>% mutate(duplicate_id = paste("set", row_number())) %>%
      ungroup() }

  ggplot(plot_dat, aes(x=.data$DateTime_rd, y=.data[[y_var]], color=duplicate_id)) + geom_line() + geom_point() +
      labs(color = "duplicate ID")

  #user selects ID
  keep_opts <- c(unique(plot_dat$duplicate_id), "use_mean")

  keep_opts <- "use_mean"
  #work through what would happen
  if(keep_opts  == "use_mean"){
    #pull out data we want to summarise
    data_rm <- data %>% filter(!(.data$DateTime_rd >= dups$start[row] & .data$DateTime_rd <= dups$end[row]))

    #summarise data
    df_sum <-  data %>% filter(.data$DateTime_rd >= dups$start[row] & .data$DateTime_rd <= dups$end[row]) %>%
      group_by(Date, DateTime_rd) %>%
      summarise(across(everything(), ~ {
        if(is.character(.)) {paste(unique(.), collapse = ";")
        }else if (inherits(., c("Date", "POSIXct", "POSIXlt"))) {
          as.POSIXct(mean(as.numeric(.), na.rm = TRUE), origin = "1970-01-01")
        }else if (is.numeric(.)) {
          mean(., na.rm = TRUE)
        }else{NA}}), .groups = "drop_last")

    data_nodup <- data_rm %>% bind_rows(df_sum) %>% arrange(.data$DateTime_rd) %>%
      mutate(Index = 1:n())

    data_nodup
  }else{
    rm_index <- plot_dat$Index[plot_dat$duplicate_id != keep_opts]
    data_nodup <- data[-rm_index]
    data_nodup
  }

  #when they do this, it creates an edit log that this was done --> problem because current version control won't work with dupes
    #issue: how to we track this change, because when new data is added, we'll lose the current dup list if they're removed
      #best I got right now is that the flagging functions will look for duplicates, and if there are duplicates it handles it differently
        #but not sure exactly what that looks like yet....
  #
