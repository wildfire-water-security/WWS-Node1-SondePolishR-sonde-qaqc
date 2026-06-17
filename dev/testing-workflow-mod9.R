## working on the non-shiny workflow for the last !!! module which is used to export the data and save the project

  #things it needs to do:
    #save the project
    #export data to .csv
        #format flags in a human readable way
        #export summarized data (daily max, min, mean, median)
        #also a date range to export
    #export metadata

## things passed to module
  proj <- example_sondeproj

# UI things
  frequency <- lubridate::period(1, "day")
  sum_method <- "max"

#figure out the output structure ------
  data <- proj$data

  #when not summarized (do this regardless and summarize on this df so we can export it)
    #get flags and combine into a single column per par
      parms <- get_parms(data)
      flags <- proj$flags %>% bind_rows() %>% group_by(Index, DupNum, DateTime, DateTime_rd) %>%
        summarise(across(all_of(parms),function(x) paste(x[!is.na(x)], collapse = ";")),
          .groups = "drop") %>% mutate(across(all_of(parms), ~ ifelse(.x == "", NA, .x))) %>%
        rename_with(~ paste0(.x, "_flag"), .cols=all_of(parms))

    #link to data
      export_data <- data %>% left_join(flags,by = join_by(Index, DupNum, DateTime, DateTime_rd)) %>%
         select(Index:Battery_V, sort(names(.)))

    #summarize
      export_data <- summarize_data(export_data, frequency, sum_method)

      data <- export_data
## Functions to summarize the data ------
  summarize_data <- function(data, frequency, sum_method){
    stopifnot(is.data.frame(data), inherits(frequency, "Period"),
              sum_method %in% c("mean", "median", "max", "min"))

    sum_fun <- switch(sum_method,
                      "mean" = mean,
                      "median" = median,
                      "max" = max,
                      "min" = min)

    #separate flags and
    parms <- get_parms(data)
    flags <- grep("_flag", parms, value=TRUE)
    parms <- grep("_flag", parms, value=TRUE, invert = TRUE)

    comb_unique_flags <- function(x){
      if(all(is.na(x))){return(NA_character_)}

      vals <- unique(unlist(strsplit(na.omit(x), ";", fixed = TRUE)))
      vals <- vals[vals != ""]
      if(length(vals) == 0){return(NA_character_)}else{paste(vals, collapse = ";")}
    }

    sum_data <- data %>% mutate(DateTime_rd = round_date(.data$DateTime_rd, frequency)) %>%
      group_by(.data$DateTime_rd) %>% summarise(across(all_of(parms), ~sum_fun(.x, na.rm=TRUE)),
                                                across(all_of(flags), ~comb_unique_flags(.x))) %>%
      select(DateTime_rd, sort(names(.)))

    return(sum_data)

  }

