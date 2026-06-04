
#' Identify duplicated observations with sonde data
#'
#' Uses the interval-rounded datetime (`DateTime_rd`) observations to identify periods of duplicated
#' observations. Based on if they match and the length of the section it identifies a likely cause of
#' the duplication.
#'
#' @param data a `data.frame` with Sonde data.
#'
#' @returns a `data.frame` with the following columns with a row for each duplicated period:
#' - start: starting datetime of the duplicated data
#' - end: ending datetime of the duplicated data
#' - duptype: either "same file" or "multiple files" indicating the type of duplication
#' - ndif: number of points with the duplicated section that are not exactly the same
#' - length: number of points in the duplicated section
#' - perc_dif: percentage of the data that's different
#' - likely_issue: based on information about the duplicates, a guess about the cause of the duplicate
#' - user_note: user input information about the duplicated section
#'
#' @export
#' @md
#' @examples
#' identify_dups(example_data)
identify_dups <- function(data){
  stopifnot(inherits(data, "data.frame"))
  #gets start and end of duplicated sections, filter out parts with all parameters NA, those have been dealt with
  pars <- paste(c("Cond", "fDOM", "ODO", "Sal", "TDS", "Turbidity","TSS","pH","Temp", "Depth"), collapse="|")
  par_names <- grep(pars, names(plot_dat), value = TRUE)
  keep <- rowSums(!is.na(data[par_names])) > 0


  dup_rng <-data[keep, ] %>% dplyr::select(-c("FileName", "DateTime")) %>% group_by(.data$DateTime_rd) %>%
    summarise(count = n(), .groups = "drop") %>% filter(.data$count > 1) %>%
    reframe(start = summarise_date_ranges(.data$DateTime_rd, ignore=0)$start,
            end = summarise_date_ranges(.data$DateTime_rd, ignore=0)$end)

  if(nrow(na.omit(dup_rng)) == 0){
    return(NULL)
  }
  #get more information about the duplicates to determine the likely issue
  problems <- data.frame()
  for(r in 1:nrow(dup_rng)){
    plot_dat <- data %>% filter(.data$DateTime_rd >= dup_rng$start[r] &
                                  .data$DateTime_rd <= dup_rng$end[r]) %>%
      pivot_longer(-c("Index":"Site_Name"), names_to = "parameter", values_to = "measure")

    #check for different values (if more than one file)
    if(length(unique(plot_dat$FileName)) > 1){
      dat <- plot_dat %>% select(-c("Index", "DupNum")) %>% pivot_wider(names_from="FileName", values_from="measure")
      colstart <- which(colnames(dat) == "parameter") +1
      colend <- ncol(dat)
      colnames(dat)[colstart:colend] <- paste0("file", 1:length(colstart:colend))

      if("file3" %in% colnames(dat)){
        dif1 <- dat$file1 - dat$file2
        dif2 <- dat$file1 - dat$file3
        dif3 <- dat$file2 - dat$file3

        avg_dif <- rowSums(cbind(dif1,dif2,dif3), na.rm=TRUE)

        dat$dif <- avg_dif
      }else{
        dat$dif <- dat$file1 - dat$file2
      }
      problem <- dat %>% group_by(.data$DateTime_rd) %>% summarise(dif = sum(.data$dif, na.rm = TRUE), .groups="keep") %>%
        mutate(exact = ifelse(.data$dif != 0, FALSE, TRUE),
               duptype = "multiple files",
               duprow = r)

    }

    #if single file due to malfunction
    if(length(unique(plot_dat$FileName)) == 1){
      problem <- plot_dat %>%
        group_by(.data$DateTime_rd, .data$parameter) %>%
        summarise(dif = max(.data$measure)-min(.data$measure), .groups="keep") %>%
        group_by(.data$DateTime_rd) %>% summarise(dif = sum(.data$dif, na.rm = TRUE), .groups="keep") %>%
        mutate(exact = ifelse(.data$dif != 0, FALSE, TRUE),
               duptype= "same file",
               duprow = r)
    }

    problems <- problems %>% bind_rows(problem)
  }

  #get ranges again to see which ranges are exact and which are different
  dup_check_dif <- problems %>% dplyr::select("DateTime_rd", "duptype","exact") %>% distinct() %>% group_by(.data$duptype) %>%
    reframe(start = summarise_date_ranges(.data$DateTime_rd, .data$exact, ignore=0)$start,
            end = summarise_date_ranges(.data$DateTime_rd,.data$exact, ignore=0)$end,
            ndif = summarise_date_ranges(.data$DateTime_rd,.data$exact, ignore=0)$n_dif)

  #guess at cause (but have to verify)
  dup_check <- dup_check_dif %>% mutate(length = time_diff(.data$start, .data$end, "15 minutes")+1,
                                        perc_dif = round(.data$ndif / .data$length *100, 2),
                                        likely_issue = case_when(
                                          duptype == "multiple files" & .data$perc_dif < 2 ~ "data downloaded multiple times",
                                          duptype == "same file" & .data$perc_dif == 100 & .data$length < 2 ~ "sonde malfunctioned creating erroneous reading",
                                          duptype == "same file" & .data$ndif <= 1 ~ "sonde malfunctioned duplicating data",
                                          duptype == "multiple files" & .data$length > 50 & .data$perc_dif > 10 ~ "!!data likely has a mislabeled site name",
                                          duptype == "multiple files" & .data$length <= 50 & .data$perc_dif >50 ~ "multiple readings during sonde switching",
                                          TRUE ~ "!!unknown")) %>%
    arrange(.data$start) %>% select("start", "end","duptype":"likely_issue") %>% mutate(user_note = NA)

  return(dup_check)
}


#' Apply duplicate flags from a duplicate diff
#'
#' @param flag_tbl the correct flagging table
#' @param col_diff the diff by y_var
#' @param y_var parameter being edited
#' @param op_type the change being made
#' @param flag_code the flag to add
#'
#' @noRd
#'
update_dup_flags <- function(flag_tbl, col_diff, y_var, op_type, flag_code) {
  rows <- col_diff$op_type == op_type

  if(!any(rows)){return(flag_tbl)} #if none of that type, return unchanged

  flag_tbl %>% left_join(col_diff[rows,], by = join_by("DateTime_rd", "DupNum")) %>%
    rename("y_var" = !!y_var) %>%
    mutate(y_var = case_when(
      is.na(.data$op_type) ~ .data$y_var,
      is.na(.data$y_var) ~ flag_code,
      TRUE ~ paste(.data$y_var, flag_code, sep = ";"))) %>%
    select(-"old":-"op_type") %>% rename(!!y_var := "y_var")

}

#' Deals with duplicates in data and documents changes
#'
#' Given a row from `identify_dups` and a `sondeproj`, the user can select which data to keep (or take mean)
#' and the requested data summary will be performed, the appropriate flags will be added, and a changelog entry
#' will be made.
#'
#' @param proj  `sondeproj` object holding sonde data.
#' @param dup_row Row from the outputs of `identify_dups`
#' @param keep_opt Character describing which set of duplicates to keep (identified by `DupNum`) or "use_mean" to take the mean of the values
#' @param flag_notes Optional character with additional notes to write to the changelog
#'
#' @returns a `sondeproj` with the updated data, flags, and changelog
#' @export
#'
apply_dup_edits <- function(proj, dup_row, keep_opt, flag_notes){
   data <- proj$data

  #identify parameters that need to be set to NA
    pars <- paste(c("Cond", "fDOM", "ODO", "Sal", "TDS", "Turbidity","TSS","pH","Temp", "Depth"), collapse="|")
    par_names <- grep(pars, names(plot_dat), value = TRUE)

   #determine which values should be summarised
      row_filter <- data$DateTime_rd >= dup_row$start & data$DateTime_rd <= dup_row$end

    #pull out data we want to summarise
      data_rm <- data %>% filter(!row_filter)
      df_sum <-  data %>% filter(row_filter)

  #if you want to summarise the values, get mean, set first dup to sum value, make other NA
  if(keep_opt  == "use_mean"){
    #summarise data
    df_sum <- df_sum %>%
      group_by(Date, DateTime_rd) %>%
      mutate(across(any_of(par_names), ~ if_else(DupNum == 1,mean(.x, na.rm = TRUE),NA_real_)),
             across("FileName", ~ if_else(DupNum == 1,paste(unique(.), collapse = ";"),NA_character_))) %>%
      ungroup()

  }else{
   #otherwise just keep the group we're interested in
    df_sum <- df_sum %>% mutate(across(any_of(par_names), ~ ifelse(as.numeric(keep_opt) == DupNum, .x, NA_real_)))
  }

  #recombine and re-sort
    data_nodup <- data_rm %>% bind_rows(df_sum) %>% arrange(.data$Index)

  #get diff to log
    diff <- get_diff(data, data_nodup, id=c("DateTime_rd", "DupNum"))

  #apply flags to project
    flag_diff <- diff[names(diff) %in% par_names] #get only diffs we want to flag
    flag_diff <- flag_diff[!sapply(flag_diff, is.null)] #remove any without changes

    #across parameters apply dup flags
    for(y_var in names(flag_diff)){
      col_diff <- flag_diff[[y_var]]

      proj$flags$flag_chg <- update_dup_flags(proj$flags$flag_chg,
                                              col_diff, y_var,"data_changed","DUP01") #DUP01, changed, vals averaged

      proj$flags$flag_rm  <- update_dup_flags(proj$flags$flag_rm,
                                              col_diff, y_var,"data_removed","DUP02") #DUP02, removed

    }

  #update log entry
    #make note
    note <- ifelse(keep_opt == "use_mean", "averaged across duplicate values", paste("kept duplicates from duplicate Set", keep_opt))
    if(flag_notes != ""){note <- paste(note, flag_notes, sep="; ")}

    #give name to diff
    diff <- list(diff)
    names(diff) <- diff_version(proj) #give name to list item

    proj <- write_log(proj, "all", "combine duplicates", n=max(sapply(flag_diff, nrow)),
                      note = note, diff_name = names(diff), return = "sondeproj")

  #add in new df and diff
    proj$data <- data_nodup
    proj$diffs <- c(proj$diffs, dif)

  return(proj)
}

