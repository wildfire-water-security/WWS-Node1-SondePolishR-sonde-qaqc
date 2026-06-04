#' Identify missing observations with sonde data
#'
#' Uses the interval-rounded datetime (`DateTime_rd`) observations to identify periods of missing
#' observations.
#'
#' @param data a `data.frame` with sonde data.
#' @param ignore the length in minutes to ignore gaps
#'
#' @returns a `data.frame` with the following columns with a row for each missing period:
#' - start: starting datetime of the missing data
#' - end: ending datetime of the missing data
#' - gap_length: number of points in the missing section
#' - user_note: user input information about the duplicated section
#' @export
#' @md
#'
#' @examples
#' identify_gaps(example_data, ignore=0)
identify_gaps <- function(data, ignore = 60*5){
  stopifnot(inherits(data, "data.frame"))

  get_mode <- function(x) {
    ux <- unique(x)
    ux[which.max(tabulate(match(x, ux)))]
  }
  interval <- get_mode(as.numeric(difftime(data$DateTime, lag(data$DateTime), units="mins")))

  missing <- data %>% complete(DateTime_rd = seq(min(.data$DateTime_rd), max(.data$DateTime_rd), by = paste(interval, "min"))) %>%
    filter(is.na(.data$Index)) %>% reframe(start = summarise_date_ranges(.data$DateTime_rd, ignore=ignore, interval =interval)$start,
                                           end = summarise_date_ranges(.data$DateTime_rd, ignore=ignore, interval =interval)$end,
                                           gap_length =summarise_date_ranges(.data$DateTime_rd, ignore=ignore, interval =interval)$gap_min/interval) %>%
    mutate(user_note =NA)

  if(nrow(missing) == 0){return(NULL)}
  return(missing)
}


#' Updates the duplicate and gaps tables in a project based on the data
#'
#' @param proj a `sondeproj` object
#'
#' @returns a `sondeproj` object
#' @noRd
#'
refresh_checks <- function(proj) {

  #get duplicates
  dup_check <- identify_dups(proj$data)

  #put in the project (merge user notes to preserve)
  if(!is.null(proj$duplicates) && !is.null(dup_check)){
    old_dups <- proj$duplicates

    merge_dups <- dup_check %>% select(-"user_note") %>% left_join(old_dups %>% select("start", "end", "user_note"), join_by("start", "end"))
  }else{
    merge_dups <- dup_check
  }

  proj$duplicates <- merge_dups

  #get gaps
  missing <- identify_gaps(proj$data)

  #put in the project (merge user notes to preserve)
  if(!is.null(proj$data_gaps)){
    old_gap <- proj$data_gaps

    merge_gap <- missing %>% select(-"user_note") %>% left_join(old_gap %>% select("start", "end", "user_note"), join_by("start", "end"))
  }else{
    merge_gap <- missing
  }

  proj$data_gaps <- merge_gap

  proj
}
