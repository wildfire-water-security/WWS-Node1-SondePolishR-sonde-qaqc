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
