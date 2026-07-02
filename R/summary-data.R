#' Summarize data to a different time period
#'
#' Used to take data and aggregate it to a different interval.
#'
#' @param data a `data.frame` to summarize
#' @param frequency a `Period` object created using `lubridate::period()` specifying the time frame to summarize over
#' @param sum_method the summary method to use to summarize the data
#'
#' @returns a `data.frame.
#' @export
#' @md
#' @examples
#' summarize_data(example_data, lubridate::period(1, "month"), "mean")
#'
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
    group_by(.data$DateTime_rd) %>% summarise(across(all_of(parms), ~ifelse(all(is.na(.x)), NA, sum_fun(.x, na.rm=TRUE))),
                                              across(all_of(flags), ~comb_unique_flags(.x)))
  sum_data <- sum_data[, c("DateTime_rd", sort(setdiff(names(sum_data), "DateTime_rd")))]

  return(sum_data)

}

#' Pull flags from a project and join with data.frame
#'
#' @param sondeproj a `sondeproj` object to pull flags and connect with data
#' @returns a `data.frame`
#' @noRd
#'
combine_flags <- function(sondeproj){
  stopifnot(inherits(sondeproj, "sondeproj"))

  data <- sondeproj$data

  #get flags and combine into a single column per par
  parms <- get_parms(data)
  flags <- sondeproj$flags %>% bind_rows() %>% group_by(.data$Index, .data$DupNum, .data$DateTime, .data$DateTime_rd) %>%
    summarise(across(all_of(parms),function(x) paste(x[!is.na(x)], collapse = ";")),
              .groups = "drop") %>% mutate(across(all_of(parms), ~ ifelse(.x == "", NA, .x))) %>%
    rename_with(~ paste0(.x, "_flag"), .cols=all_of(parms))

  #link to data
  export_data <- data %>% left_join(flags, by = join_by("Index", "DupNum", "DateTime", "DateTime_rd"))
  export_data <- export_data %>%
    select("Index":"Battery_V",sort(setdiff(names(export_data), names(export_data %>% select("Index":"Battery_V")))))

  return(export_data)
}

describe_data <- function(data){

}
