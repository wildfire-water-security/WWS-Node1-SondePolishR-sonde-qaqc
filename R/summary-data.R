#' Summarize data to a different time period
#'
#' Used to take data and aggregate it to a different interval.
#'
#' @param data a `data.frame` to summarize
#' @param frequency a `Period` object created using `lubridate::period()` specifying the time frame to summarize over
#' @param sum_method the summary method to use to summarize the data, can choose more than one
#'
#' @returns a `data.frame where the summary method has been appended to each parameter column name.
#' @export
#' @md
#' @examples
#' summarize_data(example_sondeproj$data, lubridate::period(1, "month"), "mean")
#'
#' #using multiple methods
#' summarize_data(example_sondeproj$data, lubridate::period(1, "month"), c("mean", "median", "max"))

summarize_data <- function(data, frequency, sum_method = c("mean", "median", "max", "min")){
  stopifnot(is.data.frame(data), inherits(frequency, "Period"),
            sum_method %in% c("mean", "median", "max", "min"))

  parms <- get_parms(data) #get parameter names

 #if more than one method, run over all methods, then combine
  if(length(sum_method) > 1){
    full_df <- lapply(sum_method, function(x){
      df <- summarize_data(data, frequency, x)
    })

    #combine into a single df
    order <- c("DateTime_rd", paste0(rep(parms, each=length(sum_method)+1), "_", c(sum_method, "flag")))
    id_cols <- colnames(full_df[[1]])[!(colnames(full_df[[1]]) %in% get_parms(full_df[[1]]))]
    merged_df <- Reduce(function(x, y) left_join(x, y, by = id_cols), full_df) %>%
      select(all_of(order))

    return(merged_df)

  }
  sum_fun <- switch(sum_method,
                    "mean" = mean,
                    "median" = median,
                    "max" = max,
                    "min" = min)

  #separate flags and
  parms <- get_parms(data, flags=FALSE)
  flags <- grep("_flag$", get_parms(data, flags=TRUE), value=TRUE)

  #get all flags for summarized points and return unique ones
  comb_unique_flags <- function(x){
    vals <- na.omit(unique(unlist(x)))

    if(length(vals) == 0){return(NA_character_)}else{paste(vals, collapse = ";")}
  }

  sum_data <- data %>% mutate(DateTime_rd = floor_date(.data$DateTime_rd, frequency)) %>%
    group_by(.data$DateTime_rd) %>% summarise(across(all_of(parms), ~ifelse(all(is.na(.x)), NA, sum_fun(.x, na.rm=TRUE))),
                                              across(all_of(flags), ~comb_unique_flags(.x)))
  sum_data <- sum_data[, c("DateTime_rd", sort(setdiff(names(sum_data), "DateTime_rd")))]

  sum_data <- sum_data %>% rename_at(parms, ~paste0(.x, "_", sum_method))

  return(sum_data)

}

#' Gets summary statistics for Sonde data
#'
#' Used to quickly generate summary statistics about each parameter for a given dataset.
#'
#' @param data a `data.frame` with sonde dataset.
#' @param precip Optional precipitation dataset.
#' @md
#' @returns a `data.frame`
#' @export
#'
#' @examples
#' describe_data(example_data)
describe_data <- function(data, precip=NULL){
  parms <- get_parms(data)

  #get summaries per each variable
  sum_parm <- function(var){
    data.frame("Mean" = mean(var, na.rm=TRUE),
               "Median" = median(var, na.rm=TRUE),
               "Maximum" = max(var, na.rm=TRUE),
               "Minimum" = min(var, na.rm=TRUE),
               "Std_Deviation" = sd(var, na.rm=TRUE),
               "Quantile_1st" = quantile(var, 0.25, na.rm = TRUE, names=FALSE),
               "Quantile_3rd" = quantile(var, 0.75, na.rm =TRUE, names=FALSE),
               "Number_NAs" = sum(is.na(var)))
  }

  #get nice row names
  summary_names <- c(names(nice_yvar(data)), "Precipitation (mm hr\U207B\U00B9)")

  sum_df <- lapply(data[parms], sum_parm) %>% bind_rows()
  if(!is.null(precip)){
    precip_sum <- sum_parm(precip$Precip_mm_hr)
  }else{
    precip_sum <- sum_parm(1)
    precip_sum[1,] <- NA
  }

  sum_df <- sum_df %>% bind_rows(precip_sum)

  sum_df <- cbind(Parameter = summary_names, sum_df, row.names = NULL) %>%
    mutate(across("Mean":"Quantile_3rd", ~round(.x, 3)))

  return(sum_df)
}

