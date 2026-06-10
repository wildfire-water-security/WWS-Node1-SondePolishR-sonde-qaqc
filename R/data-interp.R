#' Fill data gaps with interpolation
#'
#' Provides a number of methods to fill NA values for a given parameter within the dataset with
#' options to only fill a certain gap length otherwise leave the missing values alone.
#'
#' @param proj A `sondeproj` object holding sonde data.
#' @param y_var Variable being interpolated.
#' @param max_length The maximum length in hours to fill via interpolation.
#' @param method The method to use for interpolation, options include: `linear`, `spline`, `random_forest`, and `ts_interp` see details for further information about the methods.
#' @param freq The period interval for the time series in days, only used if `method` is `ts_interp`.
#' @param progress A function passed from a shiny module to create a progress bar.
#'
#' @returns A data.frame if there were completely missing observations, rows may have been added with the missing datetimes. Additionally, an extra column is added (`x_fill`)
#' which is a logical flag identifying which values were interpolated.
#' @export
#' @md
#'
#' @details
#' Currently the follow interpolation options are supported:
#' - `linear`: Uses the zoo::na.approx() function to linearly fill gaps.
#' - `spline`: Uses the zoo::na.spline() function to fill gaps via spline interpolation (makes smoother curves).
#' - `random_forest`: Depending on how much data is in the project, this option can be a little slow. Uses missForest::missForest() to
#' fill gaps. uses responses accross all the available parameters to try and fill in the missing parameter.
#' - `ts_interp`: Uses forecast::na.interp() which treats the data as a time series and make predictions based on the natural seasonal
#' patterns within the data. The seasonality is determined by `freq`. For instance, if set to 1, it will account for daily fluctuations.
#' If set to 365 it wil look at annual fluctuations.
#'
#' @examples
#' path <- file.path(fs::path_package("extdata", package = "SondePolishR"),
#' "example-sondeproj-messy.RDS")
#' proj <- readRDS(path)
#' data_filled <- data_interp(proj, "fDOM_QSU", 4, method="linear")
#'
data_interp <- function(proj, y_var, method, max_length, freq=1, progress=NULL){
  stopifnot(inherits(proj, "sondeproj"), is.character(y_var), is.numeric(max_length),
            is.null(freq) || is.numeric(freq))

  update_progress <- function(value, detail = NULL) {
    if (!is.null(progress)){progress(value, detail)}}

  update_progress(0.05, "Preparing data")

  #get data from project
    data <- proj$data

  #determine interval of data for gap length
    interval <- get_interval(data)

  #stuff to fill in missing correctly
    tz <- tz(data$DateTime_rd)
    name <- unique(data$Site_Name)
    par_names <- get_parms(data)

  #get the dataset to interpolate (still may have dupes)
    #fill in info from totally missing lines
    update_progress(0.10, "Creating complete time series")
    data_fill <- data %>%
      complete(DateTime_rd = seq(min(.data$DateTime_rd), max(.data$DateTime_rd),
                                 by = paste(interval, "min"))) %>%
      arrange(.data$DateTime_rd, .data$DupNum) %>% #want to arrange in time order for filling
      mutate(Index = 1:n(),
             DupNum = ifelse(is.na(.data$DupNum), 1, .data$DupNum),
             FileName = ifelse(is.na(.data$FileName), "interpolated", .data$FileName),
             Date = if_else(is.na(.data$Date), as.Date(.data$DateTime_rd, tz = tz), .data$Date),
             Time_HH_mm_ss = if_else(is.na(.data$Time_HH_mm_ss), strftime(.data$DateTime_rd, "%H:%M:%S"), .data$Time_HH_mm_ss),
             DateTime = if_else(is.na(.data$DateTime), .data$DateTime_rd, .data$DateTime),
             Site_Name = name)

  #get df with a single stamp per row (conflicting duplicates are set to NA)
    #determine which sets of dups are conflicting (for removing from interpolated data)
    update_progress(0.25, "Identifying duplicate conflicts")

    conflict <- data %>%
      pivot_longer(any_of(par_names), names_to = "param", values_to = "value") %>%
      group_by(.data$DateTime_rd, .data$param) %>%
      summarise(count = n(),sd = sd(.data$value),.groups = "drop_last") %>%
      filter(.data$count > 1 & .data$sd != 0)

    #track which values we want to fill in (ignoring gap size)
    update_progress(0.40, "Preparing interpolation dataset")

    data_fill <- data_fill %>% group_by(.data$DateTime_rd) %>%
      mutate(n_dup = n(), n_non_na = sum(!is.na(.data[[y_var]]))) %>%
      mutate(fill_flag = ifelse(is.na(.data[[y_var]]) & (.data$n_dup == 1 | .data$n_non_na == 0 & .data$DupNum == 1), TRUE,FALSE))

    #set those parameters/datetimes to NA
    conflict_list <- split(conflict$DateTime_rd, conflict$param)

    data_interp <- data_fill %>%
      mutate(across(all_of(names(conflict_list)),
                    ~ replace(.x, .data$DateTime_rd %in% conflict_list[[cur_column()]],NA)))

    #fill and summarize to a single obs per datetime
    data_interp <- data_interp %>% arrange(.data$DateTime_rd, .data$DupNum) %>%
      group_by(.data$DateTime_rd) %>%
      tidyr::fill(any_of(par_names), .direction = "downup") %>%
      slice(1) %>% ungroup()

#pass non-duplicated data to interpolation functions
  update_progress(0.60, "Running interpolation")

  if(method == "linear"){
    x_fill <- zoo::na.approx(data_interp[[y_var]],
                             maxgap = max_length*(60/interval),
                             na.rm=FALSE)}

  if(method == "spline"){
    x_fill <- zoo::na.spline(data_interp[[y_var]],
                             maxgap = max_length*(60/interval),
                             na.rm=FALSE)}

  if(method == "random_forest"){
    update_progress(0.70, "Fitting random forest")

    filled <- data_interp %>%
      select(-any_of(c("DateTime_rd", "FileName", "Date", "Time_HH_mm_ss",
                       "DateTime", "Site_Name", "DupNum", "fill_flag"))) %>%
      as.data.frame() %>% missForest::missForest()

    #only fill to max gap
    x_fill <- .fill_short_gaps(data_interp[[y_var]], filled$ximp[[y_var]],
                               maxgap = max_length*(60/interval))}

  if(method == "ts_interp"){
    #make time series
    var_ts <- ts(data_interp[[y_var]], frequency = freq*24*(60/interval))
    #only fill to max gap
    x_fill <- .fill_short_gaps(data_interp[[y_var]],
                               as.numeric(forecast::na.interp(var_ts)),
                               maxgap = max_length*(60/interval))}

  #save as df that we can join with
  fill_df <- data.frame(DateTime_rd = data_interp$DateTime_rd, x_fill = x_fill)

  #map filled values back to filled df
  update_progress(0.95, "Mapping values back to original data")

  data_fill <- data_fill %>% left_join(fill_df, by="DateTime_rd") %>%
    mutate(!!y_var := ifelse(.data$fill_flag, .data$x_fill, .data[[y_var]])) %>%
    select(-(c("n_dup", "n_non_na", "x_fill"))) %>% ungroup() %>% arrange(.data$DateTime_rd, .data$DupNum) %>%
    mutate(Index = 1:n())
  update_progress(1.00, "Done")
  return(data_fill)

}

#' Fill gaps only up to a max length
#'
#' Using directly from zoo package (not exported by zoo).
#'
#' @param x vector with na values
#' @param fill vector filled
#' @param maxgap maximum length to fill
#'
#' @returns x filled with fill if gap isn't too long
#' @noRd
#' @source  Zeileis A, Grothendieck G (2005). “zoo: S3 Infrastructure for Regular and Irregular Time Series.” _Journal of Statistical Software_,
#' *14*(6), 1-27. doi:10.18637/jss.v014.i06 <https://doi.org/10.18637/jss.v014.i06>.
#'
.fill_short_gaps <- function(x, fill, maxgap) {
  if (maxgap <= 0)
    return(x)
  if (maxgap >= length(x))
    return(fill)
  naruns <- rle(is.na(x))
  naruns$values[naruns$lengths > maxgap] <- FALSE
  naok <- inverse.rle(naruns)
  x[naok] <- fill[naok]
  return(x)
}


