#' Prepare interpolation dataset
#'
#' Completes the date-time sequence to add missing rows for missing datetimes. Also creates an
#' interpolation dataset where any duplicates have been condensed to a single value. In the case of
#' two different values, they will be set to NA for the purposes of interpolation to not
#' interpolate with questionable data.
#'
#' @param proj A `sondeproj` object holding sonde data.
#'
#' @returns a list of length two:
#' - fill: `data.frame` based on `proj$data` with missing `datetime` values added.
#' - interp: `data.frame` based on `proj$data` with duplicates condensed to a single value.
#' @export
#' @md
#' @examples
#' interp_dfs <- prep_interp(example_sondeproj)
prep_interp <- function(proj){
  stopifnot(inherits(proj, "sondeproj"))

  #get data from project
  data <- proj$data

  #determine interval of data for gap length
  interval <- get_interval(data)

  #stuff to fill in missing correctly
  tz <- tz(data$DateTime_rd)
  name <- unique(data$Site_Name)
  par_names <- get_parms(data)

  #get the dataset to interpolate (still may have dupes)
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
  conflict <- data %>%
    pivot_longer(any_of(par_names), names_to = "param", values_to = "value") %>%
    group_by(.data$DateTime_rd, .data$param) %>%
    summarise(count = n(),sd = sd(.data$value),.groups = "drop_last") %>%
    filter(.data$count > 1 & .data$sd != 0)
  conflict_list <- split(conflict$DateTime_rd, conflict$param)

  #set those parameters/datetimes to NA
  data_interp <- data_fill %>%
    mutate(across(all_of(names(conflict_list)),
                  ~ replace(.x, .data$DateTime_rd %in% conflict_list[[cur_column()]],NA)))

  #fill and summarize to a single obs per datetime
  data_interp <- data_interp %>% arrange(.data$DateTime_rd, .data$DupNum) %>%
    group_by(.data$DateTime_rd) %>%
    tidyr::fill(any_of(par_names), .direction = "downup") %>%
    slice(1) %>% ungroup()

  return(list(fill = data_fill, interp = data_interp))

}

#' Interpolate data gaps
#'
#' Uses the specified method to fill NA values for a given parameter within the dataset.
#'
#' @param data_interp `data.frame` from `prep_interp()` with dupes condensed.
#' @param y_var Variable being interpolated.
#' @param method The method to use for interpolation, options include:
#' `linear`, `spline`, `random_forest`, and `ts_interp` see details for further information about the methods.
#' @param freq The period interval for the time series in days, only used if `method` is `ts_interp`.
#'
#' @returns a `data_interp` with a extra column added:
#' - `yvar_fill`: Filled values for `y_var`.
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
#' interp_dfs <- prep_interp(example_sondeproj)
#' filled_yvar <- run_interp(interp_dfs$interp, "fDOM_QSU", "linear")
run_interp <- function(data_interp, y_var, method, freq=1){
  stopifnot(is.data.frame(data_interp))

  if(method == "linear"){
    yvar_fill <- zoo::na.approx(data_interp[[y_var]], na.rm=FALSE)}

  if(method == "spline"){
    yvar_fill <- zoo::na.spline(data_interp[[y_var]], na.rm=FALSE)}

  if(method == "random_forest"){
    filled <- data_interp %>%
      select(-any_of(c("DateTime_rd", "FileName", "Date", "Time_HH_mm_ss",
                       "DateTime", "Site_Name", "DupNum", "fill_flag"))) %>%
      as.data.frame() %>% missForest::missForest()

    #only fill to max gap
    yvar_fill <- filled$ximp[[y_var]]}

  if(method == "ts_interp"){
    interval <- get_interval(data_interp)

    #make time series
    var_ts <- ts(data_interp[[y_var]], frequency = freq*24*(60/interval))
    #only fill to max gap
    yvar_fill <- as.numeric(forecast::na.interp(var_ts))}

  #save as df that we can join with
  data_interp$yvar_fill <- yvar_fill

  return(data_interp)
}

#' Map interpolated data back to dataset
#'
#' Uses interpolated values to attempt to fill in missing data being aware of
#' maximum gap lengths to fill and duplicates.
#'
#' @param data_fill `data.frame` based on `proj$data` with missing `datetime` values added from `prep_interp()`.
#' @param data_interp `data.frame` based on `proj$data` with duplicates condensed to a single value and missing values interpolated from `run_interp()`.
#' @param y_var Variable being interpolated.
#' @param max_length The maximum length in hours to fill via interpolation.
#'
#' @returns `data_fill` with missing values interpolated.
#' @export
#' @md
#'
#' @examples
#' interp_dfs <- prep_interp(example_sondeproj)
#' filled_yvar <- run_interp(interp_dfs$interp, "fDOM_QSU", "linear")
#' data_filled <- apply_interp(interp_dfs$fill, filled_yvar, "fDOM_QSU", 8)
apply_interp <- function(data_fill, data_interp, y_var, max_length){
  interval <- get_interval(data_fill)

  yvar_fill <- .fill_short_gaps(data_interp[[y_var]],
                                data_interp$yvar_fill,
                                maxgap = max_length * (60 / interval))

  fill_df <- data.frame(DateTime_rd = data_interp$DateTime_rd,
                        yvar_fill = yvar_fill)

  #track which values we want to fill in (ignoring gap size)
  data_fill <- data_fill %>% group_by(.data$DateTime_rd) %>%
    mutate(n_dup = n(), n_non_na = sum(!is.na(.data[[y_var]]))) %>%
    mutate(fill_flag = ifelse(is.na(.data[[y_var]]) & (.data$n_dup == 1 | .data$n_non_na == 0 & .data$DupNum == 1), TRUE,FALSE))

  #map interpolated data back
  data_fill <- data_fill %>% left_join(fill_df, by="DateTime_rd") %>%
    mutate(!!y_var := ifelse(.data$fill_flag, .data$yvar_fill, .data[[y_var]])) %>%
    select(-(c("n_dup", "n_non_na", "yvar_fill"))) %>% ungroup() %>% arrange(.data$DateTime_rd, .data$DupNum) %>%
    mutate(Index = 1:n()) %>% relocate("DateTime_rd", .after = "DateTime")

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


