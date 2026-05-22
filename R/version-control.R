#' Get and apply changes to data
#'
#' Wrappers for `daff::diff_data()` and `daff::apply_patch()`. Natively `daff` functions don't play
#' nicely with dates and datetimes. So the wrapper confirms they're the same between data and
#' then temporarily removes them to get the diff without those columns. Used to succinctly track changes to
#' the raw data and revert changes when needed.
#'
#' @param olddata The original `data.frame` to compare to.
#' @param newdata The new `data.frame` you want to get the changes for.
#' @param diff  A `data_diff` object storing the difference between `olddata` and `newdata` or a list of `data_diff`.
#'
#' @returns
#' `commit_diff` returns a `data_diff` object storing the differences between `data.frames`. See `daff` package for more details.
#' `apply_diff` returns a `data.frame` with the `diff` applied.
#'
#' @export
#' @md
#' @rdname version-control
#'
#' @examples
#' data_edit <- example_data
#'
#' #make a change in newdata
#' data_edit$fDOM_QSU[1:4] <- NA
#' head(data_edit)
#'
#' #save difference
#' dd <- commit_diff(example_data, data_edit)
#'
#' #apply difference
#' data_edit2 <- apply_diff(example_data, dd)
#' head(data_edit2)
commit_diff <- function(olddata, newdata){
  #check that dates/datetime are exactly the same because we can't check these
  if(any(olddata$Date != newdata$Date)){
    stop("Dates are different between the two datasets, can't determine differences.")
  }

  if(any(olddata$DateTime != newdata$DateTime)){
    stop("Datetimes are different between the two datasets, can't determine differences.")
  }

  #rm date and datetime so we don't get warning
  dates <- olddata %>% dplyr::select("Date", "DateTime")
  olddata <- olddata %>% dplyr::select(-c("Date", "DateTime", "DateTime_rd"))
  newdata <- newdata %>% dplyr::select(-c("Date", "DateTime_rd"))

  #get diff
  dd <- daff::diff_data(olddata, newdata, ids=c("Index"))

  return(dd)
}

#' @export
#' @rdname version-control
apply_diff <- function(olddata, diff){
#loop through if multiple diffs
  if(inherits(diff, "list")){
    for(x in diff){olddata <- apply_diff(olddata, x)}

    return(olddata)
  }
  #pull out dates
  dates <- olddata %>% dplyr::select("Date", "DateTime", "DateTime_rd")
  olddata <- olddata %>% dplyr::select(-c("Date", "DateTime", "DateTime_rd"))

  #apply patch
  suppressWarnings(newdata <- daff::patch_data(olddata, diff))

  #put back in datetimes
  newdata <- newdata %>% dplyr::mutate(Date = dates$Date, .before="Time_HH_mm_ss") %>%
    dplyr::mutate(DateTime = dates$DateTime, .after="Time_HH_mm_ss") %>%
    dplyr::mutate(DateTime_rd = dates$DateTime_rd, .after="DateTime")

  return(newdata)
}
