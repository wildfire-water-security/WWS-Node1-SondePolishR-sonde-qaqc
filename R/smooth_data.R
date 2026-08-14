#' Apply smoothing functions to data
#'
#' Applies the selected smoothing method to the specified data range.
#'
#' @param data A `data.frame` with the data to smooth (must have the columns Index and y_var)
#' @param y_var Character specifying the variable to apply smoothing to.
#' @param method Character specifying the method to use for smoothing. Options include "rollmean", "rollmedian",
#' "savgol", and "kalman". See details for details on the different methods.
#' @param k A adjustment parameter for the selected method (see details).
#' @param index Index values of the rows that need to be smoothed
#'
#' @returns data with the values within `range` replaced with the smoothed values
#' @md
#' @export
#'
#' @details
#' The following smoothing methods are currently supported:
#' - **rollmean**: Based on the `rollmean` function from the `zoo` package.
#' Replaces data with a rolling mean value. The `k` parameter is used to control the number of points
#' to include in the mean calculation.
#'
#' - **rollmedian**: Based on the `rollmedian` function from the `zoo` package.
#' Replaces data with a rolling median value. The `k` parameter is used to control the number of points
#' to include in the median calculation, should be odd but will convert to an odd number with a warning.
#'
#' - **savgol**: Based on the `savgol` function from the `pracma` package.
#' Applies a Savitzky–Golay filter which fits a fourth order polynomial to a sliding range of data.
#' The `k` parameter is used controls the number of points in the window.
#'
#' - **savgol**: Based on the `dlmSmooth` function from the `dlm` package.
#' Applies a Kalman filter which process model to the data.
#' The `k` parameter is used controls the amount of smoothing.
#'
#' @examples
#' smoothed <- apply_smoothing(example_data, "Temp_C", "rollmean", k=100)
#' ggplot(example_data, aes(x=DateTime_rd, y=Temp_C)) +
#' geom_line(color="black", na.rm=TRUE) +
#' geom_line(data=smoothed, color="darkred", na.rm=TRUE)
apply_smoothing <- function(data, y_var, method, index=NULL, k=7){
  stopifnot(method %in% c("rollmean", "rollmedian", "savgol", "kalman"))

  #get indices if not specified
  if(is.null(index)){index <- data$Index}

  #if using rolling methods that have NA's at the start, grab a little extra
  if(method %in% c("rollmean", "rollmedian")){
    newstart <- ifelse(min(index)-k < 1, 1, min(index)-k)
    newend <- ifelse(max(index)+k > length(index), max(index), max(index)+k)
    smooth_idx <- seq(from=newstart, to =newend, by=1)
  }else{
    smooth_idx <- index
  }

  #pull out just data to smooth with a little extra for starting and ending to prevent unneeded NA's
    #get wider index if a subset of data, if not outside the bounds
    #index should directly map to rows, but in case....
    subdat <- data %>% filter(.data$Index %in% smooth_idx)
    vals <- subdat %>% pull(.data[[y_var]])

  #apply smoothing method
  if(method == "rollmean"){
    filled <- rollmean(vals, na.pad=TRUE, k=k)
  }

  if(method == "rollmedian"){
    filled <- rollmedian(vals, na.pad=TRUE, k=k)
  }

  if(method == "savgol"){
    filled <- savgol(vals, k, forder = 4, dorder = 0)
  }

  if(method == "kalman"){
    filled <- dlmSmooth(vals, dlmModPoly(1, dV = 10, dW = k))$s[-1]
  }

  #replace data with smoothed data
  keep <- smooth_idx %in% index
  filled <- filled[keep]

  data[[y_var]][data$Index %in% index] <- filled

  #return smoothed data
  return(data)
}
