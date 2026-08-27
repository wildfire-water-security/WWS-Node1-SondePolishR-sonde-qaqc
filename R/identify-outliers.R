#' Auto-detect potential bad data points
#'
#' Uses various filtering approaches to attempt to automatically identify data points that are likely
#' bad and need to be removed.
#'
#' @param data A `data.frame` with the data to smooth (must have the columns Index and y_var)
#' @param y_var Character specifying the variable to apply smoothing to.
#' @param method Character specifying the method to use for smoothing. Options include "hampel","rel_change", "high_var".
#' See details for more information about the different methods.
#' @param k A adjustment parameter for the selected method (see details).
#' @param t A adjustment parameter for the selected method (see details).
#'
#' @returns The indices of points within data flagged as "bad".
#' @md
#' @export
#'
#' @details
#' The following detection methods are currently supported:
#' - **hampel**: Based on the `hampel` function from the `pracma` package which identifies points
#' based on a median absolute deviation. The `k` parameter is used to control the number of points
#' to include in the median calculation, the `t` parameter is used to control the threshold required to be marked as bad.
#'
#' - **rel_change**: Based on the `rollmedian` function from the `zoo` package which identifies points
#' based on a relative change between the points before and after it. The `k` parameter is used to control the number of points
#' to include in the median calculation, the `t` parameter is used to control the threshold required to be marked as bad.
#'
#' - **high_var**: Used to determine regions of high variability. Uses rolling functions from the `zoo` package to determine
#' the difference between the point and it's rolling median, then the median absolute deviation (MAD) for these differences are caclulated.
#' If the median absolute deviation is greater than `t` times the overall data's mean MAD it will be marked as bad.
#'
#' @examples
#' identify_outliers(example_data, "fDOM_QSU", "hampel")

identify_outliers <- function(data, y_var, method, k=5, t=7){
  stopifnot(method %in% c("hampel", "rel_change", "high_var"), is.data.frame(data))

  x <- data[[y_var]] #get variable we're identifying

  #if number not odd and should be make odd
  if(k %% 2 == 0){k <- k + 1}

  if(method == "hampel"){
    # interpolate to temp fill gaps so filter will work
    x_fill <- zoo::na.approx(x, na.rm = FALSE)
    x_fill <- zoo::na.locf(x_fill, na.rm = FALSE)        # forward fill
    x_fill <- zoo::na.locf(x_fill, fromLast = TRUE)      # backward fill
    hampel_out <- pracma::hampel(x_fill, k, t)

    outlier <- rep(FALSE, length(x))
    outlier[hampel_out$ind] <- TRUE
  }

  if(method == "rel_change"){
    # interpolate to temp fill gaps so filter will work
    x_fill <- zoo::na.approx(x, na.rm = FALSE)
    x_fill <- zoo::na.locf(x_fill, na.rm = FALSE)        # forward fill
    x_fill <- zoo::na.locf(x_fill, fromLast = TRUE)      # backward fill
    rel_change_lead <- abs(x_fill - lead(x_fill)) / zoo::rollmedian(x_fill, k, fill= NA, align = "right") * 100
    rel_change_lag <- abs(x_fill - lag(x_fill)) / zoo::rollmedian(x_fill, k, fill= NA, align = "left") * 100

    outlier <- rel_change_lead >= t & rel_change_lag >= t
    outlier[is.na(outlier)] <- FALSE #deal with ending/starting NA
  }

  if(method == "high_var"){
    smooth <- zoo::rollmedian(x, k, fill = NA, align = "center") #get expected median
    resid <- x - smooth #see difference between smoothed and observed
    roll_mad <- zoo::rollapply(resid, k, fill = NA, mad,na.rm = TRUE, align = "center") #get median dev of residuals
    typical_mad <- mean(roll_mad, na.rm = TRUE) #mean not median since it is likely 0
    outlier <- roll_mad > typical_mad * t #is above threshold?
    outlier[is.na(outlier)] <- FALSE #deal with ending/starting NA

  }

  indices <- data[outlier,] %>% pull(.data$Index)

  return(indices)
}
