
#' Guess the amount to shift observations
#'
#' Sometimes a continuous sonde signal will be shifted up or down by a consistent value for a period of time. This function will use the
#' data before and after the selected observations to guess the appropriate value to correct the data by
#'
#' @param data a data.frame with sonde data
#' @param par the parameter being corrected
#' @param index the index values of the rows that need to be shifted

#' @returns a numeric with the guessed shift value based on the parameter
#' @export
#'
#' @examples
#' guess_shift(raw_sonde, "Cond_uS_cm", 1591:1630)

guess_shift <- function(data, par, index){
  #get start and end of points to shift
    start <- min(index, na.rm = TRUE)
    end <- max(index, na.rm = TRUE)

  #get values
    vals <- data[[par]]

  #get values of data to shift and good data
    y1 <- vals[start]
    y2 <- vals[end]

    t1 <- vals[start - 1]
    t2 <- vals[end + 1]

  #calculate slope and intercept
    b <- (t2 - t1) / (y2 - y1)
    a <- t1 - b * y1

  #TODO: account for issues at start and end of data -> use a absolute shift
  #return
    return(list(slope = b, int=a))

  #
  # #don't get values before start
  #   before <- dplyr::lag(vals, n=3)[start:(start+2)]
  #
  #   if(all(is.na(before))){start_dif <- NA}else{
  #     start_dif <- stats::weighted.mean(before -  vals[start], w = (1:3/3), na.rm=TRUE)
  #   }
  #
  # #check difference between start and points before
  #   after <- dplyr::lead(vals, n=3)[end:(end+2)]
  #   if(all(is.na(after))){end_dif <- NA}else{
  #     end_dif <- stats::weighted.mean(after -  vals[end], w = (3:1/3), na.rm=TRUE)
  #     }
  #
  # #guess shift
  #   slope <- (mean(after) - mean(before)) / (vals[end] - vals[start])
  #   int <- start_dif
  #
  #   shift <- mean(c(start_dif, end_dif), na.rm=TRUE)
  #
  # #protect from errors
  #   if(!is.numeric(shift) | is.nan(shift)){
  #    return(0)}else{
  #      return(round(shift, digits=2))
  #    }

}

#' Correct points via an absolute shift
#'
#' Sometimes a continuous sonde signal will be shifted up or down by a consistent value for a period of time, this will adjust the
#' dataset for those values by that set amount.
#' @md
#' @param data a data.frame with sonde data
#' @param par the parameter being corrected
#' @param index the index values of the rows that need to be shifted
#' @param shift a list of the slope and int (intercept) to use to shift the data by, if `NULL`, it will be guessed using \link[SondePolishR]{guess_shift}
#'
#' @returns a data.frame with the values adjusted
#' @export
#'
#' @examples
#' raw_sonde$Cond_uS_cm[1591:1600]
#' data <- shift_points(raw_sonde, "Cond_uS_cm", 1591:1630)
#' data$Cond_uS_cm[1591:1600]

shift_points <- function(data, par, index, shift_val=NULL){
  stopifnot(inherits(data, "data.frame"), is.character(par))

  #if not specified, guess
  if(is.null(shift_val)){
    shift_val <- guess_shift(data, par, index)
  }

  #get new points
  data[index, par] <- (data[index, par] * shift_val$slope) + shift_val$int

  return(data)
}
