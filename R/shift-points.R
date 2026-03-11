
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

  #calculate slope and intercept (these are used calculate amount to add to val)
  if(length(t1) == 0 || is.na(t1)){
    #when start of data
    b <- 0
    a <- t2-y2
  }else if(length(t2) == 0 || is.na(t2)){
    #end of data
    b <- 0
    a <- t1 - y1
  }else{
    #otherwise calc slope
    b <- ((t2-y2) - (t1-y1)) / length(index)
    a <- t1- y1
    }

  #return
    return(list(slope = round(b, 3), int=round(a, 3)))


}

#' Correct points via an absolute shift
#'
#' Sometimes a continuous sonde signal will be shifted up or down by a consistent value for a period of time, this will adjust the
#' dataset for those values by that set amount.
#' @md
#' @param data a data.frame with sonde data
#' @param par the parameter being corrected
#' @param index the index values of the rows that need to be shifted
#' @param shift_val a list of the slope and int (intercept) to use to shift the data by, if `NULL`, it will be guessed using \link[SondePolishR]{guess_shift}
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
  add <- (shift_val$slope * (seq_along(index)-1)) + shift_val$int

  data[index, par] <- data[index, par] + add

  return(data)
}
