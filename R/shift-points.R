
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
#' guess_shift(example_sondeproj$data, "ODO_mg_L", 5:7)

guess_shift <- function(data, par, index){
  if(is.null(index)){
    return(list(slope = 0, int = 0))
  }

  start <- min(index, na.rm = TRUE)
  end <- max(index, na.rm = TRUE)

  vals <- data[[par]]

  y <- vals[index]

  t1 <- vals[start - 1]
  t2 <- vals[end + 1]

  # edge cases
  if(length(t1) == 0 || is.na(t1)){
    add <- rep(t2 - vals[end], length(index))

  } else if(length(t2) == 0 || is.na(t2)){
    add <- rep(t1 - vals[start], length(index))

  } else {

    target <- seq(t1, t2, length.out = length(index) + 2)[2:(length(index)+1)]

    add <- target - y
  }

  list(
    slope = round(add[2] - add[1], 3),
    int = round(add[1], 3)
  )
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
#' example_sondeproj$data$ODO_mg_L[5:7]
#' data <- shift_points(example_sondeproj$data, "ODO_mg_L", 5:7)
#' data$ODO_mg_L[5:7]

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
