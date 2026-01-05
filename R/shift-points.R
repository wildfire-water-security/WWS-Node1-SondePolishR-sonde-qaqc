
#' Guess the amount to shift observations
#'
#' Sometimes a continuous sonde signal will be shifted up or down by a consistent value for a period of time. This function will use the
#' data before and after the selected observations to guess the appropriate value to correct the data by
#'
#' @param df a data.frame with sonde data
#' @param par the parameter being corrected
#' @param index the index values of the rows that need to be shifted

#' @returns a numeric with the guessed shift value based on the parameter
#' @export
#'
#' @examples
#' guess_shift(raw_sonde, "Cond_S_cm", 1591:1630)
guess_shift <- function(df, par, index){
  start <- min(index, na.rm = TRUE)
  end <- max(index, na.rm = TRUE)

  #check difference between start and points before
    before <- df[(start-5):(start-1),par]
    start_dif <- mean(before, na.rm = TRUE) -  df[start, par]

  #check difference between start and points before
    after <- df[(end+1):(end+5),par]
    end_dif <- mean(after, na.rm = TRUE) -  df[end, par]

 #guess shift
    shift <- mean(start_dif, end_dif, na.rm=TRUE)

  return(shift)
}

#' Correct points via an absolute shift
#'
#' Sometimes a continuous sonde signal will be shifted up or down by a consistent value for a period of time, this will adjust the
#' dataset for those values by that set amount.
#' @md
#' @param df a data.frame with sonde data
#' @param par the parameter being corrected
#' @param index the index values of the rows that need to be shifted
#' @param shift_val the value to shift the selected data by, if `NULL`, it will be guessed using \link[SondePolishR]{guess_shift}
#'
#' @returns a data.frame with the values adjusted
#' @export
#'
#' @examples
#' raw_sonde$Cond_S_cm[1591:1600]
#' df <- shift_points(raw_sonde, "Cond_S_cm", 1591:1630)
#' df$Cond_S_cm[1591:1600]

shift_points <- function(df, par, index, shift_val=NULL){
  stopifnot(inherits(df, "data.frame"), is.character(par))

  #if not specified, guess
  if(is.null(shift_val)){
    shift_val <- guess_shift(df, par, index)
  }

  #get new points
  df[index, par] <- df[index, par] + shift_val

  return(df)
}
