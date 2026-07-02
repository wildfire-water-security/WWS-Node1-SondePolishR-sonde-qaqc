
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

  if(is.null(index) || all(is.na(index))){
    return(list(slope = 0, int = 0))
  }

  start <- min(index, na.rm = TRUE)
  end <- max(index, na.rm = TRUE)

  vals <- data[[par]]

  y <- vals[index]

  t1 <- vals[start - 1]
  t2 <- vals[end + 1]

  # edge cases
  if(is.na(t1) & is.na(t2)){
    return(list(slope = 0, int = 0))
  }else if(length(t1) == 0 || is.na(t1)){
    add <- rep(t2 - vals[end], length(index))

  } else if(length(t2) == 0 || is.na(t2)){
    add <- rep(t1 - vals[start], length(index))

  } else {

    target <- seq(t1, t2, length.out = length(index) + 2)[2:(length(index)+1)]

    add <- target - y
  }

  #determine slope and int
  slope <- round(add[2] - add[1], 3)
  int <- round(add[1], 3)

  #correct if it's a single point
  slope <- ifelse(is.na(slope), 0 , slope)

  list(
    slope = slope,
    int = int
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

#' Apply a drift correction to a parameter
#'
#' Applies a linear correction to data based on the final "corrected" value often determined via a freshly calibrated sonde compared
#' to the sonde that's been deployed ("uncorrected").
#'
#' @param x Vector of data to apply correction to.
#' @param rows Row numbers of the values within x that should be corrected.
#' @param corrected Value of the corrected end value, used to determine how much to shift data.
#' @param uncorrected Value of the uncorrected value, used to determine how much to shift data.
#'
#' @returns `x` with the drift shift applied.
#' @export
#'
#' @examples
#' rows <- example_data$FileName == "example-data1.csv"
#' x_shift <- apply_drift_shift(example_data$fDOM_QSU, rows, 17.49, 21.71)
apply_drift_shift <- function(x, rows, corrected, uncorrected){
  n <- sum(rows)

  # amount needed at final point (using paired check, resident)
  d <- corrected - uncorrected

  # increasing additive correction
  add <- d * ((seq_len(n) - 1) / (n - 1))

  x[rows] <- x[rows] + add

  return(x)
}

#' Guess optimal values for drift correction
#'
#' Uses the calibration check data when possible to guess values for a drift correction. If not available
#' it will guess based on the values at the end of the value and the starting values of the next file.
#'
#' @param data a data.frame with sonde data
#' @param calcheck the `calcheck` from the sondeproj
#' @param par the parameter being corrected
#' @param file the name of the file to correct
#'
#' @noRd

guess_drift <- function(data, calcheck, par, file){
  rows <- data$FileName == file

  #get potential calcheck data (if available)
  if(!is.null(calcheck)){
    par_calcheck <- calcheck %>%
      filter(.data$Parameter == par & .data$Date == as.Date(max(data$DateTime[rows])))
  }else{par_calcheck <- NULL}

  if(!is.null(par_calcheck) && nrow(par_calcheck) == 1){
    ##update uncorrected and corrected value in UI to resident and check values
    uncorrected <- par_calcheck$Resident_Value
    corrected <- par_calcheck$Check_Value
  }else{
    row_num <- which(rows)
    #we guess from data
    #get median 5 points before file ends
    endvals <- data[[par]][(max(row_num)-4):max(row_num)]
    newvals <- data[[par]][(max(row_num)+1):(max(row_num)+5)]

    uncorrected <- median(endvals, na.rm = TRUE)
    corrected <- median(newvals, na.rm = TRUE)
  }

  #guard against NA
  if(is.na(corrected) & is.na(uncorrected)){
    corrected <- uncorrected <- 0
  }
    uncorrected <- ifelse(is.na(uncorrected), corrected, uncorrected)
    corrected <- ifelse(is.na(corrected), uncorrected, corrected)

 return(list(correct = corrected, uncorrect = uncorrected))
}
