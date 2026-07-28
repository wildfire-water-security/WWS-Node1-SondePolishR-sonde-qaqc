
#' Add new flag to dataset
#'
#' Safely adds a flag to a specific parameter without overwriting old flags. Also maintains a consistent order
#' so that additional flags can be identified with version control.
#'
#' @param data Data.frame with sonde data.
#' @param y_var The column to add flag to.
#' @param index Row numbers to add flag to.
#' @param flag Flag to add to the data.frame.
#' @md
#' @returns a data.frame with the same dimensions as `data` with the flags added to the appropriate column.
#' @export
#'
#' @examples
#' data <- add_flags(example_sondeproj$data, "fDOM_QSU", 2:7, "TEST01")
#' data$fDOM_QSU_flag[1:8]
add_flags <- function(data, y_var, index, flag){
  stopifnot(inherits(data, "data.frame"))

  if(length(index) > 0 && all(!is.na(index))){
    #get location of flag column
    coln <- which(paste0(y_var, "_flag") == colnames(data))

    #add flag
    flags <- data[[coln]]

    flags[index] <- lapply(flags[index], function(old_flag) {
      sort(unique(na.omit(c(old_flag, flag))))
    })

    data[[coln]] <- flags
  }

  return(data)

  }


#' Extract quality flags from sonde project
#'
#' @param data A `data.frame` containing the data and flags.
#' @param y_var Parameter being plotted.
#'
#' @returns a vector the same length as rows in the dataset with the nice name of the quality flag.
#' @noRd
get_qual_flags <- function(data, y_var){
  flags <- data[[paste0(y_var, "_flag")]]

  qual_flags <- ifelse(grepl("QUAL01",flags), "Bad", ifelse(grepl("QUAL02",flags), "Questionable", NA))

  return(qual_flags)

}
