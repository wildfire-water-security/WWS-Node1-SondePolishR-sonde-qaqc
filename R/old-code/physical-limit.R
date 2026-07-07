#' Remove data outside of physical limits
#'
#' A good first QA/QC check for sonde data is to determine if there are any values that aren't physically possible.
#'
#' @param data a data.frame with sonde data
#' @param min the minimum physical value
#' @param max the maximum physical value
#' @param par the parameter to check
#' @param keep row numbers to exclude from the check (used to omit values which user has said are acceptable)
#'
#' @md
#' @returns
#' a list with two data.frames:
#' - **outlier:** the rows of data that have values outside the limits not excluded using keep
#' - **within:** the rows of data that have values within the limits or have been kept using `keep`
#' @export
#'
#' @examples
#' results <- physical_limit(raw_sonde, min=0, max=98, par="ODO_%_sat")
#' rownames(results$outlier)
#'
#' results <- physical_limit(raw_sonde, min=0, max=98, par="ODO_%_sat", keep=1436)
#' rownames(results$outlier)
#'
physical_limit <- function(data, min, max, par, keep=NULL){
  stopifnot(is.numeric(min), is.numeric(max), par %in% colnames(data))

  flag <- which(data[[par]] < min | data[[par]] > max)

  if(!is.null(keep)){
    flag <- flag[!(flag %in% keep)]
  }

  if(length(flag) > 0){
    outlier <- data[flag,]
    within <- data[-flag,]
  }else{
    outlier <- data.frame()
    within <- data
  }

  return(list(outlier=outlier, within=within))
}
