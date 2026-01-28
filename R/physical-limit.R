#' Remove data outside of physical limits
#'
#' A good first QA/QC check for sonde data is to determine if there are any values that aren't physically possible.
#'
#' @param df a data.frame with sonde data
#' @param min the minimum physical value
#' @param max the maximum physical value
#' @param par the parameter to check
#' @param keep row numbers to exclude from the check (used to omit values which user has said are acceptable)
#'
#' @md
#' @returns
#' a list with two data.frames:
#' - **outlier:** the rows of df that have values outside the limits not excluded using keep
#' - **within:** the rows of df that have values within the limits or have been kept using `keep`
#' @export
#'
#' @examples
#' results <- physical_limit(raw_sonde, min=0, max=98, par="ODO_%_sat")
#' rownames(results$outlier)
#'
#' results <- physical_limit(raw_sonde, min=0, max=98, par="ODO_%_sat", keep=1436)
#' rownames(results$outlier)
#'
physical_limit <- function(df, min, max, par, keep=NULL){
  stopifnot(is.numeric(min), is.numeric(max), par %in% colnames(df))

  flag <- which(df[[par]] < min | df[[par]] > max)

  if(!is.null(keep)){
    flag <- flag[!(flag %in% keep)]
  }

  if(length(flag) > 0){
    outlier <- df[flag,]
    within <- df[-flag,]
  }else{
    outlier <- data.frame()
    within <- df
  }

  return(list(outlier=outlier, within=within))
}
