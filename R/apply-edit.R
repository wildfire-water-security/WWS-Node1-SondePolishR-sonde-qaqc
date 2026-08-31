#' Log edits to a `sondeproj`
#'
#' Uses a list of edit parameters to update a `sondeproj` with the specified edits. Makes
#' changes to the data, changelog, and flags.
#'
#' @param proj A `sondeproj` object holding sonde data.
#' @param edit A list of length six:
#' - data: new updated data as a `data.frame`
#' - rows: vector of indices that were changed
#' - y_var: parameter being edited
#' - step: name of the editing step for the changelog
#' - note: an optional note to add to the changelog
#' - flag: character flag to use for edits to the data
#'
#' @returns A `sondeproj` object with edits made.
#' @export
#' @md
#' @examples
#' data <- example_data
#' data$fDOM_QSU[1:4] <- NA
#' rows <- c(1:4)
#' rows[1:4] <- TRUE
#' edit <- list(data = example_data,
#'              rows = rows,
#'              y_var = "fDOM_QSU",
#'              step = "outlier removal",
#'              note = "example edit",
#'              flag = "RM07")
#' updated_proj <- apply_edit(example_sondeproj, edit)
#'
apply_edit <- function(proj, edit){
  stopifnot(is.list(edit), inherits(proj, "sondeproj"))

  #skip applying edit if no rows changed
  if(length(edit$rows) == 0){
    return(proj)
  }

  #extract data
    olddata <- proj$data
    newdata <- edit$data

  #apply flags to data preserving any existing flags
    #if all loop through
    index <- edit$rows
    if(edit$y_var == "all"){
      y_vars <- get_parms(olddata)
      for(v in y_vars){newdata <- add_flags(newdata, v,index, edit$flag)}
    }else{
      newdata <- add_flags(newdata, edit$y_var,index, edit$flag)
    }

  #get diff (must be after applying flags)
    dif <- list(get_diff(olddata, newdata, id=c("DateTime_rd", "DupNum")))
    names(dif) <- diff_version(proj) #give name to list item

  #update log entry
    proj <- write_log(proj, edit$y_var, edit$step, n=length(edit$rows),
                      note = edit$note, diff_name = names(dif), return = "sondeproj")

  #add in new df and diff
    proj$data <- newdata
    proj$diffs <- c(proj$diffs, dif)

  return(proj)
}
