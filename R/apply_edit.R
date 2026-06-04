#' Log edits to a `sondeproj`
#'
#' Uses a list of edit parameters to update a `sondeproj` with the specified edits. Makes
#' changes to the data, changelog, and flags.
#'
#' @param proj A `sondeproj` object holding sonde data.
#' @param edit A list of length six:
#' - data: new updated data as a `data.frame`
#' - rows: logical vector which specifies rows changed as TRUE
#' - y_var: parameter being edited
#' - step: name of the editing step for the changelog
#' - note: an optional note to add to the changelog
#' - flag: character flag to use for edits to the data
#' - changetype: character specifying where to add flag, either "flag_rm", "flag_chg", or "flag_add"
#'
#' @returns A `sondeproj` object with edits made.
#' @noRd
#'
apply_edit <- function(proj, edit){
  stopifnot(is.list(edit))

  #extract data
    olddata <- proj$data
    newdata <- edit$data

  #get diff
    dif <- list(get_diff(olddata, newdata, id=c("DateTime_rd", "DupNum")))
    names(dif) <- diff_version(proj) #give name to list item


  #apply flags to project
    proj$flags[[edit$changetype]][[edit$y_var]][edit$rows] <- edit$flag

  #update log entry
    proj <- write_log(proj, edit$y_var, edit$step, n=sum(edit$rows, na.rm=TRUE),
                      note = edit$note, diff_name = names(dif), return = "sondeproj")

  #add in new df and diff
    proj$data <- newdata
    proj$diffs <- c(proj$diffs, dif)

  return(proj)
}
