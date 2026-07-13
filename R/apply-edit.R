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
#' @export
#'
#' @examples
#' data <- example_data
#' data$fDOM_QSU[1:4] <- NA
#' rows <- rep(FALSE, nrow(data))
#' rows[1:4] <- TRUE
#' edit <- list(data = example_data,
#'              rows = rows,
#'              y_var = "fDOM_QSU",
#'              step = "outlier removal",
#'              note = "example edit",
#'              flag = "RM07",
#'              changetype = "flag_rm")
#' updated_proj <- apply_edit(example_sondeproj, edit)
#'
apply_edit <- function(proj, edit){
  stopifnot(is.list(edit))

  #skip applying edit if no rows changed
  if(sum(edit$rows) == 0){
    return(proj)
  }

  #extract data
    olddata <- proj$data
    newdata <- edit$data

  #get diff
    dif <- list(get_diff(olddata, newdata, id=c("DateTime_rd", "DupNum")))
    names(dif) <- diff_version(proj) #give name to list item

  #apply flags to project
    #update flags if adding new rows
    if(nrow(proj$flags$flag_rm) != nrow(edit$data)){
      proj <- add_flags(proj, edit$data)
    }

  #add flags preserving any existing flags
    old_flags <- proj$flags[[edit$changetype]][[edit$y_var]]
    new_flags <- rep(edit$flag, length(old_flags))
    new_flags[!edit$rows] <- NA

    apply_flag <- function(old, new){
      old_flags <- unlist(strsplit(old, ";"))
      new_flags <- na.omit(unique(c(old_flags, new)))
      ifelse(length(new_flags) >0, paste(new_flags, collapse = ";"), NA)
    }
    proj$flags[[edit$changetype]][[edit$y_var]] <- sapply(1:length(old_flags), function(x) apply_flag(old_flags[x], new_flags[x]))

  #update log entry
    proj <- write_log(proj, edit$y_var, edit$step, n=sum(edit$rows, na.rm=TRUE),
                      note = edit$note, diff_name = names(dif), return = "sondeproj")

  #add in new df and diff
    proj$data <- newdata
    proj$diffs <- c(proj$diffs, dif)

  return(proj)
}
