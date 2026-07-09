
#' Write to Change Log
#'
#' The change log is a component of a `sondeproj` object used to keep track of the changes
#' made to the initial raw sonde data.
#'
#' @param sondeproj the sonde project to get the change log from. If `NULL` will return a blank change log structure.
#' @param par the name of the parameter modified
#' @param step a description of the type of change made
#' @param n the number of points modified
#' @param note a note from the analyst about the change made
#' @param diff_name the associated diff object
#' @param datetime the date and time the change was made
#' @param user the username of the person who made the change
#' @param return either `df` or `sondeproj` to specify if the changelog only should be returned or the `sondeproj` with
#' the change log updated.
#'
#' @returns If `return` is `df` returns the changelog as a `data.frame`.
#' If `return` is `sondeproj` the `sondeproj` is returned with the change log updated.
#' @export
#' @md
#'
#' @examples
#' write_log(NULL, "Cond_S_cm", "physical limits", 5, "making an example", "diff1")
  write_log <- function(sondeproj, par, step, n, note="", diff_name=NULL, datetime = Sys.time(), user=Sys.info()[["user"]],
                        return="df"){
    stopifnot(is.null(sondeproj) || inherits(sondeproj, "sondeproj"))

    if(is.null(sondeproj)){
      # initialize log
      old_log <- data.frame(datetime=as.POSIXct(character()),
                            parameter = character(),
                            step = character(),
                            n_changed = numeric(),
                            note = character(),
                            user = character(),
                            diff_name = character())
    }else{
     old_log <- sondeproj$changelog
    }

    log_row <- data.frame(datetime=datetime, parameter=par,
                          step = step, n_changed = n, note=note,
                          user = user,
                          diff_name=diff_name)

    new_log <- rbind(old_log, log_row)

    if(return == "df" | is.null(sondeproj)){
      return(new_log)
    }

    if(return == "sondeproj"){
      sondeproj$changelog <- new_log
      return(sondeproj)
    }
  }
