
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
                            analyst = character(),
                            analyst_note = character(),
                            review_status = character(),
                            review_date = as.POSIXct(character()),
                            reviewer = character(),
                            review_note = character(),
                            diff_name = character())
    }else{
     old_log <- sondeproj$changelog
    }

    log_row <- data.frame(datetime=datetime,
                          parameter=par,
                          step = step,
                          n_changed = n,
                          analyst = user,
                          analyst_note=note,
                          review_status = "pending",
                          review_date = as.POSIXct(NA),
                          reviewer = NA,
                          review_note = NA,
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
#' Add review info to changelog
#'
#' Allows analyst or reviewer notes and status changes to be added to the existing changelog based on the name of the diff.
#'
#' @param sondeproj the sonde project to get the change log from. If `NULL` will return a blank change log structure.
#' @param dd the name of the diff in the changelog.
#' @param status the review status of the change, should be one of the following: "pending", "approved", "returned", "justified"
#' @param analyst either NULL if it's a reviewer change or the username of the analyst
#' @param reviewer either NULL if it's an analyst change or the username of the person who did the review
#' @param note a note from the analyst or reviewer about the change made
#'
#' @returns the `sondeproj` is returned with the change log updated.
#' @noRd
#'
#' @examples
#' proj <- update_log(example_sondeproj, "dd2", "approved", reviewer="J. Smith", note = "change looks fine")
#' proj$changelog
update_log <- function(sondeproj, dd, status, analyst=NULL, reviewer=NULL, note = ""){
    stopifnot(is.null(sondeproj) || inherits(sondeproj, "sondeproj"), status %in% c("pending", "approved", "returned", "justified"))

    log <- sondeproj$changelog #pull out log
    row <- log$diff_name == dd #get row to edit

    #if analyst change update note
    if(!is.null(analyst)){
      log$analyst_note[row] <- note
      log$review_status[row] <- status
      log$analyst[row] <- paste(unique(c(log$analyst[row], analyst)), collapse="/") #preserve all
    }

    #if reviewer update with that info
    if(!is.null(reviewer)){
      log$reviewer[row] <- reviewer
      log$review_date[row] <- Sys.time()
      log$review_status[row] <- status
      log$review_note[row] <- note
    }

    sondeproj$changelog <- log

    return(sondeproj)
  }
