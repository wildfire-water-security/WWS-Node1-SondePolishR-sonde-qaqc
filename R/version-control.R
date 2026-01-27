#initialize log (will need to clear when a new file is loaded)
.SondePolishR <- new.env(parent = emptyenv())
.SondePolishR$log <- data.frame(datetime=as.POSIXct(character()),
                                parameter = character(),
                                step = character(),
                                n_changed = numeric(),
                                user = character(),
                                version = character())  # initialize log
.SondePolishR$data_ver <- list()

#' Get and Write to Change Log
#'
#' Log is a `data.frame` stored in the package environment used to keep track of the changes
#' made to the initial raw sonde data.
#'
#' @param par the name of the parameter modified
#' @param step a description of the type of change made
#' @param n the number of points modified
#' @param version the associated file version name
#' @param datetime the date and time the change was made
#' @param user the username of the person who made the change
#' @param log a existing change log read in from read_project
#' @returns a `data.frame` with the log from the package environment
#' @export
#' @rdname change-log
#' @md
#'
#' @examples
#' get_log()
#'
#' write_log("Cond_S_cm", "physical limits", 5, "V1")
  get_log <- function() {
    if (!exists("log", envir = .SondePolishR)) {
      return(NULL)
    }
    .SondePolishR$log
  }

#' @export
#' @rdname change-log
  write_log <- function(par, step, n, version, datetime = Sys.time(), user=Sys.info()[["user"]]){
    if (!exists("log", envir = .SondePolishR)) {
      .SondePolishR$log <- data.frame(datetime=as.POSIXct(character()),
                 parameter = character(), step = character(),
                 n_changed = numeric(),
                 user = character(),
                 version = character())  # initialize log
    }

    log_row <- data.frame(datetime=datetime, parameter=par,
                          step = step, n_changed = n,
                          user = user,
                          version=version)

    .SondePolishR$log <- rbind( .SondePolishR$log, log_row)


  }

#' @export
#' @rdname change-log
  clear_log <- function(){
    .SondePolishR$log <- data.frame(datetime=as.POSIXct(character()),
                                    parameter = character(), step = character(),
                                    n_changed = numeric(),
                                    user = character(),
                                    version = character())  # initialize log
  }

#' @export
#' @rdname change-log
  set_log <- function(log){
    .SondePolishR$log <- log
  }

#' Sonde data versioning
#'
#' Stores edits to the raw sonde data as objects in a list so edits can be undone.
#'
#' @param data a data.frame to add to the version history
#' @param version the name of the version
#' @param data_ver a existing data version history read in from read_project

#' @rdname data-versions
#' @md
#' @returns a list of `data.frames`
#' @export
#'
#' @examples
#' get_data()
#'
#' write_data(raw_sonde, "V1")
  write_data <- function(data, version){
    if (!exists("data_ver", envir = .SondePolishR)) {
      .SondePolishR$data_ver <- list()
    }


    .SondePolishR$data_ver[[version]] <- data
  }

#' @export
#' @rdname data-versions
  get_data <- function(){
    if (!exists("data_ver", envir = .SondePolishR)) {
      return(NULL)
    }
    .SondePolishR$data_ver
  }

#' @export
#' @rdname data-versions
  clear_data <- function(){
      .SondePolishR$data_ver <- list()
  }

#' @export
#' @rdname data-versions
  set_data <- function(data_ver){
    .SondePolishR$data_ver <- data_ver
  }

#' Checks if the data is different than the current saved version
#'
#' @param df the data.frame to check to see if it's new
#'
#' @returns TRUE if df differs from the previous version,
#' FALSE if df is the same as the previous version
#' @export
#'
new_version <- function(df){

  #browser()
  #get saved versions
  current <- get_data()

  #get most recent item
  current <- tail(current, n = 1)

  #see if the version of the data is new
  diff <- !identical(df, current)
  return(diff)
}
