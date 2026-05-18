
#' Get and Write to Change Log
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
#' @param log a existing change log read in from read_project
#' @param return either `df` or `sondeproj` to specify if the changelog only should be returned or the `sondeproj` with
#' the change log updated.
#'
#' @returns a `data.frame` with the log from `sondeproj`
#' @export
#' @rdname change-log
#' @md
#'
#' @examples
#' write_log("Cond_S_cm", "physical limits", 5, "making an example", "V1")
#' get_log()

  get_log <- function(sondeproj) {
     sondeproj$changelog
  }

#' @export
#' @rdname change-log
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
     old_log <- get_log(sondeproj)
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

#' Get and set project path variable in package environment
#'
#' Used to store the save path to a sonde project so the user doesn't
#' need to specify each time.
#' @param env the environment in which configuration settings is stored. Defaults to the package environment.
#' @param prj_path the file path including the file name to the save location of the Sonde project.
#' @export
#' @rdname prj-path
  get_prjpath <- function(env = .pkgenv){
    rlang::env_get(env, "prj_path")
  }

#' @rdname prj-path
#' @export
  set_prjpath <- function(prj_path, env = .pkgenv){
    rlang::env_bind(env, prj_path = prj_path)
  }

#' @rdname prj-path
#' @export
  clear_prjpath <- function(prj_path, env = .pkgenv){
    rlang::env_bind(env, prj_path = list(type=character(), path=character()))
  }

#' Sonde data versioning
#'
#' Stores edits to the raw sonde data as objects in a list so edits can be undone.
#'
#' @param data a data.frame to add to the version history
#' @param version the name of the version
#' @param data_ver a existing data version history read in from read_project
#' @param env the environment in which configuration settings is stored. Defaults to the package environment.

#' @rdname data-versions
#' @md
#' @returns a list of `data.frames`
#' @export
#'
#' @examples
#' get_data()
#'
#' write_data(raw_sonde, "V1")
  write_data <- function(data, version, env = .pkgenv){
    new_vers <- get_data()
    new_vers[[version]] <- data
    rlang::env_bind(env, data_ver = new_vers)
  }

#' @export
#' @rdname data-versions
  get_data <- function(env = .pkgenv){
    rlang::env_get(env, "data_ver")
  }

#' @export
#' @rdname data-versions
  clear_data <- function(env = .pkgenv){
    rlang::env_bind(env, data_ver = list())
  }

#' @export
#' @rdname data-versions
  set_data <- function(data_ver, env = .pkgenv){
    rlang::env_bind(env, data_ver = data_ver)
  }

#' Checks if the data is different than the current saved version
#'
#' @param data the data.frame to check to see if it's new
#'
#' @returns TRUE if data differs from the previous version,
#' FALSE if data is the same as the previous version
#' @export
#'
new_version <- function(data){
  #get saved versions
  current <- get_data()

  if (length(current) == 0) {
    return(TRUE)
  }

  last <- current[[length(current)]]

  !isTRUE(all.equal(data, last))
}
