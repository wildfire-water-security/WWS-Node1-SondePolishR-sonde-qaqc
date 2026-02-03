#initialize log (will need to clear when a new file is loaded)

log <- data.frame(datetime=as.POSIXct(character()),
                  parameter = character(),
                  step = character(),
                  n_changed = numeric(),
                  user = character(),
                  version = character())  # initialize log

data_ver <- list() #initialize list for data
prj_path <- character() #initialize path for data

#initialize environment
.pkgenv <- rlang::new_environment(data = list(log = log, data_ver =data_ver, prj_path = prj_path), parent = rlang::empty_env())

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
#' @param env the environment in which configuration settings is stored. Defaults to the package environment.
#'
#' @returns a `data.frame` with the log from the package environment
#' @export
#' @rdname change-log
#' @md
#'
#' @examples
#' get_log()
#'
#' write_log("Cond_S_cm", "physical limits", 5, "V1")
  get_log <- function(env = .pkgenv) {
    rlang::env_get(env, "log")
  }

#' @export
#' @rdname change-log
  write_log <- function(par, step, n, version, datetime = Sys.time(), user=Sys.info()[["user"]], env = .pkgenv){

    log_row <- data.frame(datetime=datetime, parameter=par,
                          step = step, n_changed = n,
                          user = user,
                          version=version)

    new_log <- rbind(get_log(), log_row)
    rlang::env_bind(env, log = new_log)

  }

#' @export
#' @rdname change-log
  clear_log <- function(env = .pkgenv){
    clear_log <- data.frame(datetime=as.POSIXct(character()),
                                    parameter = character(), step = character(),
                                    n_changed = numeric(),
                                    user = character(),
                                    version = character())  # initialize log

    rlang::env_bind(env, log = clear_log)

  }

#' @export
#' @rdname change-log
  set_log <- function(log, env = .pkgenv){
    rlang::env_bind(env, log = log)
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
    rlang::env_bind(env, prj_path = character())
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

  #browser()
  #get saved versions
  current <- get_data()

  if (length(current) == 0) {
    return(TRUE)
  }

  last <- current[[length(current)]]

  !isTRUE(all.equal(data, last))
}
