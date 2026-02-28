
#' Save version history and sonde data as a project
#'
#' Combines the change history with the version history of the sonde data and saves to a
#' `.RDS` file to prevent data loss and enable work over multiple days.
#' @rdname read-sonde-project
#' @param data_ver the data version object (a list)
#' @param log the change log object (a data.frame)
#' @param prj_path the path to save the project to including the file name
#' @md
#' @returns `save_project` saves `prj_path`, `data_ver` and `log` as a `.RDS` file to the provided `prj_path`.
#' The format of the `.RDS` object is a list with the following items:
#' - **prj_path:** a character vector specifying the save location, this is used to guess the save location when reading in a project.
#' - **change_log:** a `data.frame` object detailing the changes made to the dataset.
#' - **raw:** a `data.frame` with the initial dataset as it was loaded in.
#' - **...:** other versions of the data specified by hash codes.
#'
#' `read_project` loads the project, updates the package environment with the project data, and returns `data_ver` as a list of `data.frames`
save_project <- function(data_ver, log, prj_path){
  stopifnot(inherits(data_ver, "list"), inherits(log, "data.frame"), is.null(resolve_path(prj_path)) || dir.exists(dirname(resolve_path(prj_path))))

  #add log to data_ver
  sonde_prj <- append(list(log), data_ver)
  names(sonde_prj)[1] <- "change_log"

  #add path to data_ver
  sonde_prj <- append(prj_path, sonde_prj)
  names(sonde_prj)[1] <- "prj_path"

  #save as .RDS
  saveRDS(sonde_prj, resolve_path(prj_path))

}

#' @rdname read-sonde-project
#' @export
read_project <- function(prj_path){
  stopifnot(tools::file_ext(prj_path) == "RDS")

  #read into R
  sonde_prj <- readRDS(prj_path)

  #assign to package envir
  set_log(sonde_prj$change_log)

  data_ver <- sonde_prj[-(1:2)]
  set_data(data_ver)

  set_prjpath(sonde_prj$prj_path)

  return(data_ver)
}
