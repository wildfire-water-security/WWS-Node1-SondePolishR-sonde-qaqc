
#' Save version history and sonde data as a project
#'
#' Combines the change history with the version history of the sonde data and saves to a
#' `.qs` file to prevent data loss and enable work over multiple days.
#' @rdname read-sonde-project
#' @param data_ver the data version object (a list)
#' @param log the change log object (a data.frame)
#' @param prj_path the path to save the project to including the file name
#' @md
#' @returns saves data_ver and log as a .qs file
save_project <- function(data_ver, log, prj_path){
  stopifnot(inherits(data_ver, "list"), inherits(log, "data.frame"), is.character(prj_path))

  #add log to data_ver
  sonde_prj <- append(list(log), data_ver)
  names(sonde_prj)[1] <- "change_log"

  #save as .qs
  qs::qsave(sonde_prj, prj_path)

}

#' @rdname read-sonde-project
#' @export
read_project <- function(prj_path){
  stopifnot(tools::file_ext(prj_path) == "qs")

  #read into R
  sonde_prj <- qs::qread(prj_path)

  #assign to package envir
  set_log(sonde_prj[[1]])

  data_ver <- sonde_prj[-1]
  set_data(data_ver)

}
