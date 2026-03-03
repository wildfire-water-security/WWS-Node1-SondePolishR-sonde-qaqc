#' Convert a path to an absolute path
#'
#' Returns a regular filepath as the same, but if it's a list will get correct absolute path
#'
#' @param x the type of path (package, project, absolute)
#' @param project_root the project path, potentially relative
#'
#' @noRd
#'
resolve_path <- function(x, project_root = getwd()) {
  if(!is.list(x) && is.character(x)){return(x)}

  stopifnot(is.list(x), !is.null(x$type), !is.null(x$path))

  if(length(x$type) == 0 & length(x$path) ==0){return(NULL)}
  switch(
    x$type,
    package = fs::path_package(x$path, package ="SondePolishR"),
    project = {
      file.path(project_root, x$path)
    },
    absolute = x$path,
    stop("Unknown path type")
  )
}

#' Determine how many files are in the directory of the same name
#'
#' Used to determine the number to use to append the file name with so as
#' to not overwrite an existing project.
#'
#' @param path the file path including the file name.
#'
#' @returns a modified path with the number appended correctly for the number of versions
#' @noRd
#'
version_path <- function(path) {
  if(is.null(resolve_path(path))){return(NULL)}

  browser()
  fullpath <- resolve_path(path) #get full path if relative

  #if file doesn't exist, return path
  if (!file.exists(fullpath)){return(path)}

  #get parts of the path
  base <- tools::file_path_sans_ext(fullpath)
  dir  <- dirname(fullpath)
  ext  <- tools::file_ext(fullpath)

  #determine how many of the files exist
  existing <- list.files(dir, pattern = paste0("^", basename(base), "( \\([0-9]+\\))?\\.", ext, "$"))

  #if 0, return path
  if(length(existing) == 0){return(path)}

  #if more than 1, return appended path

  path$path <- paste0(tools::file_path_sans_ext(path$path), " (", length(existing), ").", ext)
  return(path)
}
