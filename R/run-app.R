#' Run the SondePolishR App
#'
#' Opens a window with the interactive Shiny App to
#' interactively load, view, correct, and export sonde data.
#'
#' @returns Shiny App
#' @param default_path Default filepath used for saving data to, defaults to current working directory.
#' @export
#' @examples
#' \dontrun{
#' if(interactive()){
#'   library(SondePolishR)
#'   run_app()
#' }}
run_app <- function(default_path = getwd()){
  appDir <- system.file("app", package = "SondePolishR")
  options(SondePolishR.default_path = default_path)
  on.exit(options(SondePolishR.default_path = NULL), add = TRUE)

  shiny::runApp(appDir, display.mode = "normal")

  }
