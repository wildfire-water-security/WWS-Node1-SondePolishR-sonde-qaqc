#' Run the SondePolishR App
#'
#' Opens a window with the interactive Shiny App to
#' interactively load, view, correct, and export sonde data.
#'
#' @returns Shiny App
#' @export
#' @examples
#' \dontrun{
#' if(interactive()){
#'   library(SondePolishR)
#'   run_app()
#' }}
run_app <- function(){
  appDir <- system.file("app", package = "SondePolishR")

  shiny::runApp(appDir, display.mode = "normal")
}
