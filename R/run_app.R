#' Run the SondePolishR App
#'
#' @returns Shiny App
#' @export
#'
run_sondepolishR <- function(){
  appDir <- system.file("app", package = "SondePolishR")

  shiny::runApp(appDir, display.mode = "normal")
}
