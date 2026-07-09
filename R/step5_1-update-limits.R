

#' Update numeric input with limits from sonde data
#'
#' @param data data.frame with sonde data
#' @param y_var parameter to plot
#' @param session the environment that can be used to access information and functionality relating to the session
#' @noRd
update_limits <- function(data, y_var, session){
  observeEvent(list(y_var(), data()), {
    req(data())               # make sure data is available
    req(y_var())        # make sure the input exists
    req(y_var() %in% names(data()))  # make sure column exists

    updateNumericInput(
      session,
      "min",
      value = min(data()[[y_var()]], na.rm=TRUE))

    updateNumericInput(
      session,
      "max",
      value = max(data()[[y_var()]], na.rm=TRUE))
  })
}
