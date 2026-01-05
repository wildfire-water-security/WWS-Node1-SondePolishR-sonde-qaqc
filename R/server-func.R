

#' Update numeric input with limits from sonde data
#'
#' @param df data.frame with sonde data
#' @param y_var parameter to plot
#' @param session the environment that can be used to access information and functionality relating to the session
#' @export
update_limits <- function(df, y_var, session){
  observeEvent(y_var(), {
    req(df())               # make sure df is available
    req(y_var())        # make sure the input exists
    req(y_var() %in% names(df()))  # make sure column exists

    updateNumericInput(
      session,
      "min",
      value = min(df()[[y_var()]], na.rm=TRUE))

    updateNumericInput(
      session,
      "max",
      value = max(df()[[y_var()]], na.rm=TRUE))
  })
}


#' Store user zoom info from plotly to maintain zoom while updating data
#'
#' Natively, when plotly redraws, it will remove any user interactions like zooming. This function
#' will capture the zoom level and apply it when redrawing.
#'
#' @param df the data.frame with the sonde data
#' @param y_var the y_var
#' @param plot_id the plot_id of the plot to keep zoom steady
#'
#' @returns a list of length two:
#' -  xaxis$range with the start and end values for the x-axis range
#' -  yaxis$range with the start and end values for the y-axis range
#' @export
#' @md
preserve_zoom <- function(df, y_var, plot_id) {
  d <- reactiveVal()
  plot_lyout <- reactiveValues(xaxis = list(), yaxis = list())

  observeEvent(event_data("plotly_relayout", source = "shift_plot"), {

    relayout <- tryCatch(
      event_data("plotly_relayout", source = "shift_plot"),
      error = function(e) NULL
    )

    if (is.null(relayout)) return()

    # X axis
    if (!is.null(relayout$`xaxis.autorange`)) {
      plot_lyout$xaxis <- list()
    }
    if (!is.null(relayout$`xaxis.range[0]`)) {
      plot_lyout$xaxis <- list(
        range = c(relayout$`xaxis.range[0]`, relayout$`xaxis.range[1]`)
      )
    }

    # Y axis
    if (!is.null(relayout$`yaxis.autorange`)) {
      plot_lyout$yaxis <- list()
    }
    if (!is.null(relayout$`yaxis.range[0]`)) {
      plot_lyout$yaxis <- list(
        range = c(relayout$`yaxis.range[0]`, relayout$`yaxis.range[1]`)
      )
    }

  })

  # reset zoom whenever y_var changes, [not triggering]
  observeEvent(y_var(), {
    plot_lyout$xaxis <- list()
    plot_lyout$yaxis <- list()
  })

  return(plot_lyout)
}
