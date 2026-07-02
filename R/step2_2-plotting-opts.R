#module to add plotting options to UI and return list of T/F to use when deciding on plot

#' Specify Plotting Options
#'
#' Used to create and return selections about what to include in the plot.
#'
#' @param id the shiny ID of the module
#'
#' @returns a list of length 5:
#' - points: should points be plotted?
#' - line: should line be plotted?
#' -files: should points be colored by file?
#' -oow: should out of water periods be plotted?
#' -calcheck: should cal check be plotted?
#' @rdname plot-options
#' @export
#' @md
#' @keywords internal
#'
#'
plot_options_UI <- function(id){
  ns <- NS(id)
  tagList(
    tags$h5("Plot Options"),
    tags$div(
      style = "margin-bottom:-5px;",
    fluidRow(
      column(6,
             div(style = "margin-bottom:-10px;",
             checkboxInput( ns("points"), "Plot points",value = TRUE)),

             div(style = "margin-bottom:-10px;",
                 checkboxInput( ns("line"), "Plot line",value = TRUE)),

             div(style = "margin-bottom:-10px;",
                 checkboxInput( ns("files"),"Color points by file",value = FALSE))),
      column(6,
             div(style = "margin-bottom:-10px;",
                 checkboxInput(ns("oow"),"Show out-of-water periods",value = FALSE)),

             div(style = "margin-bottom:-10px;",
                 checkboxInput(ns("calcheck"),"Show calibration checks",value = FALSE))),
)

)
  )}


#' @rdname plot-options
#' @export
plot_options_server <- function(id){
  moduleServer(id, function(input, output, session){

    reactive({

      list(
        points = input$points,
        line = input$line,
        files = input$files,
        oow = input$oow,
        calcheck = input$calcheck
      )

    })


  })

}
