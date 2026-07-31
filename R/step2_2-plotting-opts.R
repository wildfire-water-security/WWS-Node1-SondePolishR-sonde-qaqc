#module to add plotting options to UI and return list of T/F to use when deciding on plot

#' Specify Plotting Options
#'
#' Used to create and return selections about what to include in the plot.
#'
#' @param id the shiny ID of the module
#' @param start_val a vector the same length as the list with the initial values to use for the plotting options.
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
plot_options_UI <- function(id, start_val = c(TRUE,TRUE,FALSE,FALSE,FALSE,FALSE)){
  ns <- NS(id)
  tagList(
    tags$h5("Plot Options"),
    tags$div(
      style = "margin-bottom:-5px;",
    fluidRow(
      column(6,
             div(style = "margin-bottom:-10px;",
             checkboxInput( ns("points"), "Plot points",value = start_val[1])),

             div(style = "margin-bottom:-10px;",
                 checkboxInput( ns("line"), "Plot line",value = start_val[2])),

             div(style = "margin-bottom:-10px;",
                 checkboxInput( ns("files"),"Color points by file",value = start_val[3]))),
      column(6,
             div(style = "margin-bottom:-10px;",
                 checkboxInput(ns("oow"),"Show out-of-water periods",value = start_val[4])),

             div(style = "margin-bottom:-10px;",
                 checkboxInput(ns("calcheck"),"Show calibration checks",value = start_val[5])),

             div(style = "margin-bottom:-10px;",
                 checkboxInput(ns("qualflag"),"Show questionable points",value = start_val[6]))),
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
        calcheck = input$calcheck,
        qualflag = input$qualflag
      )

    })


  })

}
