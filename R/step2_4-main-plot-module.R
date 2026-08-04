#' Visualize main data plot
#'
#' Comes with an adjustable y-axis to adjust the values being viewed.
#'
#' @param id the shiny ID of the module
#' @param plot_data A `reactiveVal` holding the current dataset to plot.
#' @param sondeproj A `reactiveVal` holding the current dataset.
#' @param plot_obj Plotly object to plot.
#' @param y_var A `reactiveVal` holding the Y-variable to plot on the y-axis.
#' @param plot_name The plot name to register the plot as for selecting points
#' @param sel_mode Logical, should selection mode be turned on as default?
#' @param plot_exist A `reactiveVal` indicating if the plot exists or not to prevent warnings about plot obj not being registered.
#'
#' @returns a plot of the data.
#' @rdname main-plot
#' @export
#' @keywords internal
#'
#'
main_plot_UI <- function(id){
  ns <- NS(id)
  tagList(
    div(
      style = "position: relative; padding-left: 70px;",

      #add plot
      plotlyOutput(ns("plot"), height = "450px"),

      # Upper y-limit
      div(style = "position:absolute; top:20px; left:0px; width:75px;font-size: 10px;",
          numericInput(ns("yaxismax"),label = "max y-value",value = NA,width = "75px")),

      # Lower y-limit
      div(style = "position:absolute; bottom:35px; left:0px; width:75px;font-size: 10px;",
          numericInput(ns("yaxismin"),label = "min y-value",value = NA,width = "75px")))
  )}

#' @rdname main-plot
#' @export
main_plot_server <- function(id, sondeproj, plot_obj, plot_data, y_var, plot_name=NULL, sel_mode=FALSE, plot_exist=reactiveVal(),
                             startmin=reactiveVal(), startmax=reactiveVal()){
  moduleServer(id, function(input, output, session){

  #when y_var changes update min/max values
    observe({
      req(sondeproj(), y_var())

      maxv <- ceiling(max(sondeproj()$data[[y_var()]], na.rm=TRUE)*1.05)
      minv <- floor(min(sondeproj()$data[[y_var()]], na.rm=TRUE) - (maxv*0.05))

      updateNumericInput(session, "yaxismax", value=max(startmax(), maxv, na.rm=TRUE))
      updateNumericInput(session, "yaxismin", value=min(startmin(), minv, na.rm=TRUE))
    })

  observeEvent(input$yaxismax, {
    req(sondeproj(), y_var())

    minv <- floor(min(sondeproj()$data[[y_var()]], na.rm=TRUE) - (input$yaxismax*0.05))
    updateNumericInput(session, "yaxismin", value=min(startmin(), minv, na.rm=TRUE))
  })

  #export plot to UI
    #save to export
    output$plot <- plotly::renderPlotly({
      validate(
        need(nrow(plot_data()) > 0,
             "No data available for the selected date range."))

      # convert to plotly
      p <- plot_obj() %>% layout(yaxis2 = list(range = c(input$yaxismin, input$yaxismax)))

      if(!is.null(plot_name)){
        p <- p %>% plotly::event_register(plot_name)
      }

      if(sel_mode){
        p <- p %>% plotly::layout(dragmode = "select")
      }

      plot_exist(TRUE)

      toWebGL(p)
    })

  })

}
