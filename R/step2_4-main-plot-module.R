#' Visualize main data plot
#'
#' Comes with an adjustable y-axis to adjust the values being viewed.
#'
#' @param id the shiny ID of the module
#' @param plot_data A `reactiveVal` holding the current dataset to plot.
#' @param sondeproj A `reactiveVal` holding the current dataset.
#' @param plot_obj Plotly object to plot.
#' @param y_var A `reactiveVal` holding the Y-variable to plot on the y-axis.
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
main_plot_server <- function(id, sondeproj, plot_obj, plot_data, y_var, sel_mode=FALSE, plot_exist=reactiveVal(),
                             startmin=reactiveVal(), startmax=reactiveVal()){
  moduleServer(id, function(input, output, session){

  #store zoom vals
    xzoom <- reactiveVal()
    yzoom <- reactiveVal()
    interact_method <- reactiveVal()

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

  observeEvent(req(plot_exist(), event_data("plotly_relayout", source = id)),{
    zoom_dat <- event_data("plotly_relayout", source = id)


    if(names(zoom_dat)[1] %in% c("dragmode")){
      interact_method(zoom_dat$dragmode)
    }

    #selecting points counts as a relayout, only trigger if zoom is actually changed
    if(names(zoom_dat)[1] %in% c("xaxis.range[0]","xaxis.autorange")){
      #if performing a zoom reset selection to zoom, otherwise keep
      interact_method(NULL)

      #if cleared, reset values
      if(is.null(zoom_dat) || names(zoom_dat[1]) %in% c("xaxis.autorange")){
        xzoom(NULL)
        yzoom(NULL)
      }

      #otherwise cache axis values
      xzoom(list(range = c(zoom_dat$`xaxis.range[0]`, zoom_dat$`xaxis.range[1]`)))
      yzoom(list(range = c(zoom_dat$`yaxis2.range[0]`, zoom_dat$`yaxis2.range[1]`)))
    }

  })

  #clear zoom when y_var changes
  observeEvent(list(y_var(),input$yaxismax, input$yaxismin),{
      xzoom(NULL)
      yzoom(NULL)
    })

  #export plot to UI
    #save to export
    output$plot <- plotly::renderPlotly({
      validate(
        need(nrow(plot_data()) > 0,
             "No data available for the selected date range."))

      # add things to plot
      p <- plot_obj() %>% plotly::event_register("plotly_relayout")

      #determine which zoom to use
      if(!is.null(xzoom())){
        p <- p %>% layout(xaxis = xzoom())
      }

      if(!is.null(yzoom())){
        p <- p %>% layout(yaxis2 = yzoom())
      }else{
        p <- p %>% layout(yaxis2 = list(range = c(input$yaxismin, input$yaxismax)))

        #check if there's a yaxis name is "raw" if so, also apply layout to yaxis
        build <- plotly_build(p)
        if("Raw Data" %in% unlist(sapply(build$x$data, function(x){x$name}))){
          p <- p %>% layout(yaxis = list(range = c(input$yaxismin, input$yaxismax)))
        }
      }

      if(sel_mode){
        p <- p %>% plotly::event_register("plotly_selected")
      }

      if(!is.null(interact_method())){
        p <- p %>% plotly::layout(dragmode = interact_method())
      }

      plot_exist(TRUE)

      toWebGL(p)
    })

  })

}
