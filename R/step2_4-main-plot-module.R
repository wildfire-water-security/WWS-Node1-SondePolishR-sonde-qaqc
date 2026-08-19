#' Visualize main data plot
#'
#' Comes with an adjustable y-axis to adjust the values being viewed.
#'
#' @param id the shiny ID of the module
#' @param data_ver A `reactiveVal` holding a number used to track when new data is added to trigger resets.
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
main_plot_server <- function(id, data_ver, sondeproj, plot_obj, plot_data, y_var, sel_mode=FALSE, plot_exist=reactiveVal(),
                             startmin=reactiveVal(), startmax=reactiveVal()){
  moduleServer(id, function(input, output, session){

  #store zoom vals
    zoom_state <- reactiveVal(list(x=list(range=NULL), y=list(range=NULL), dragmode="zoom"))
    date_range <- reactiveVal(NULL) #track x-axis values
    val_range <- reactiveVal() #track y-axis values

  #check if we need to reset y-axis when data changes (only update if min/max changes)
    observeEvent(sondeproj(),{
      req(sondeproj(), y_var())

      new_vals <- range(sondeproj()$data[[y_var()]], na.rm = TRUE)
      if(!identical(new_vals, val_range())){
        val_range(new_vals)

        #reset if range changes
        maxv <- ceiling(max(sondeproj()$data[[y_var()]], na.rm=TRUE)*1.05)
        minv <- floor(min(sondeproj()$data[[y_var()]], na.rm=TRUE) - (maxv*0.05))

        updateNumericInput(session, "yaxismax", value=max(c(startmax(), maxv), na.rm=TRUE))
        updateNumericInput(session, "yaxismin", value=min(c(startmin(), minv), na.rm=TRUE))
        }

    })

  #check if we want to reset zoom, if no viewable data, reset zoom
    observeEvent(plot_data(), {
      req(plot_data(), zoom_state())
      zoom_data <- plot_data()
      zoom <- zoom_state()

      new_range <- range(zoom_data$DateTime_rd, na.rm = TRUE)
      if(!identical(new_range, date_range())){date_range(new_range)}

      if(!is.null(zoom$x$range)){
        zoom_data <- zoom_data %>% filter(.data$DateTime_rd >= zoom$x$range[1] & .data$DateTime_rd <= zoom$x$range[2])
      }

      if(!is.null(zoom$y$range)){
        zoom_data <- zoom_data %>% filter(.data[[y_var()]] >= zoom$y$range[1] & .data[[y_var()]] <= zoom$y$range[2])
      }

      if(nrow(zoom_data) == 0){
        update_zoom_state(zoom_state, x = list(range=NULL))
        update_zoom_state(zoom_state, y = list(range=NULL))
      }
    })

  #reset axes and back to zoom
    observeEvent(list(data_ver(), y_var(), date_range(), input$yaxismax, input$yaxismin), {
      req(plot_obj(), y_var(),sondeproj())
      update_zoom_state(zoom_state, x = list(range=NULL))
      update_zoom_state(zoom_state, y = list(range=NULL))
    }, ignoreInit = TRUE)

  #don't reset dragmode on differences with date range, only update user limits with changed data/yvar
    observeEvent(list(data_ver(), y_var(), startmax(), startmin()), {
      req(plot_obj(), y_var(), sondeproj(), y_var())
      update_zoom_state(zoom_state, dragmode = "zoom")

      #only reset y-values here (for now, likely want to do something similar to manual zoom??)
      maxv <- ceiling(max(sondeproj()$data[[y_var()]], na.rm=TRUE)*1.05)
      minv <- floor(min(sondeproj()$data[[y_var()]], na.rm=TRUE) - (maxv*0.05))

      updateNumericInput(session, "yaxismax", value=max(c(startmax(), maxv), na.rm=TRUE))
      updateNumericInput(session, "yaxismin", value=min(c(startmin(), minv), na.rm=TRUE))
    }, ignoreInit = TRUE)

  #keep track of correct y-axis limits
    y_axis <- reactive({
    req(sondeproj(), y_var())

    zoom <- zoom_state()
    if(is.null(zoom$y$range)){
      list(range=c(input$yaxismin, input$yaxismax))
    }else{zoom$y}
  })

    #adjust y min when y max changes
    observeEvent(input$yaxismax, {
      req(sondeproj(), y_var())

      minv <- floor(min(sondeproj()$data[[y_var()]], na.rm=TRUE) - (input$yaxismax*0.05))
      updateNumericInput(session, "yaxismin", value=min(startmin(), minv, na.rm=TRUE))
    })

 #observe changes to plot
  observeEvent(req(plot_exist(), event_data("plotly_relayout", source = id)),{
    zoom_dat <- event_data("plotly_relayout", source = id)

    if(names(zoom_dat)[1] %in% c("dragmode")){
      update_zoom_state(zoom_state, dragmode = zoom_dat$dragmode)
    }

    #selecting points counts as a relayout, only trigger if zoom is actually changed
    if(names(zoom_dat)[1] %in% c("xaxis.range[0]","xaxis.autorange")){
      #if performing a zoom reset selection to zoom, otherwise keep
      if(zoom_state()$dragmode != "pan"){
        update_zoom_state(zoom_state, dragmode = "zoom")
      }

      #if cleared, reset values
      if(is.null(zoom_dat) || names(zoom_dat[1]) %in% c("xaxis.autorange")){
        update_zoom_state(zoom_state, x = list(range=NULL))
        update_zoom_state(zoom_state, y = list(range=NULL))
      }

      #otherwise cache axis values
      update_zoom_state(zoom_state, x = list(range=c(zoom_dat$`xaxis.range[0]`, zoom_dat$`xaxis.range[1]`)))
      update_zoom_state(zoom_state, y = list(range=c(zoom_dat$`yaxis2.range[0]`, zoom_dat$`yaxis2.range[1]`)))

    }

  })

  #export plot to UI
    #save to export
    output$plot <- plotly::renderPlotly({
      req(sondeproj())
      validate(
        need(nrow(plot_data()) > 0,
             "No data available for the selected date range."))

      zoom <- zoom_state()

      # add things to plot
      p <- plot_obj() %>% plotly::event_register("plotly_relayout") %>%
        plotly::layout(dragmode = zoom$dragmode)

      #apply zoom
      if(!is.null(zoom$x)){
        p <- p %>% layout(xaxis = zoom$x)
      }

      p <- p %>% layout(yaxis2 = y_axis())

      #if raw data, put on y-axis
      build <- plotly_build(p)
      if(build$x$data[[1]]$name == "Raw Data"){
        p <- p %>% layout(yaxis = y_axis())
      }

      if(sel_mode){
        p <- p %>% plotly::event_register("plotly_selected")
      }

      plot_exist(TRUE)

      toWebGL(p)
    })

  })

}
