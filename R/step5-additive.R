#' @export
#' @rdname additive
additive_UI <- function(id){
  ns <- NS(id) #line to make module work

  tagList(

    sidebarLayout(
      sidebarPanel(
        update_parms_UI(ns("update_parms")),

        #select physical limits
        tags$h5("Shift Values"),
        fluidRow(
          div(style="margin-bottom: 8px; font-size:14px",
              "Adjust the slope and intercept to shift the selected data:"),
          numericInput(ns("slope"),"Slope",value = 0,step=0.001),
          numericInput(ns("int"),"Intercept",value = 0,step=0.01)),

        HTML("<hr>"),

        apply_edit_UI(ns("apply_limits"), note=""),
        HTML("<hr>"),

        #date options
        weekly_range_sidebar_UI(ns("date_nav")),

        HTML("<hr>"),

        #plotting options
        plot_options_UI(ns("plot_opts"))

        ),

    mainPanel(
      plotlyOutput(ns("shift_plot"), height="60%"),
      #add buttons to navigate date
      weekly_range_buttons_UI(ns("date_nav")),
    ))

  )}

#' Address any additive shifts
#'
#' Plots loaded dataset, user can select a group of points and apply a additive shift to the data to correct for shifts.
#'
#' @param id An ID string passed to shiny::NS(), used for namespacing UI inputs/outputs.
#' @param sondeproj A `reactiveVal` holding the current dataset.
#' @param data_ver A `reactiveVal` holding a number used to track when new data is added to trigger resets.
#' @param y_var Y-variable to plot on the y-axis.
#' @md
#' @keywords internal
#' @export
#' @rdname additive
#' @returns Invisible NULL
#'
additive_server <- function(id, sondeproj, data_ver, y_var){
  moduleServer(id, function(input, output, session){

  index <- reactiveVal() #stores index of selected points

  #get column names after file upload (dynamic)
    update_parms_server("update_parms", sondeproj, data_ver, y_var, choices_fun = nice_yvar)

  #get what to plot via user options
    plot_opts <- plot_options_server("plot_opts")

  #keep track of dates
    dates <- weekly_range_server(
      "date_nav",
      min_date = reactive({req(sondeproj())
        min(sondeproj()$data$Date, na.rm = TRUE)}),
      max_date = reactive({req(sondeproj())
        max(sondeproj()$data$Date, na.rm = TRUE)}))


  #filter data to plot
    plot_data <- reactive({
      req(sondeproj(), dates())
      dat <- sondeproj()$data %>% dplyr::filter(.data$Date >= dates()[1], .data$Date <= dates()[2])

      #if selected points, update where they're plotted
      if(!is.null(index())){
        rows <- which(dat$Index %in% index())
        dat <- shift_points(dat, y_var(), rows, shift_val = list(slope=input$slope, int=input$int))
      }

      dat
    })

  #create plotly plot
    plot_obj <- reactive({
      req(y_var(), plot_data())

     #use function to plot sonde data
      p <- plot_sonde(plot_data(), y_var(), plot_opts(),sondeproj()$fieldform, sondeproj()$calcheck)

     #if points are selected color those
      if(!is.null(index())){
        flag_data <- plot_data()[plot_data()$Index %in% index(),]
        p <- p + ggplot2::geom_point(data=flag_data, aes(x = .data$DateTime_rd,y = .data[[y_var()]]), color = "darkred")

      }

      #return plot
      p
    })

    #save to export
    output$shift_plot <- plotly::renderPlotly({
        req(plot_obj())

      # convert to plotly
      plot_obj() %>%
                plotly::ggplotly(source = "shift_plot") %>%
               plotly::layout(dragmode = "select") %>%
                plotly::event_register("plotly_selected")
    })

  #observe selection from plot and get indices of selected
      observeEvent(
        req(sondeproj(), event_data("plotly_selected", source = "shift_plot")),{
           req(sondeproj(), y_var())

           sel <- event_data("plotly_selected", source = "shift_plot")

           if(!is.null(sel) && length(sel) && nrow(sel) > 0) {
             full_index <- plot_data()$Index[sel$pointNumber + 1]
             index(full_index)
           }else {index(NULL)}

        #update guesses
           guess <- guess_shift(sondeproj()$data, y_var(), index())
           updateNumericInput(session,"slope", value = guess$slope)
           updateNumericInput(session,"int", value = guess$int)


         })

    #create edit object
    edit <- reactive({
      newdata <- sondeproj()$data

    #get updated data
      newdata <- shift_points(newdata, y_var(), index(), shift_val = list(slope=input$slope, int=input$int))
      rows <- newdata$Index %in% index() #convert from row numbers to T/F

    #make edit list
      list(
        data = newdata,
        rows = rows,
        y_var = y_var(),
        step = "additive shifts",
        note = paste0("shift with slope ", input$slope," and intercept ", input$int),
        flag = "CHG01",
        changetype = "flag_chg"
      )

    })

  #flagging module
    apply_edit_server("apply_limits", sondeproj, edit)

  #export plot so we can check it
    exportTestValues(
      plot_obj = plot_obj(),
      changelog = sondeproj()$changelog)



   #  #initialize
   #    index <- reactiveVal() #stores index of selected points
   #
   #  #update choices based on data table
   #    y_var <- SondePolishR::update_parms_server("update_parms", sdata, choices_fun = nice_yvar)
   #
   #  #reset when dataset or yvar changes
   #    observeEvent(list(sdata(), y_var()), {
   #      index(NULL)
   #
   #      updateNumericInput(session,"slope_val", value = 0)
   #      updateNumericInput(session,"int_val", value = 0)
   #    })
   #
   #  #observe selection from plot and get indices of selected
   #    observeEvent(
   #      event_data("plotly_selected", source = "shift_plot"),{
   #        req(sdata(), y_var())
   #
   #        sel <- event_data("plotly_selected", source = "shift_plot")
   #
   #        if(!is.null(sel) && length(sel) && nrow(sel) > 0) {
   #          index(sel$pointNumber + 1)
   #        }else {index(NULL)}
   #      })
   #
   #  #guess slope/intercept when points selected
   #    observeEvent(index(), {
   #        if(is.null(index())){
   #          new_slope <- 0
   #          new_int <- 0
   #        }else {
   #          shift <- guess_shift(sdata(), y_var(), index())
   #
   #          new_slope <- shift$slope
   #          new_int <- shift$int
   #        }
   #
   #        updateNumericInput(session,"slope_val", value = new_slope)
   #        updateNumericInput(session,"int_val", value = new_int)
   #      })
   #
   #
   #  #preview data with shift
   #    data_plot <- reactive({
   #          req(sdata(), y_var())
   #
   #          dat <- sdata()
   #          dat$sel <- FALSE
   #
   #          if(!is.null(index())){
   #            colnum <- which(names(dat) == y_var())
   #
   #            add <- (input$slope_val * (seq_along(index())-1)) + input$int_val
   #
   #            dat[index(), colnum] <- dat[index(), colnum] + add
   #            dat$sel <- dat$Index %in% index()
   #          }
   #
   #          dat
   #        })
   #
   #  # preserve zoom
   #   plot_lyout <- preserve_zoom(data_plot, y_var, "shift_plot")
   #
   # #plot
   #  #make plot
   #    plot_obj <- reactive({
   #      req(data_plot(), y_var())
   #
   #      dat <- data_plot()
   #
   #      #if no points selected just plot normally
   #      if(is.null(index())){
   #        p <- ggplot2::ggplot(dat, ggplot2::aes(x = .data$DateTime, y = .data[[y_var()]])) + ggplot2::geom_point(na.rm=TRUE)
   #      }else {
   #
   #        p <- ggplot2::ggplot(dat, ggplot2::aes(.data$DateTime, .data[[y_var()]], color = sel)) +
   #          ggplot2::geom_point(na.rm=TRUE) +
   #          ggplot2::scale_color_manual(values = c("white","red"), guide = "none")
   #      }
   #
   #      return(p)
   #
   #    })
   #
   #    #return plot
   #    output$shift_plot <- renderPlotly({
   #      p <- plot_obj() %>%
   #        plotly::ggplotly(source = "shift_plot") %>%
   #        plotly::layout(dragmode = "select") %>%
   #        plotly::event_register("plotly_selected") %>%
   #        plotly::event_register("plotly_relayout")
   #
   #      if(length(plot_lyout$xaxis) > 0 || length(plot_lyout$yaxis) > 0){
   #        p <- layout(p, xaxis = plot_lyout$xaxis, yaxis = plot_lyout$yaxis)
   #      }
   #
   #      p
   #
   #      })
   #
   #  #confirm changes
   #  SondePolishR::confirm_changes_server(
   #    id = "flag2",
   #    newdata = data_plot,
   #    sdata = sdata,
   #    index = index,
   #    par = y_var,
   #    flag_name = "Additive Shift",
   #    note = reactive(paste0("shift with slope ", input$slope_val," and intercept ", input$int_val)),
   #    prj_path = prj_path,
   #    log = log)
   #
   #
   #  #export plot and table so we can check it
   #  exportTestValues(
   #    plot_obj = plot_obj())

   })

}

